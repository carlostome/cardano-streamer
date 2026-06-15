{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Streamer.Conformance where

import Cardano.Ledger.Api.Tx
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ledgerSlotNoL)
import Cardano.Ledger.TxIn (TxId)
import Cardano.Streamer.BlockInfo
import Cardano.Streamer.Common
import Cardano.Streamer.Inspection
import Cardano.Streamer.Ledger
import Cardano.Streamer.LedgerState
import Conduit as C
import Control.State.Transition
import Control.State.Transition.Extended (ValidationPolicy (..))
import qualified Data.Conduit.List as C
import Data.List
import Ouroboros.Consensus.Cardano.Block hiding (TxId)
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, Ticked, tickedLedgerState)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..), pInfoConfig)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Ledger (tickedShelleyLedgerState)
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayLedgerExecContext (..))
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))

type Stat = [Either Text ()]

doConformance :: ConduitT SlotWithBlock Void (RIO App) ()
doConformance = do
  toNewEpochStateWithTxsConway
    .| mapC (\((globals, env), (st, tx, st')) -> applyTxValidationConformance globals env st tx st')
    .| printC

applyTxValidationConformance ::
  Shelley.Globals ->
  Shelley.LedgerEnv ConwayEra ->
  Shelley.LedgerState ConwayEra ->
  Tx TopTx ConwayEra ->
  Shelley.MempoolState ConwayEra ->
  Either Text ()
applyTxValidationConformance _ env st tx st' =
  do
    let ctx =
          ConwayLedgerExecContext
            { clecGuardrailsScriptHash =
                st ^. lsUTxOStateL . utxosGovStateL . constitutionGovStateL . constitutionGuardrailsScriptHashL
            , clecEnactState = mkEnactState $ st ^. lsUTxOStateL . utxosGovStateL
            , clecUtxoExecContext =
                UtxoExecContext
                  { uecTx = tx
                  , uecUTxO = st ^. utxoL
                  , uecUtxoEnv =
                      UtxoEnv
                        { ueSlot = env ^. ledgerSlotNoL
                        , uePParams = st ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
                        , ueCertState = st ^. lsCertStateL
                        }
                  }
            }
        trc = TRC (env, st, tx)
        trc' = TRC (env, st', tx)
    specTRC <-
      translateInputs @"LEDGER" @ConwayEra ctx trc
    implResponse <-
      translateOutput @"LEDGER" @ConwayEra ctx trc' st'
    agdaResponse <-
      specNormalize
        <$> runAgdaRule @"LEDGER" @ConwayEra specTRC
    if implResponse == agdaResponse
      then return ()
      else Left "Error: implResponse == agdaResponse"

toNewEpochStateWithTxsConway ::
  ConduitT
    SlotWithBlock
    ( (Shelley.Globals, Shelley.MempoolEnv ConwayEra)
    , (Shelley.MempoolState ConwayEra, Tx TopTx ConwayEra, Shelley.MempoolState ConwayEra)
    )
    (RIO App)
    ()
toNewEpochStateWithTxsConway =
  do
    cnf <- pInfoConfig . dsAppProtocolInfo <$> ask
    C.mapMaybe
      ( \swb ->
          case (tickedLedgerState $ swbTickExtLedgerState swb, biBlockComponent (swbBlockWithInfo swb)) of
            (TickedLedgerStateConway _ ls, BlockConway conwayBlock) -> do
              globals <- swbGlobals swb cnf
              let nes = tickedShelleyLedgerState ls
                  env = Shelley.mkMempoolEnv nes (biSlotNo (swbBlockWithInfo swb))
              if ( Shelley.mkMempoolState nes
                     ^. lsUTxOStateL
                     . utxosGovStateL
                     . curPParamsGovStateL
                     . ppProtocolVersionL
                 )
                >= ProtVer (natVersion @10) 0
                then
                  Just
                    ( biSlotNo (swbBlockWithInfo swb)
                    , globals
                    , nes
                    , toList . view txSeqBlockBodyL . Shelley.blockBody . shelleyBlockRaw $ conwayBlock
                    )
                else Nothing
            _ -> Nothing
      )
    .| awaitForever
      ( \(slotNo, globals, nes, txs) ->
          let env :: Shelley.MempoolEnv ConwayEra
              env = Shelley.mkMempoolEnv nes slotNo
           in unfoldC
                ( \(st, txs) ->
                    case txs of
                      [] -> Nothing
                      (tx : txs) ->
                        let st' =
                              fst $
                                fromRight
                                  (error "Validation error")
                                  (Shelley.applyTxValidation ValidateNone globals env st tx)
                         in Just ((st, tx, st'), (st', txs))
                )
                (Shelley.mkMempoolState nes, txs)
                .| mapC ((globals, env),)
      )
