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
import Cardano.Streamer.BlockInfo
import Cardano.Streamer.Common
import Cardano.Streamer.Inspection
import Cardano.Streamer.LedgerState
import Conduit as C
import Control.State.Transition
import qualified Data.Text as Text
import Ouroboros.Consensus.Cardano.Block hiding (TxId)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..), pInfoConfig)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Ledger (tickedLedgerState, tickedShelleyLedgerState)
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayLedgerExecContext (..))
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))

doConformance :: ConduitT SlotWithBlock Void (RIO App) ()
doConformance = do
  toNewEpochStateWithTxsConway
    .| applyTxValidationConformance

applyTxValidationConformance ::
  ConduitT
    ( (Shelley.Globals, Shelley.MempoolEnv ConwayEra)
    , (Shelley.MempoolState ConwayEra, Tx TopTx ConwayEra, Shelley.MempoolState ConwayEra)
    )
    Void
    (RIO App)
    ()
applyTxValidationConformance =
  awaitForever $ \((globals, env), (st, tx, st')) ->
    do
      logInfo $ "TxId: " <> display (txIdTx tx)
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
      let result = do
            specTRC <-
              first (" [translateInputs]: " <>) $
                translateInputs @"LEDGER" @ConwayEra ctx trc
            implResponse <-
              first (" [translateOutput]: " <>) $
                translateOutput @"LEDGER" @ConwayEra ctx trc' st'
            agdaResponse <-
              first (" [runAgdaRule]: " <>) $
                specNormalize
                  <$> runAgdaRule @"LEDGER" @ConwayEra specTRC
            if implResponse == agdaResponse
              then return ()
              else Left " : implResponse != agdaResponse"
      logInfo $ display $ either ("Error" <>) (const "Success") result

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
    awaitForever
      ( \swb ->
          case (tickedLedgerState $ swbTickExtLedgerState swb, biBlockComponent (swbBlockWithInfo swb)) of
            (TickedLedgerStateConway _ ls, BlockConway conwayBlock) -> do
              let Just globals = swbGlobals swb cnf
                  nes = tickedShelleyLedgerState ls
                  env = Shelley.mkMempoolEnv nes (biSlotNo (swbBlockWithInfo swb))
                  st = Shelley.mkMempoolState nes
                  txs = toList . view txSeqBlockBodyL . Shelley.blockBody . shelleyBlockRaw $ conwayBlock
                  curProtVer =
                    Shelley.mkMempoolState nes
                      ^. lsUTxOStateL
                      . utxosGovStateL
                      . curPParamsGovStateL
                      . ppProtocolVersionL
              when (curProtVer >= ProtVer (natVersion @10) 0) $
                unfoldC
                  ( \(st, txs) ->
                      case txs of
                        [] -> Nothing
                        (tx : txs) ->
                          let st' =
                                either
                                  (error . show)
                                  fst
                                  (Shelley.applyTxValidation ValidateAll globals env st tx)
                           in Just ((st, tx, st'), (st', txs))
                  )
                  (st, txs)
                  .| mapC ((globals, env),)
            _ -> return ()
      )
