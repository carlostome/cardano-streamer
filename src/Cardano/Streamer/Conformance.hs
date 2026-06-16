{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Streamer.Conformance (
  doConformanceTesting,
) where

import Cardano.Ledger.Api.Tx
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Streamer.BlockInfo
import Cardano.Streamer.Inspection
import Cardano.Streamer.Ledger
import Cardano.Streamer.LedgerState
import Control.State.Transition.Extended (ValidationPolicy (..))
import Ouroboros.Consensus.Cardano.Block hiding (TxId)
import Ouroboros.Consensus.Config (TopLevelConfig (..))

doConformanceTesting ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  SlotWithBlock ->
  [String]
doConformanceTesting cnf swb =
  applyTickedNewEpochStateWithTxs
    (\_ _ -> ["Success"])
    doStuff
    (swbTickExtLedgerState swb)
    (biBlockComponent (swbBlockWithInfo swb))
  where
    doStuff ::
      forall era.
      EraApp era =>
      Shelley.NewEpochState era ->
      [Tx TopTx era] ->
      [String]
    doStuff nes txs = go st txs
      where
        env :: Shelley.MempoolEnv era
        env = Shelley.mkMempoolEnv nes (biSlotNo (swbBlockWithInfo swb))

        globals :: Shelley.Globals
        globals = let Just x = swbGlobals swb cnf in x

        st :: Shelley.MempoolState era
        st = Shelley.mkMempoolState nes

        go :: Shelley.MempoolState era -> [Tx TopTx era] -> [String]
        go _ [] = ["Success"]
        go st (tx : txs) =
          let st' = Shelley.applyTxValidation @era ValidateAll globals env st tx
           in case st' of
                Left err -> [show err]
                Right (st', _) -> "Success" : go st' txs
