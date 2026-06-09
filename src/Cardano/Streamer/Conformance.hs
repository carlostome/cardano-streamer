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
import Cardano.Streamer.Common
import Cardano.Streamer.Inspection
import Cardano.Streamer.Ledger
import Cardano.Streamer.LedgerState
import Control.State.Transition.Extended (ValidationPolicy (ValidateNone))
import Ouroboros.Consensus.Cardano.Block hiding (TxId)
import Ouroboros.Consensus.Config (TopLevelConfig (..))

type Stats = ()

doConformanceTesting ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  Stats ->
  SlotWithBlock ->
  Stats
doConformanceTesting cnf _ swb =
  applyTickedNewEpochStateWithTxs
    (\_ _ -> ())
    doStuff
    (swbTickExtLedgerState swb)
    (biBlockComponent (swbBlockWithInfo swb))
  where
    doStuff ::
      forall era.
      EraApp era =>
      Shelley.NewEpochState era ->
      [Tx TopTx era] ->
      Stats
    doStuff nes txs = const () (foldl' go st txs)
      where
        env :: Shelley.MempoolEnv era
        env = Shelley.mkMempoolEnv nes (biSlotNo (swbBlockWithInfo swb))

        globals :: Shelley.Globals
        globals = let Just x = swbGlobals swb cnf in x

        st :: Shelley.MempoolState era
        st = Shelley.mkMempoolState nes

        go :: Shelley.MempoolState era -> Tx TopTx era -> Shelley.MempoolState era
        go st tx =
          fst $
            fromRight
              (error "Validation error")
              (Shelley.applyTxValidation @era ValidateNone globals env st tx)
