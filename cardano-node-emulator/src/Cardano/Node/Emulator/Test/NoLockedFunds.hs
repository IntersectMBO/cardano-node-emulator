{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Node.Emulator.Test.NoLockedFunds (
  NoLockedFundsProof (..),
  defaultNLFP,
  checkNoLockedFundsProof,
  checkNoLockedFundsProofWithOptions,
  checkNoLockedFundsProofFast,
  checkNoLockedFundsProofFastWithOptions,
  NoLockedFundsProofLight (..),
  checkNoLockedFundsProofLight,
) where

import Cardano.Node.Emulator.API (EmulatorM)
import Cardano.Node.Emulator.Test (
  Options,
  defaultOptions,
  prettyAddr,
  propRunActions,
  propRunActionsWithOptions,
 )
import Control.Lens ((^.))
import Data.Map qualified as Map
import Ledger (CardanoAddress)
import Test.QuickCheck (Property, Testable (property), counterexample)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.ContractModel (
  Actions,
  DL,
  ModelState,
  RunModel,
  SymValue,
  balanceChange,
  balanceChanges,
  lockedValue,
  stateAfter,
  symLeq,
  pattern Actions,
 )
import Test.QuickCheck.ContractModel.DL qualified as DL
import Test.QuickCheck.ContractModel.Internal.Model (annotatedStateAfter)

{- | A "proof" that you can always recover the funds locked by a contract. The first component is
  a strategy that from any state of the contract can get all the funds out. The second component
  is a strategy for each wallet that from the same state, shows how that wallet can recover the
  same (or bigger) amount as using the first strategy, without relying on any actions being taken
  by the other wallets.

  For instance, in a two player game where each player bets some amount of funds and the winner
  gets the pot, there needs to be a mechanism for the players to recover their bid if the other
  player simply walks away (perhaps after realising the game is lost). If not, it won't be
  possible to construct a `NoLockedFundsProof` that works in a state where both players need to
  move before any funds can be collected.
-}
data NoLockedFundsProof model = NoLockedFundsProof
  { nlfpMainStrategy :: DL model ()
  -- ^ Strategy to recover all funds from the contract in any reachable state.
  , nlfpWalletStrategy :: CardanoAddress -> DL model ()
  -- ^ A strategy for each wallet to recover as much (or more) funds as the main strategy would
  --   give them in a given state, without the assistance of any other wallet.
  , nlfpOverhead :: ModelState model -> SymValue
  -- ^ An initial amount of overhead value that may be lost - e.g. setup fees for scripts that
  -- can't be recovered.
  , nlfpErrorMargin :: ModelState model -> SymValue
  -- ^ The total amount of margin for error in the value collected by the WalletStrategy compared
  -- to the MainStrategy. This is useful if your contract contains rounding code that makes the order
  -- of operations have a small but predictable effect on the value collected by different wallets.
  }

newtype NoLockedFundsProofLight model = NoLockedFundsProofLight
  {nlfplMainStrategy :: DL model ()}

-- | The default skeleton of a NoLockedFundsProof - doesn't permit any overhead or error margin.
defaultNLFP :: NoLockedFundsProof model
defaultNLFP =
  NoLockedFundsProof
    { nlfpMainStrategy = return ()
    , nlfpWalletStrategy = const (return ())
    , nlfpOverhead = const mempty
    , nlfpErrorMargin = const mempty
    }

{- | Check a `NoLockedFundsProof`. Each test will generate an arbitrary sequence of actions
  (`anyActions_`) and ask the `nlfpMainStrategy` to recover all funds locked by the contract
  after performing those actions. This results in some distribution of the contract funds to the
  wallets, and the test then asks each `nlfpWalletStrategy` to show how to recover their
  allotment of funds without any assistance from the other wallets (assuming the main strategy
  did not execute). When executing wallet strategies, the off-chain instances for other wallets
  are killed and their private keys are deleted from the emulator state.
-}
checkNoLockedFundsProof
  :: (RunModel model EmulatorM)
  => NoLockedFundsProof model
  -> Property
checkNoLockedFundsProof = checkNoLockedFundsProofWithOptions defaultOptions

checkNoLockedFundsProofFast
  :: (RunModel model EmulatorM)
  => NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofFast = checkNoLockedFundsProofFastWithOptions defaultOptions

checkNoLockedFundsProofWithOptions
  :: (RunModel model EmulatorM)
  => Options model
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofWithOptions =
  checkNoLockedFundsProof' . propRunActionsWithOptions

checkNoLockedFundsProofFastWithOptions
  :: (RunModel model EmulatorM)
  => Options model
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofFastWithOptions _ = checkNoLockedFundsProof' (const $ property True)

checkNoLockedFundsProof'
  :: (RunModel model EmulatorM)
  => (Actions model -> Property)
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProof'
  run
  NoLockedFundsProof
    { nlfpMainStrategy = mainStrat
    , nlfpWalletStrategy = walletStrat
    , nlfpOverhead = overhead
    , nlfpErrorMargin = wiggle
    } =
    DL.forAllDL DL.anyActions_ $ \(Actions as) ->
      let ans0 = (annotatedStateAfter $ Actions as)
       in DL.forAllUniqueDL ans0 mainStrat $ \(Actions as') ->
            let s0 = (stateAfter $ Actions as)
                s = stateAfter $ Actions (as ++ as')
             in foldl
                  (QC..&&.)
                  ( counterexample "Main run prop" (run (Actions $ as ++ as'))
                      QC..&&. (counterexample "Main strategy" . counterexample (show . Actions $ as ++ as') $ prop s0 s)
                  )
                  [ walletProp ans0 s0 as addr bal
                  | (addr, bal) <- Map.toList (s ^. balanceChanges)
                  , not $ bal `symLeq` (s0 ^. balanceChange addr)
                  ]
    where
      -- if the main strategy leaves w with <= the starting value, then doing nothing is a good wallet strategy.

      prop s0 s =
        -- TODO: check that nothing is locked by scripts
        let lockedVal = lockedValue s
         in ( counterexample
                ("Locked funds should be at most " ++ show (overhead s0) ++ ", but they are\n  " ++ show lockedVal)
                $ symLeq lockedVal (overhead s0)
            )

      walletProp ans0 s0 as addr bal =
        -- TODO: It's not clear what this Unilateral action is supposed to do
        -- Escrow doesn't need it, so let's fix this when we have a testcase that does need it
        DL.forAllUniqueDL ans0 ({-DL.action (ContractAction False $ Unilateral w) >> -} walletStrat addr) $ \acts ->
          let Actions as' = acts
              wig = wiggle s0
              err =
                "Unilateral strategy for "
                  ++ walletName
                  ++ " should have gotten it at least\n"
                  ++ "  "
                  ++ show bal
                  ++ "\n"
                  ++ concat
                    [ "with wiggle room\n" ++ "  " ++ show wig ++ "\n"
                    | wig /= mempty
                    ]
                  ++ "but it got\n"
                  ++ "  "
                  ++ show bal'
              bal' = stateAfter smacts ^. balanceChange addr
              smacts = Actions $ as ++ as'
              err' =
                "The ContractModel's Unilateral behaviour for "
                  ++ walletName
                  ++ " does not match the actual behaviour for actions:\n"
                  ++ show smacts
              walletName = prettyAddr addr
           in counterexample err (symLeq bal (bal' <> wig))
                QC..&&. counterexample err' (run smacts)

checkNoLockedFundsProofLight
  :: (RunModel model EmulatorM)
  => NoLockedFundsProofLight model
  -> Property
checkNoLockedFundsProofLight NoLockedFundsProofLight{nlfplMainStrategy = mainStrat} =
  DL.forAllDL DL.anyActions_ $ \(Actions as) ->
    DL.forAllUniqueDL (annotatedStateAfter $ Actions as) mainStrat $ \(Actions as') ->
      counterexample "Main run prop" (propRunActions $ Actions $ as ++ as')
