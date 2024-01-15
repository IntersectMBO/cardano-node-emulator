{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test facility for 'Cardano.Node.Emulator.API.MonadEmulator'
module Cardano.Node.Emulator.Test (
  -- * Options
  Options (..),
  defaultOptions,
  testnet,

  -- * Basic testing
  runEmulatorM,
  checkPredicate,
  checkPredicateOptions,
  (.&&.),
  hasValidatedTransactionCountOfTotal,
  walletFundsChange,
  renderLogs,

  -- * Testing with `quickcheck-contractmodel`
  propSanityCheckModel,
  propSanityCheckAssertions,
  propRunActions,
  propRunActionsWithOptions,
  checkThreatModelWithOptions,
  checkDoubleSatisfactionWithOptions,
  quickCheckWithCoverage,
  quickCheckWithCoverageAndResult,
  defInitialDist,
  balanceChangePredicate,

  -- * Other exports
  chainStateToChainIndex,
  chainStateToContractModelChainState,
  prettyAddr,

  -- * Re-export quickcheck-contractmodel
  module Test.QuickCheck.ContractModel,
) where

import Cardano.Api qualified as C
import Cardano.Api qualified as CardanoAPI
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.API (
  EmulatorError,
  EmulatorLogs,
  EmulatorM,
  EmulatorMsg (ChainEvent),
  EmulatorState,
  LogMessage (LogMessage),
  awaitSlot,
  emptyEmulatorStateWithInitialDist,
  esChainState,
  fundsAt,
  getParams,
 )
import Cardano.Node.Emulator.Generators (knownAddresses)
import Cardano.Node.Emulator.Internal.Node qualified as E
import Cardano.Node.Emulator.Internal.Node.Params (ledgerProtocolParameters, pNetworkId, testnet)
import Cardano.Node.Emulator.LogMessages (
  EmulatorMsg (TxBalanceMsg),
  TxBalanceMsg (FinishedBalancing),
 )
#if MIN_VERSION_base(4,18,0)
import Control.Applicative ((<|>))
#else
import Control.Applicative (liftA2, (<|>))
#endif
import Control.Lens (use, view, (^.))
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (gets, runRWS)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (runWriterT)
import Data.Default (def)
import Data.Foldable (for_, toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import Data.String (fromString)
import Data.Text qualified as Text
import Ledger (
  CardanoAddress,
  CardanoTx (CardanoEmulatorEraTx),
  OnChainTx,
  getCardanoTxFee,
  onChainTxIsValid,
  unOnChain,
 )
import Ledger.AddressMap qualified as AM
import Ledger.Index qualified as Index
import Ledger.Tx.CardanoAPI (fromCardanoSlotNo)
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Script.Utils.Value (Value)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Coverage (CoverageData, CoverageIndex, CoverageReport (CoverageReport))
import Prettyprinter (pretty, (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import Test.QuickCheck (Property, Testable (property), counterexample)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.ContractModel (
  Actions,
  BalanceChangeOptions (BalanceChangeOptions),
  ChainIndex (ChainIndex, networkId, transactions),
  ChainState (ChainState, slot, utxo),
  ContractModel,
  HasChainIndex,
  IsRunnable,
  ModelState,
  RunModel,
  RunMonad (unRunMonad),
  TxInState (TxInState),
  assertBalanceChangesMatch,
  asserts,
  balanceChanges,
  runContractModel,
  signerPaysFees,
  stateAfter,
  symIsZero,
 )
import Test.QuickCheck.ContractModel qualified as CM
import Test.QuickCheck.ContractModel qualified as QCCM
import Test.QuickCheck.ContractModel.Internal (ContractModelResult)
import Test.QuickCheck.ContractModel.ThreatModel (ThreatModel, assertThreatModel)
import Test.QuickCheck.Monadic (PropertyM, monadic, monadicIO)
import Test.QuickCheck.StateModel (Realized)
import Test.QuickCheck.StateModel qualified as QCSM
import Test.QuickCheck.ThreatModel.DoubleSatisfaction (doubleSatisfaction)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

type EmulatorPredicate = EmulatorLogs -> EmulatorM (Maybe String)

-- | Combine two test predicates
(.&&.) :: EmulatorPredicate -> EmulatorPredicate -> EmulatorPredicate
(.&&.) = liftA2 (liftA2 (<|>))

{- | Test the number of validated transactions and the total number of transactions.
Returns a failure message if the numbers don't match up.
-}
hasValidatedTransactionCountOfTotal :: Int -> Int -> EmulatorPredicate
hasValidatedTransactionCountOfTotal valid total lg =
  let count = \case
        LogMessage _ (ChainEvent (E.TxnValidation Index.Success{})) -> (Sum 1, Sum 0)
        LogMessage _ (ChainEvent (E.TxnValidation Index.FailPhase1{})) -> (Sum 0, Sum 1)
        LogMessage _ (ChainEvent (E.TxnValidation Index.FailPhase2{})) -> (Sum 0, Sum 1)
        _otherLogMsg -> mempty
      (Sum validCount, Sum invalidCount) = foldMap count lg
   in pure $
        if valid /= validCount
          then Just $ "Unexpected number of valid transactions: " ++ show validCount
          else
            if total - valid /= invalidCount
              then Just $ "Unexpected number of invalid transactions: " ++ show invalidCount
              else Nothing

initialWalletFunds :: CardanoAddress -> EmulatorM C.Value
initialWalletFunds addr =
  gets
    ( Map.findWithDefault mempty addr
        . AM.values
        . AM.fromChain
        . pure
        . last
        . view (esChainState . E.chainNewestFirst)
    )

walletFundsChange :: CardanoAddress -> C.Value -> EmulatorPredicate
walletFundsChange addr dlt lg = do
  now <- fundsAt addr
  thn <- initialWalletFunds addr
  let fees = flip foldMap lg $ \case
        LogMessage _ (TxBalanceMsg (FinishedBalancing tx changeAddr)) | changeAddr == addr -> getCardanoTxFee tx
        _ -> mempty
      balance = now <> C.lovelaceToValue fees <> C.negateValue thn
      result = balance == dlt
  pure $
    if result
      then Nothing
      else
        Just $
          unlines $
            map show $
              [ "Expected funds of" <+> fromString (prettyAddr addr) <+> "to change by"
              , " " <+> pretty dlt
              , "  (excluding" <+> pretty fees <+> " in fees)"
              ]
                ++ if balance == mempty
                  then ["but they did not change"]
                  else
                    [ "but they changed by"
                    , " " <+> pretty balance
                    , "a discrepancy of"
                    , " " <+> pretty (balance <> C.negateValue dlt)
                    ]

-- | Render the logs in a format useful for debugging why a test failed.
renderLogs :: EmulatorLogs -> Text.Text
renderLogs =
  Pretty.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions
    . Pretty.vsep
    . toList
    . fmap Pretty.pretty

type instance Realized EmulatorM a = a

instance IsRunnable EmulatorM where
  awaitSlot = awaitSlot . fromCardanoSlotNo

instance HasChainIndex EmulatorM where
  getChainIndex = do
    nid <- pNetworkId <$> getParams
    chainStateToChainIndex nid <$> use esChainState
  getChainState = do
    chainStateToContractModelChainState <$> use esChainState

-- | Sanity check a `ContractModel`. Ensures that wallet balances are not always unchanged.
propSanityCheckModel :: forall state. (ContractModel state) => QC.Property
propSanityCheckModel =
  QC.expectFailure (noBalanceChanges . stateAfter @state)
  where
    noBalanceChanges s = all symIsZero (s ^. balanceChanges)

{- | Sanity check a `ContractModel`. Ensures that all assertions in
the property generation succeed.
-}
propSanityCheckAssertions :: forall state. (ContractModel state) => Actions state -> QC.Property
propSanityCheckAssertions as = asserts $ stateAfter as

-- | Default initial assignment of value to wallet addresses
defInitialDist :: Map CardanoAddress C.Value
defInitialDist = Map.fromList $ (,Value.adaValueOf 100_000_000) <$> knownAddresses

{- | Run `Actions` in the emulator and check that the model and the emulator agree on the final
  wallet balance changes. Starts with 100.000.000 Ada for each wallet and the default parameters.
-}
propRunActions
  :: forall state
   . (RunModel state EmulatorM)
  => Actions state
  -- ^ The actions to run
  -> Property
propRunActions = propRunActionsWithOptions defaultOptions

data Options state = Options
  { initialDistribution :: Map CardanoAddress C.Value
  -- ^ Initial distribution of funds
  , params :: E.Params
  -- ^ Node parameters
  , finalPred :: ModelState state -> EmulatorLogs -> Maybe String
  -- ^ Predicate to check at the end of execution
  , modelPred :: C.LedgerProtocolParameters C.ConwayEra -> ContractModelResult state -> Property
  -- ^ Predicate to run on the contract model
  , coverageIndex :: CoverageIndex
  , coverageRef :: Maybe (IORef CoverageData)
  }

defaultOptions :: Options state
defaultOptions = Options defInitialDist def (\_ _ -> Nothing) balanceChangePredicate mempty Nothing

runEmulatorM
  :: Options state -> EmulatorM a -> (Either EmulatorError a, (EmulatorState, EmulatorLogs))
runEmulatorM Options{..} m =
  case runRWS (runExceptT m) params (emptyEmulatorStateWithInitialDist initialDistribution) of
    (r, s, l) -> (r, (s, l))

checkPredicate
  :: TestName
  -> EmulatorPredicate
  -> EmulatorM a
  -> TestTree
checkPredicate = checkPredicateOptions defaultOptions

checkPredicateOptions
  :: Options state
  -> TestName
  -> EmulatorPredicate
  -> EmulatorM a
  -> TestTree
checkPredicateOptions options testName test contract =
  testCase testName $
    let (res, (st, lg)) = runEmulatorM options contract
     in case res of
          Left err -> assertFailure $ show err
          Right _ ->
            let (res1, _, _) = runRWS (runExceptT (test lg)) (params options) st
             in case res1 of
                  Left err -> assertFailure $ show err
                  Right (Just msg) -> assertFailure $ Text.unpack (renderLogs lg) ++ "\n" ++ msg
                  _ -> pure ()

propRunActionsWithOptions
  :: forall state
   . (RunModel state EmulatorM)
  => Options state
  -> Actions state
  -- ^ The actions to run
  -> Property
propRunActionsWithOptions opts@Options{..} actions =
  asserts finalState
    QC..&&. monadic runFinalPredicate monadicPredicate
  where
    finalState = stateAfter actions

    monadicPredicate :: PropertyM (RunMonad EmulatorM) Property
    monadicPredicate = do
      result <- runContractModel actions
      pure $ modelPred (ledgerProtocolParameters params) result

    runFinalPredicate
      :: RunMonad EmulatorM Property
      -> Property
    runFinalPredicate contract =
      let (res, (s, lg)) =
            runEmulatorM opts
              . fmap fst
              . runWriterT
              . unRunMonad
              $ contract
          cd = s ^. esChainState . E.coverageData
          logs = Text.unpack (renderLogs lg)
       in monadicIO $ do
            liftIO $ for_ coverageRef (`writeIORef` cd)
            case (res, finalPred finalState lg) of
              (Left err, _) ->
                return $
                  counterexample (logs ++ "\n" ++ show err) $
                    property False
              (Right prop, Just msg) -> return $ counterexample (logs ++ "\n" ++ msg) prop
              (Right prop, Nothing) -> return $ counterexample logs prop

balanceChangePredicate
  :: C.LedgerProtocolParameters C.ConwayEra -> ContractModelResult state -> Property
balanceChangePredicate ledgerPP =
  assertBalanceChangesMatch (BalanceChangeOptions False signerPaysFees ledgerPP prettyAddr)

-- | Check a threat model on all transactions produced by the given actions.
checkThreatModelWithOptions
  :: (RunModel state EmulatorM)
  => Options state
  -> ThreatModel a
  -> Actions state
  -- ^ The actions to run
  -> Property
checkThreatModelWithOptions opts threatModel =
  propRunActionsWithOptions opts{modelPred = assertThreatModel threatModel}

checkDoubleSatisfactionWithOptions
  :: (RunModel state EmulatorM)
  => Options state
  -> Actions state
  -- ^ The actions to run
  -> Property
checkDoubleSatisfactionWithOptions opts =
  checkThreatModelWithOptions opts doubleSatisfaction

prettyWalletNames :: [(CardanoAddress, String)]
prettyWalletNames = [(addr, "Wallet " ++ show nr) | (addr, nr) <- zip knownAddresses [1 .. 10 :: Int]]

prettyAddr :: CardanoAddress -> String
prettyAddr a = fromMaybe (show a) $ lookup a prettyWalletNames

-- Note `chainStateToChainIndex` below is moved from `Plutus.Contract.Test.ContractModel.Internal`
-- and could use some serious clean up. Mostly to get rid of the conversions to/from plutus types.

-- Note, we don't store the genesis transaction in the index but put it in the before state
-- instead to avoid showing that as a balance change in the models.
chainStateToChainIndex :: CardanoAPI.NetworkId -> E.ChainState -> ChainIndex
chainStateToChainIndex nid cs =
  ChainIndex -- The Backwards order
    { transactions =
        fst $
          foldr
            addBlock
            ([], beforeState)
            ( reverse
                . drop 1
                . reverse
                . view E.chainNewestFirst
                $ cs
            )
    , networkId = nid
    }
  where
    beforeState =
      CM.ChainState
        { slot = 0
        , utxo = Index.initialise (take 1 $ reverse (cs ^. E.chainNewestFirst))
        }
    addBlock block (txs, state) =
      ( txs
          ++ [ TxInState
              ((\(CardanoEmulatorEraTx tx') -> tx') . unOnChain $ tx)
              state
              (onChainTxIsValid tx)
             | tx <- block
             ]
      , updateState block state
      )

    updateState :: [OnChainTx] -> CM.ChainState -> CM.ChainState
    updateState block state =
      CM.ChainState
        { slot = slot state + 1
        , utxo = Index.insertBlock block (utxo state)
        }

chainStateToContractModelChainState :: E.ChainState -> CM.ChainState
chainStateToContractModelChainState cst =
  ChainState
    { utxo = cst ^. E.index
    , slot = fromIntegral $ cst ^. E.chainCurrentSlot
    }

-- | Run QuickCheck on a property that tracks coverage and print its coverage report.
quickCheckWithCoverage
  :: (QC.Testable prop)
  => QC.Args
  -> Options state
  -> (Options state -> prop)
  -> IO CoverageReport
quickCheckWithCoverage qcargs opts prop = fst <$> quickCheckWithCoverageAndResult qcargs opts prop

quickCheckWithCoverageAndResult
  :: (QC.Testable prop)
  => QC.Args
  -> Options state
  -> (Options state -> prop)
  -> IO (CoverageReport, QC.Result)
quickCheckWithCoverageAndResult qcargs opts prop = do
  ref <- newIORef mempty
  res <- QC.quickCheckWithResult qcargs (prop opts{coverageRef = Just ref})
  covdata <- readIORef ref
  let report = CoverageReport (coverageIndex opts) covdata
  when (QC.chatty qcargs) $ print $ Pretty.pretty report
  return (report, res)

instance QCCM.HasSymbolics Builtins.BuiltinByteString where
  getAllSymbolics _ = mempty
deriving via
  QCSM.HasNoVariables Builtins.BuiltinByteString
  instance
    QCSM.HasVariables Builtins.BuiltinByteString

instance QCCM.SymValueLike Value where
  toSymValue = either (error . show) QCCM.toSymValue . Value.toCardanoValue
