{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Plutus.Examples.EscrowSpec (
  tests,
  -- , redeemTrace
  -- , redeem2Trace
  -- , refundTrace
  prop_Escrow,
  prop_Escrow_DoubleSatisfaction,
  prop_FinishEscrow,
  prop_observeEscrow,
  -- , prop_NoLockedFunds
  prop_validityChecks,
  checkPropEscrowWithCoverage,
  EscrowModel,
) where

import Control.Lens (At (at), makeLenses, to, (%=), (.=), (^.))
import Control.Monad (void, when)
import Control.Monad.Trans (lift)
import Data.Default (Default (def))
import Data.Foldable (Foldable (fold, length, null), sequence_)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)

import Cardano.Api.Shelley (toPlutusData)
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as TimeSlot
import Cardano.Node.Emulator.Test.Coverage (writeCoverageReport)
import Ledger (Slot, minAdaTxOutEstimated)
import Ledger qualified
import Ledger.Tx.CardanoAPI (fromCardanoSlotNo)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value, geq)
import PlutusLedgerApi.V1.Time (POSIXTime)

import Plutus.Examples.Escrow (
  EscrowParams (EscrowParams, escrowDeadline, escrowTargets),
  badRefund,
  pay,
  payToPaymentPubKeyTarget,
  redeem,
  refund,
  typedValidator,
 )
import Plutus.Examples.Escrow qualified as Impl
import PlutusTx (fromData)
import PlutusTx.Monoid (inv)

import Cardano.Api (
  AddressInEra (AddressInEra),
  TxOut (TxOut),
  TxValidityLowerBound (TxValidityLowerBound, TxValidityNoLowerBound),
  TxValidityUpperBound (TxValidityNoUpperBound, TxValidityUpperBound),
  UTxO (unUTxO),
  ValidityLowerBoundSupportedInEra (ValidityLowerBoundInBabbageEra),
  ValidityNoUpperBoundSupportedInEra (ValidityNoUpperBoundInBabbageEra),
  ValidityUpperBoundSupportedInEra (ValidityUpperBoundInBabbageEra),
  toAddressAny,
 )
import Test.QuickCheck qualified as QC hiding ((.&&.))
import Test.QuickCheck.ContractModel (
  Action,
  Actions,
  ContractModel,
  DL,
  RunModel,
  action,
  anyActions_,
  assertModel,
  contractState,
  currentSlot,
  deposit,
  forAllDL,
  lockedValue,
  observe,
  symIsZero,
  utxo,
  viewContractState,
  viewModelState,
  wait,
  waitUntilDL,
  withdraw,
 )
import Test.QuickCheck.ContractModel qualified as QCCM
import Test.QuickCheck.ContractModel.ThreatModel (
  IsInputOrOutput (addressOf),
  ThreatModel,
  anyInputSuchThat,
  changeValidityRange,
  getRedeemer,
  shouldNotValidate,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (
  Property,
  choose,
  frequency,
  testProperty,
 )

type Wallet = Integer

data EscrowModel = EscrowModel
  { _contributions :: Map Wallet Value
  , _refundSlot :: Ledger.Slot
  , _targets :: Map Wallet Value
  }
  deriving (Eq, Show, Generic)

makeLenses ''EscrowModel

modelParams :: EscrowParams d
modelParams = escrowParams $ TimeSlot.scSlotZeroTime def

options :: E.Options EscrowModel
options =
  E.defaultOptions
    { E.params = Params.increaseTransactionLimits def
    , E.coverageIndex = Impl.covIdx
    }

instance ContractModel EscrowModel where
  data Action EscrowModel
    = Pay Wallet Integer
    | Redeem Wallet
    | Refund Wallet
    | BadRefund Wallet Wallet
    deriving (Eq, Show, Generic)

  initialState =
    EscrowModel
      { _contributions = Map.empty
      , _refundSlot =
          succ
            $ TimeSlot.posixTimeToEnclosingSlot def
              . escrowDeadline
            $ modelParams
      , -- TODO: This model is somewhat limited because we focus on one
        -- set of parameters only. The solution is to use the sealed bid
        -- auction trick to generate parameters dynamically that we can
        -- use later on.
        _targets =
          Map.fromList
            [ (w1, Ada.adaValueOf 10)
            , (w2, Ada.adaValueOf 20)
            ]
      }

  nextState a = void $ case a of
    Pay w v -> do
      withdraw (walletAddress w) (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      Data.Foldable.sequence_ [deposit (walletAddress w) v | (w, v) <- Map.toList targets]
      let leftoverValue = Data.Foldable.fold contribs <> inv (Data.Foldable.fold targets)
      deposit (walletAddress w) leftoverValue
      contributions .= Map.empty
      wait 1
    Refund w -> do
      v <- viewContractState $ contributions . at w . to Data.Foldable.fold
      contributions %= Map.delete w
      deposit (walletAddress w) v
      wait 1
    BadRefund _ _ -> do
      wait 2

  precondition s a = case a of
    Redeem _ ->
      (s ^. contractState . contributions . to Data.Foldable.fold)
        `geq` (s ^. contractState . targets . to Data.Foldable.fold)
        && ( s ^. currentSlot . to fromCardanoSlotNo
              < s ^. contractState . refundSlot - 2
           )
    Refund w ->
      s ^. currentSlot . to fromCardanoSlotNo
        > s ^. contractState . refundSlot
        && Nothing /= (s ^. contractState . contributions . at w)
    Pay _ v ->
      s ^. currentSlot . to fromCardanoSlotNo
        < s ^. contractState . refundSlot - 2
        && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue Ledger.minAdaTxOutEstimated
    BadRefund w w' ->
      s ^. currentSlot . to fromCardanoSlotNo < s ^. contractState . refundSlot - 2 -- why -2?
        || w /= w'

  arbitraryAction s =
    frequency $
      [ (prefer beforeRefund, Pay <$> QC.elements testWallets <*> choose @Integer (10, 30))
      , (prefer beforeRefund, Redeem <$> QC.elements testWallets)
      , (prefer afterRefund, BadRefund <$> QC.elements testWallets <*> QC.elements testWallets)
      ]
        ++ [ ( prefer afterRefund
             , Refund <$> QC.elements (s ^. contractState . contributions . to Map.keys)
             )
           | Prelude.not . Data.Foldable.null $
              s ^. contractState . contributions . to Map.keys
           ]
    where
      slot = s ^. currentSlot . to fromCardanoSlotNo
      beforeRefund = slot < s ^. contractState . refundSlot
      afterRefund = Prelude.not beforeRefund
      prefer b = if b then 10 else 1

instance RunModel EscrowModel E.EmulatorM where
  perform _ cmd _ = case cmd of
    Pay w v -> do
      lift $
        pay
          (walletAddress w)
          (walletPrivateKey w)
          modelParams
          (Ada.adaValueOf $ fromInteger v)
    Redeem w -> do
      lift $
        void $
          redeem
            (walletAddress w)
            (walletPrivateKey w)
            modelParams
    Refund w -> do
      lift $
        void $
          refund
            (walletAddress w)
            (walletPrivateKey w)
            modelParams
    BadRefund w w' -> do
      lift $
        void $
          badRefund
            (walletAddress w)
            (walletPrivateKey w)
            modelParams
            (walletPaymentPubKeyHash w')

w1, w2, w3, w4, w5 :: Wallet
w1 = 1
w2 = 2
w3 = 3
w4 = 4
w5 = 5

walletAddress :: Wallet -> Ledger.CardanoAddress
walletAddress = (E.knownAddresses !!) . pred . fromIntegral

walletPaymentPubKeyHash :: Wallet -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash =
  Ledger.PaymentPubKeyHash
    . Ledger.pubKeyHash
    . Ledger.unPaymentPubKey
    . (E.knownPaymentPublicKeys !!)
    . pred
    . fromIntegral

walletPrivateKey :: Wallet -> Ledger.PaymentPrivateKey
walletPrivateKey = (E.knownPaymentPrivateKeys !!) . pred . fromIntegral

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5] -- removed five to increase collisions (, w6, w7, w8, w9, w10])

prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = E.propRunActionsWithOptions options

prop_Escrow_DoubleSatisfaction :: Actions EscrowModel -> Property
prop_Escrow_DoubleSatisfaction = E.checkDoubleSatisfactionWithOptions options

observeUTxOEscrow :: DL EscrowModel ()
observeUTxOEscrow = do
  action $ Pay w1 10
  observe "After payment" $ \_ cst -> numUTxOsAt addr cst == 1
  waitUntilDL 100
  action $ Refund w1
  observe "After refund" $ \_ cst -> numUTxOsAt addr cst == 0
  where
    addr = Scripts.validatorCardanoAddressAny Params.testnet $ typedValidator modelParams

    numUTxOsAt addr cst =
      Data.Foldable.length
        [ ()
        | TxOut (AddressInEra _ addr') _ _ _ <- Map.elems . unUTxO $ utxo cst
        , toAddressAny addr' == addr
        ]

prop_observeEscrow :: Property
prop_observeEscrow = forAllDL observeUTxOEscrow prop_Escrow

finishEscrow :: DL EscrowModel ()
finishEscrow = do
  anyActions_
  finishingStrategy (const True)
  assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: (Wallet -> Bool) -> DL EscrowModel ()
finishingStrategy walletAlive = do
  now <- viewModelState (currentSlot . to fromCardanoSlotNo)
  slot <- viewContractState refundSlot
  when (now < slot + 1) $ waitUntilDL $ fromIntegral $ slot + 1
  contribs <- viewContractState contributions
  Data.Foldable.sequence_
    [action $ Refund w | w <- testWallets, w `Map.member` contribs, walletAlive w]

prop_FinishEscrow :: Property
prop_FinishEscrow = forAllDL finishEscrow prop_Escrow

-- noLockProof :: NoLockedFundsProof EscrowModel
-- noLockProof = defaultNLFP
--   { nlfpMainStrategy   = finishingStrategy (const True)
--   , nlfpWalletStrategy = finishingStrategy . (==) }

-- prop_NoLockedFunds :: Property
-- prop_NoLockedFunds = checkNoLockedFundsProofWithOptions defInitialDist params noLockProof

-- | Check that you can't redeem after the deadline and not refund before the deadline.
validityChecks :: ThreatModel ()
validityChecks = do
  let startTime = TimeSlot.scSlotZeroTime def
      params = escrowParams startTime
      deadline = fromIntegral . TimeSlot.posixTimeToEnclosingSlot def $ escrowDeadline params
      scriptAddr = Scripts.validatorCardanoAddressAny Params.testnet $ typedValidator params
  input <- anyInputSuchThat $ (scriptAddr ==) . addressOf
  rmdr <- (fromData . toPlutusData =<<) <$> getRedeemer input
  case rmdr of
    Nothing -> fail "Missing or bad redeemer"
    Just Impl.Redeem ->
      shouldNotValidate $
        changeValidityRange
          ( TxValidityLowerBound ValidityLowerBoundInBabbageEra deadline
          , TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
          )
    Just Impl.Refund ->
      shouldNotValidate $
        changeValidityRange
          ( TxValidityNoLowerBound
          , TxValidityUpperBound ValidityUpperBoundInBabbageEra (deadline - 1)
          )

prop_validityChecks :: Actions EscrowModel -> Property
prop_validityChecks = E.checkThreatModelWithOptions options validityChecks

tests :: TestTree
tests =
  testGroup
    "escrow"
    -- [ let con = void $ payEp @() @EscrowSchema @EscrowError (escrowParams startTime) in
    --   checkPredicateOptions options "can pay"
    --     ( assertDone con (Trace.walletInstanceTag w1) (const True) "escrow pay not done"
    --     .&&. walletFundsChange w1 (Value.adaValueOf (-10))
    --     )
    --     $ do
    --       hdl <- Trace.activateContractWallet w1 con
    --       Trace.callEndpoint @"pay-escrow" hdl (Ada.adaValueOf 10)
    --       void $ Trace.waitNSlots 1

    -- , let con = void $ selectEither (payEp @()
    --                                        @EscrowSchema
    --                                        @EscrowError
    --                                        (escrowParams startTime))
    --                                 (redeemEp (escrowParams startTime)) in
    --   checkPredicateOptions options "can redeem"
    --     ( assertDone con (Trace.walletInstanceTag w3) (const True) "escrow redeem not done"
    --       .&&. walletFundsChange w1 (Value.adaValueOf (-10))
    --       .&&. walletFundsChange w2 (Value.adaValueOf 10)
    --       .&&. walletFundsChange w3 mempty
    --     )
    --     redeemTrace

    -- , checkPredicateOptions options "can redeem even if more money than required has been paid in"

    --       -- in this test case we pay in a total of 40 lovelace (10 more than required), for
    --       -- the same contract as before, requiring 10 lovelace to go to wallet 1 and 20 to
    --       -- wallet 2.
    --       --
    --       -- The scenario is
    --       -- * Wallet 1 contributes 20
    --       -- * Wallet 2 contributes 10
    --       -- * Wallet 3 contributes 10
    --       -- * Wallet 1 is going to redeem the payments
    --       --

    --       -- Wallet 1 pays 20 and receives 10 from the escrow contract and another 10
    --       -- in excess inputs
    --       ( walletFundsChange w1 (Value.lovelaceValueOf 0)

    --       -- Wallet 2 pays 10 and receives 20, as per the contract.
    --         .&&. walletFundsChange w2 (Value.adaValueOf 10)

    --       -- Wallet 3 pays 10 and doesn't receive anything.
    --         .&&. walletFundsChange w3 (Value.adaValueOf (-10))
    --       )
    --       redeem2Trace

    -- , let con = void (payEp @()
    --                         @EscrowSchema
    --                         @EscrowError
    --                         (escrowParams startTime))
    --          <> void (refundEp (escrowParams startTime)) in
    --   checkPredicateOptions options "can refund"
    --     ( walletFundsChange w1 mempty
    --       .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "refund should succeed")
    --     refundTrace

    -- , HUnit.testCaseSteps "script size is reasonable"
    --     $ \step -> reasonable' step
    --                            (Scripts.validatorScript $ typedValidator (escrowParams startTime))
    --                            32000

    [ testProperty "QuickCheck ContractModel" prop_Escrow
    , -- , testProperty "QuickCheck NoLockedFunds" prop_NoLockedFunds
      testProperty "QuickCheck validityChecks" $ QC.withMaxSuccess 30 prop_validityChecks
    , testProperty "QuickCheck finishEscrow" prop_FinishEscrow
    , testProperty "QuickCheck double satisfaction fails" $
        QC.expectFailure (QC.noShrinking prop_Escrow_DoubleSatisfaction)
    ]

-- where
-- startTime = TimeSlot.scSlotZeroTime def

escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 40000
    , escrowTargets =
        [ payToPaymentPubKeyTarget (walletPaymentPubKeyHash w1) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (walletPaymentPubKeyHash w2) (Ada.adaValueOf 20)
        ]
    }

-- -- | Wallets 1 and 2 pay into an escrow contract, wallet 3
-- --   cashes out.
-- redeemTrace :: Trace.EmulatorTrace ()
-- redeemTrace = do
--     startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
--     let con = void $ selectEither (payEp @()
--                                          @EscrowSchema
--                                          @EscrowError
--                                          (escrowParams startTime))
--                                   (redeemEp (escrowParams startTime))
--     hdl1 <- Trace.activateContractWallet w1 con
--     hdl2 <- Trace.activateContractWallet w2 con
--     hdl3 <- Trace.activateContractWallet w3 con

--     Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
--     Trace.callEndpoint @"pay-escrow" hdl2 (Ada.adaValueOf 10)
--     _ <- Trace.waitNSlots 1
--     Trace.callEndpoint @"redeem-escrow" hdl3 ()
--     void $ Trace.waitNSlots 1

-- -- | Wallets 1-3 pay into an escrow contract, wallet 1 redeems.
-- redeem2Trace :: Trace.EmulatorTrace ()
-- redeem2Trace = do
--     startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
--     let con = void $ both (payEp @()
--                                  @EscrowSchema
--                                  @EscrowError
--                                  (escrowParams startTime)
--                           )
--                           (redeemEp (escrowParams startTime))
--     hdl1 <- Trace.activateContractWallet w1 con
--     hdl2 <- Trace.activateContractWallet w2 con
--     hdl3 <- Trace.activateContractWallet w3 con
--     Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
--     Trace.callEndpoint @"pay-escrow" hdl2 (Ada.adaValueOf 10)
--     Trace.callEndpoint @"pay-escrow" hdl3 (Ada.adaValueOf 10)
--     _ <- Trace.waitNSlots 1
--     Trace.callEndpoint @"redeem-escrow" hdl1 ()
--     void $ Trace.waitNSlots 1

-- -- | Wallet 1 pays into an escrow contract and gets a refund when the
-- --   amount isn't claimed.
-- refundTrace :: Trace.EmulatorTrace ()
-- refundTrace = do
--     startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
--     let con = void (payEp @()
--                           @EscrowSchema
--                           @EscrowError
--                           (escrowParams startTime))
--            <> void (refundEp (escrowParams startTime))
--     hdl1 <- Trace.activateContractWallet w1 con
--     Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
--     _ <- Trace.waitNSlots 100
--     Trace.callEndpoint @"refund-escrow" hdl1 ()
--     void $ Trace.waitNSlots 1

checkPropEscrowWithCoverage :: IO ()
checkPropEscrowWithCoverage = do
  cr <-
    E.quickCheckWithCoverage QC.stdArgs options $ QC.withMaxSuccess 1000 . E.propRunActionsWithOptions
  writeCoverageReport "Escrow" cr
