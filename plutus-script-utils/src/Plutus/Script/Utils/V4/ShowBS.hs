{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | On-chain pretty-printing to 'BuiltinString' of the types that are
-- introduced or redefined by the Plutus V4 'ScriptContext' (accounts, guards and
-- the nested top-level transaction view). The types shared with the lower
-- versions are handled by "Plutus.Script.Utils.V3.ShowBS" and the modules it
-- re-exports, all of whose instances this module re-exports.
module Plutus.Script.Utils.V4.ShowBS () where

import Plutus.Script.Utils.ShowBS
import Plutus.Script.Utils.V3.ShowBS ()
import PlutusLedgerApi.V4 qualified as V4
import PlutusTx.Prelude

instance ShowBS V4.AccountId where
  {-# INLINEABLE showBS #-}
  showBS (V4.AccountId cred) = application1 "AccountId" cred

instance ShowBS V4.Address where
  {-# INLINEABLE showBS #-}
  showBS (V4.Address cred mAccountId) = application2 "Address" cred mAccountId

instance ShowBS V4.TxOutRef where
  {-# INLINEABLE showBS #-}
  showBS oref = application2 "TxOutRef" (V4.txOutRefId oref) (V4.txOutRefIdx oref)

instance ShowBS V4.TxOut where
  {-# INLINEABLE showBS #-}
  showBS (V4.TxOut address value datum mRefScriptHash) = application4 "TxOut" address value datum mRefScriptHash

instance ShowBS V4.Rational where
  {-# INLINEABLE showBS #-}
  showBS rat = application2 "Rational" (V4.numerator rat) (V4.denominator rat)

instance ShowBS V4.POSIXTimeRange where
  {-# INLINEABLE showBS #-}
  showBS (V4.POSIXTimeRange lo hi) = application2 "POSIXTimeRange" lo hi

instance ShowBS V4.AccountBalanceInterval where
  {-# INLINEABLE showBS #-}
  showBS (V4.AccountBalanceLowerBound lo) = application1 "AccountBalanceLowerBound" lo
  showBS (V4.AccountBalanceUpperBound hi) = application1 "AccountBalanceUpperBound" hi
  showBS (V4.AccountBalanceBothBounds lo hi) = application2 "AccountBalanceBothBounds" lo hi
  showBS (V4.AccountBalanceExact amount) = application1 "AccountBalanceExact" amount

instance ShowBS V4.AccountBalanceIntervals where
  {-# INLINEABLE showBS #-}
  showBS (V4.AccountBalanceIntervals m) = application1 "AccountBalanceIntervals" m

instance ShowBS V4.GovernanceActionId where
  {-# INLINEABLE showBS #-}
  showBS gaid = application2 "GovernanceActionId" (V4.gaidTxId gaid) (V4.gaidGovActionIx gaid)

instance ShowBS V4.ProtocolVersion where
  {-# INLINEABLE showBS #-}
  showBS pv = application2 "Protocol version" (V4.pvMajor pv) (V4.pvMinor pv)

instance ShowBS V4.Constitution where
  {-# INLINEABLE showBS #-}
  showBS (V4.Constitution constitutionScript) = application1 "Constitution" constitutionScript

instance ShowBS V4.Committee where
  {-# INLINEABLE showBS #-}
  showBS committee = application2 "Committee" (V4.committeeMembers committee) (V4.committeeQuorum committee)

instance ShowBS V4.GovernanceAction where
  {-# INLINEABLE showBS #-}
  showBS (V4.ParameterChange maybeActionId changeParams mScriptHash) = application3 "Parameter change" maybeActionId changeParams mScriptHash
  showBS (V4.HardForkInitiation maybeActionId protocolVersion) = application2 "HardForkInitiation" maybeActionId protocolVersion
  showBS (V4.TreasuryWithdrawals mapCredLovelace mScriptHash) = application2 "TreasuryWithdrawals" mapCredLovelace mScriptHash
  showBS (V4.NoConfidence maybeActionId) = application1 "NoConfidence" maybeActionId
  showBS (V4.UpdateCommittee maybeActionId toRemoveCreds toAddCreds quorum) = application4 "UpdateCommittee" maybeActionId toRemoveCreds toAddCreds quorum
  showBS (V4.NewConstitution maybeActionId constitution) = application2 "NewConstitution" maybeActionId constitution
  showBS V4.InfoAction = "InfoAction"

instance ShowBS V4.ProposalProcedure where
  {-# INLINEABLE showBS #-}
  showBS pp = application3 "ProposalProcedure" (V4.ppDeposit pp) (V4.ppReturnAddr pp) (V4.ppGovernanceAction pp)

instance ShowBS V4.TxCert where
  {-# INLINEABLE showBS #-}
  showBS (V4.TxCertRegAccount accountId depositAmount) = application2 "Register account" accountId depositAmount
  showBS (V4.TxCertUnRegAccount accountId refundAmount) = application2 "Unregister account" accountId refundAmount
  showBS (V4.TxCertDelegAccount accountId delegatee) = application2 "Delegate account" accountId delegatee
  showBS (V4.TxCertRegAccountDeleg accountId delegatee depositAmount) = application3 "Register and delegate account" accountId delegatee depositAmount
  showBS (V4.TxCertRegDRep dRepCred amount) = application2 "Register DRep" dRepCred amount
  showBS (V4.TxCertUpdateDRep dRepCred) = application1 "Update DRep" dRepCred
  showBS (V4.TxCertUnRegDRep dRepCred amount) = application2 "Unregister DRep" dRepCred amount
  showBS (V4.TxCertPoolRegister poolId poolVFR) = application2 "Register to pool" poolId poolVFR
  showBS (V4.TxCertPoolRetire pkh epoch) = application2 "Retire from pool" pkh epoch
  showBS (V4.TxCertAuthHotCommittee coldCommitteeCred hotCommitteeCred) = application2 "Authorize hot committee" coldCommitteeCred hotCommitteeCred
  showBS (V4.TxCertResignColdCommittee coldCommitteeCred) = application1 "Resign cold committee" coldCommitteeCred

instance ShowBS V4.ScriptPurpose where
  {-# INLINEABLE showBS #-}
  showBS (V4.Minting scriptHash cs) = application2 "Minting" scriptHash cs
  showBS (V4.Spending scriptHash oref) = application2 "Spending" scriptHash oref
  showBS (V4.Withdrawing scriptHash cred) = application2 "Withdrawing" scriptHash cred
  showBS (V4.Certifying scriptHash nb txCert) = application3 "Certifying" scriptHash nb txCert
  showBS (V4.Voting scriptHash voter) = application2 "Voting" scriptHash voter
  showBS (V4.Proposing scriptHash nb proposal) = application3 "Proposing" scriptHash nb proposal
  showBS (V4.Guarding scriptHash nb) = application2 "Guarding" scriptHash nb

instance ShowBS V4.ScriptInfo where
  {-# INLINEABLE showBS #-}
  showBS (V4.MintingScript cs) = application1 "MintingScript" cs
  showBS (V4.SpendingScript oref mDat) = application2 "SpendingScript" oref mDat
  showBS (V4.WithdrawingScript accountId) = application1 "WithdrawingScript" accountId
  showBS (V4.CertifyingScript nb txCert) = application2 "CertifyingScript" nb txCert
  showBS (V4.VotingScript voter) = application1 "VotingScript" voter
  showBS (V4.ProposingScript nb proposal) = application2 "ProposingScript" nb proposal
  showBS (V4.GuardingScript nb mTopTxInfo) = application2 "GuardingScript" nb mTopTxInfo

instance ShowBS V4.TxInInfo where
  {-# INLINEABLE showBS #-}
  showBS (V4.TxInInfo oref out) = application2 "TxInInfo" oref out

instance ShowBS V4.TxInfo where
  {-# INLINEABLE showBS #-}
  showBS V4.TxInfo {..} =
    showBSParen
      $ "transaction id:"
      <> showBS txInfoId
      <> "sub-transaction index:"
      <> showBS txInfoSubTxIx
      <> "inputs:"
      <> showBS txInfoInputs
      <> "reference inputs:"
      <> showBS txInfoReferenceInputs
      <> "outputs:"
      <> showBS txInfoOutputs
      <> "minted value:"
      <> showBS txInfoMint
      <> "certificates:"
      <> showBS txInfoTxCerts
      <> "withdrawals:"
      <> showBS txInfoWithdrawals
      <> "direct deposits:"
      <> showBS txInfoDirectDeposits
      <> "account balance intervals:"
      <> showBS txInfoAccountBalanceIntervals
      <> "valid range:"
      <> showBS txInfoValidRange
      <> "guards:"
      <> showBS txInfoGuards
      <> "required top-level guards:"
      <> showBS txInfoRequiredTopLevelGuards
      <> "redeemers:"
      <> showBS txInfoRedeemers
      <> "datums:"
      <> showBS txInfoData
      <> "votes:"
      <> showBS txInfoVotes
      <> "proposals:"
      <> showBS txInfoProposalProcedures
      <> "treasury amount:"
      <> showBS txInfoCurrentTreasuryAmount
      <> "treasury donation:"
      <> showBS txInfoTreasuryDonation

instance ShowBS V4.TopTxInfoSimplified where
  {-# INLINEABLE showBS #-}
  showBS V4.TopTxInfoSimplified {..} =
    showBSParen
      $ "transaction ids:"
      <> showBS ttisIds
      <> "inputs:"
      <> showBS ttisInputs
      <> "reference inputs:"
      <> showBS ttisReferenceInputs
      <> "outputs:"
      <> showBS ttisOutputs
      <> "mints:"
      <> showBS ttisMints
      <> "burns:"
      <> showBS ttisBurns
      <> "certificates:"
      <> showBS ttisTxCerts
      <> "withdrawals:"
      <> showBS ttisWithdrawals
      <> "direct deposits:"
      <> showBS ttisDirectDeposits
      <> "valid range:"
      <> showBS ttisValidRange
      <> "guards:"
      <> showBS ttisGuards
      <> "required top-level guards:"
      <> showBS ttisRequiredTopLevelGuards
      <> "script purposes:"
      <> showBS ttisScriptPurposes
      <> "datums:"
      <> showBS ttisData
      <> "votes:"
      <> showBS ttisVotes
      <> "proposals:"
      <> showBS ttisProposalProcedures
      <> "treasury amount:"
      <> showBS ttisCurrentTreasuryAmount
      <> "treasury donations:"
      <> showBS ttisTreasuryDonations

instance ShowBS V4.TopTxInfo where
  {-# INLINEABLE showBS #-}
  showBS V4.TopTxInfo {..} =
    showBSParen
      $ "sub-transactions:"
      <> showBS topTxInfoSubTransactions
      <> "datums:"
      <> showBS topTxInfoDatums
      <> "starting account balance intervals:"
      <> showBS topTxInfoStartingAccountBalanceIntervals
      <> "simplified:"
      <> showBS topTxInfoSimplified

instance ShowBS V4.ScriptContext where
  {-# INLINEABLE showBS #-}
  showBS V4.ScriptContext {..} =
    showBSParen
      $ "Script context:"
      <> "Script Tx info:"
      <> showBS scriptContextTxInfo
      <> "Script redeemer:"
      <> showBS scriptContextRedeemer
      <> "Script info:"
      <> showBS scriptContextScriptInfo
      <> "Script hash:"
      <> showBS scriptContextScriptHash
