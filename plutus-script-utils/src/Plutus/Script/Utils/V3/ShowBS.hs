{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | On-chain pretty-printing to 'BuiltinString' of the types that are
-- introduced by the Plutus V3 'ScriptContext' (in particular the governance
-- types of the Conway era). The types shared with the lower versions are
-- handled by "Plutus.Script.Utils.V2.ShowBS" and the modules it re-exports, all
-- of whose instances this module re-exports.
module Plutus.Script.Utils.V3.ShowBS () where

import Plutus.Script.Utils.ShowBS
import Plutus.Script.Utils.V2.ShowBS ()
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as Ratio

instance ShowBS Ratio.Rational where
  {-# INLINEABLE showBS #-}
  showBS rat = application2 "Rational" (Ratio.numerator rat) (Ratio.denominator rat)

instance ShowBS V3.MintValue where
  {-# INLINEABLE showBS #-}
  showBS mVal = showBS $ V3.mintValueMinted mVal <> negate (V3.mintValueBurned mVal)

instance ShowBS V3.TxId where
  {-# INLINEABLE showBS #-}
  showBS (V3.TxId x) = application1 "TxId" x

instance ShowBS V3.TxOutRef where
  {-# INLINEABLE showBS #-}
  showBS (V3.TxOutRef txid i) = application2 "TxOutRef" txid i

instance ShowBS V3.TxInInfo where
  {-# INLINEABLE showBS #-}
  showBS (V3.TxInInfo oref out) = application2 "TxInInfo" oref out

instance ShowBS V3.DRepCredential where
  {-# INLINEABLE showBS #-}
  showBS (V3.DRepCredential cred) = application1 "DRep credential" cred

instance ShowBS V3.DRep where
  {-# INLINEABLE showBS #-}
  showBS (V3.DRep dRepCred) = application1 "DRep" dRepCred
  showBS V3.DRepAlwaysAbstain = "DRep always abstain"
  showBS V3.DRepAlwaysNoConfidence = "DRep always no confidence"

instance ShowBS V3.Delegatee where
  {-# INLINEABLE showBS #-}
  showBS (V3.DelegStake pkh) = application1 "Delegate stake" pkh
  showBS (V3.DelegVote dRep) = application1 "Delegate vote" dRep
  showBS (V3.DelegStakeVote pkh dRep) = application2 "Delegate stake vote" pkh dRep

instance ShowBS V3.TxCert where
  {-# INLINEABLE showBS #-}
  showBS (V3.TxCertRegStaking cred maybeDepositAmount) = application2 "Register staking" cred maybeDepositAmount
  showBS (V3.TxCertUnRegStaking cred maybeDepositAmount) = application2 "Unregister staking" cred maybeDepositAmount
  showBS (V3.TxCertDelegStaking cred delegatee) = application2 "Delegate staking" cred delegatee
  showBS (V3.TxCertRegDeleg cred delegatee depositAmount) = application3 "Register and delegate staking" cred delegatee depositAmount
  showBS (V3.TxCertRegDRep dRepCred amount) = application2 "Register DRep" dRepCred amount
  showBS (V3.TxCertUpdateDRep dRepCred) = application1 "Update DRep" dRepCred
  showBS (V3.TxCertUnRegDRep dRepCred amount) = application2 "Unregister DRep" dRepCred amount
  showBS (V3.TxCertPoolRegister poolId poolVFR) = application2 "Register to pool" poolId poolVFR
  showBS (V3.TxCertPoolRetire pkh epoch) = application2 "Retire from pool" pkh epoch
  showBS (V3.TxCertAuthHotCommittee coldCommitteeCred hotCommitteeCred) = application2 "Authorize hot committee" coldCommitteeCred hotCommitteeCred
  showBS (V3.TxCertResignColdCommittee coldCommitteeCred) = application1 "Resign cold committee" coldCommitteeCred

instance ShowBS V3.Voter where
  {-# INLINEABLE showBS #-}
  showBS (V3.CommitteeVoter hotCommitteeCred) = application1 "Committee Voter" hotCommitteeCred
  showBS (V3.DRepVoter dRepCred) = application1 "DRep Voter" dRepCred
  showBS (V3.StakePoolVoter pkh) = application1 "Stake Pool Voter" pkh

instance ShowBS V3.Vote where
  {-# INLINEABLE showBS #-}
  showBS V3.VoteNo = "No"
  showBS V3.VoteYes = "Yes"
  showBS V3.Abstain = "Abstain"

instance ShowBS V3.ChangedParameters where
  {-# INLINEABLE showBS #-}
  showBS (V3.ChangedParameters builtinData) = application1 "Changed parameters" builtinData

instance ShowBS V3.ColdCommitteeCredential where
  {-# INLINEABLE showBS #-}
  showBS (V3.ColdCommitteeCredential cred) = application1 "Cold committee credential" cred

instance ShowBS V3.HotCommitteeCredential where
  {-# INLINEABLE showBS #-}
  showBS (V3.HotCommitteeCredential cred) = application1 "Hot committee credential" cred

instance ShowBS V3.GovernanceActionId where
  {-# INLINEABLE showBS #-}
  showBS (V3.GovernanceActionId txid ix) = application2 "GovernanceActionId" txid ix

instance ShowBS V3.ProtocolVersion where
  {-# INLINEABLE showBS #-}
  showBS (V3.ProtocolVersion major minor) = application2 "Protocol version" major minor

instance ShowBS V3.Constitution where
  {-# INLINEABLE showBS #-}
  showBS (V3.Constitution constitutionScript) = application1 "Constitution" constitutionScript

instance ShowBS V3.Committee where
  {-# INLINEABLE showBS #-}
  showBS (V3.Committee committeeMembers committeeQuorum) = application2 "Committee" committeeMembers committeeQuorum

instance ShowBS V3.GovernanceAction where
  {-# INLINEABLE showBS #-}
  showBS (V3.ParameterChange maybeActionId changeParams mScriptHash) = application3 "Parameter change" maybeActionId changeParams mScriptHash
  showBS (V3.HardForkInitiation maybeActionId protocolVersion) = application2 "HardForkInitiation" maybeActionId protocolVersion
  showBS (V3.TreasuryWithdrawals mapCredLovelace mScriptHash) = application2 "TreasuryWithdrawals" mapCredLovelace mScriptHash
  showBS (V3.NoConfidence maybeActionId) = application1 "NoConfidence" maybeActionId
  showBS (V3.UpdateCommittee maybeActionId toRemoveCreds toAddCreds quorum) = application4 "UpdateCommittee" maybeActionId toRemoveCreds toAddCreds quorum
  showBS (V3.NewConstitution maybeActionId constitution) = application2 "NewConstitution" maybeActionId constitution
  showBS V3.InfoAction = "InfoAction"

instance ShowBS V3.ProposalProcedure where
  {-# INLINEABLE showBS #-}
  showBS (V3.ProposalProcedure ppDeposit ppReturnAddr ppGovernanceAction) = application3 "ProposalProcedure" ppDeposit ppReturnAddr ppGovernanceAction

instance ShowBS V3.ScriptPurpose where
  {-# INLINEABLE showBS #-}
  showBS (V3.Minting cs) = application1 "Minting" cs
  showBS (V3.Spending oref) = application1 "Spending" oref
  showBS (V3.Rewarding cred) = application1 "Rewarding" cred
  showBS (V3.Certifying nb txCert) = application2 "Certifying" nb txCert
  showBS (V3.Voting voter) = application1 "Voting" voter
  showBS (V3.Proposing nb proposal) = application2 "Proposing" nb proposal

instance ShowBS V3.ScriptInfo where
  {-# INLINEABLE showBS #-}
  showBS (V3.MintingScript cs) = application1 "MintingScript" cs
  showBS (V3.SpendingScript oref mDat) = application2 "SpendingScript" oref mDat
  showBS (V3.RewardingScript cred) = application1 "RewardingScript" cred
  showBS (V3.CertifyingScript nb txCert) = application2 "CertifyingScript" nb txCert
  showBS (V3.VotingScript voter) = application1 "VotingScript" voter
  showBS (V3.ProposingScript nb proposal) = application2 "ProposingScript" nb proposal

instance ShowBS V3.TxInfo where
  {-# INLINEABLE showBS #-}
  showBS V3.TxInfo {..} =
    showBSParen
      $ "inputs:"
      <> showBS txInfoInputs
      <> "reference inputs:"
      <> showBS txInfoReferenceInputs
      <> "outputs:"
      <> showBS txInfoOutputs
      <> "fees:"
      <> showBS txInfoFee
      <> "minted value:"
      <> showBS txInfoMint
      <> "certificates:"
      <> showBS txInfoTxCerts
      <> "wdrl:"
      <> showBS txInfoWdrl
      <> "valid range:"
      <> showBS txInfoValidRange
      <> "signatories:"
      <> showBS txInfoSignatories
      <> "redeemers:"
      <> showBS txInfoRedeemers
      <> "datums:"
      <> showBS txInfoData
      <> "transaction id:"
      <> showBS txInfoId
      <> "votes:"
      <> showBS txInfoVotes
      <> "proposals:"
      <> showBS txInfoProposalProcedures
      <> "treasury amount:"
      <> showBS txInfoCurrentTreasuryAmount
      <> "treasury donation:"
      <> showBS txInfoTreasuryDonation

instance ShowBS V3.ScriptContext where
  {-# INLINEABLE showBS #-}
  showBS V3.ScriptContext {..} =
    showBSParen
      $ "Script context:"
      <> "Script Tx info:"
      <> showBS scriptContextTxInfo
      <> "Script redeemer:"
      <> showBS scriptContextRedeemer
      <> "Script info:"
      <> showBS scriptContextScriptInfo
