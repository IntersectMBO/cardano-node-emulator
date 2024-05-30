{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The set of parameters, like protocol parameters and slot configuration.
module Cardano.Node.Emulator.Internal.Node.Params (
  Params (..),
  paramsFromConfig,
  C.mkLatestTransitionConfig,
  slotConfigL,
  networkIdL,
  emulatorPParamsL,
  emulatorPParams,
  pProtocolParams,
  pParamsFromProtocolParams,
  ledgerProtocolParameters,
  increaseTransactionLimits,
  increaseTransactionLimits',
  emulatorEpochSize,
  emulatorShelleyGenesisDefaults,
  emulatorAlonzoGenesisDefaults,
  emulatorConwayGenesisDefaults,
  keptBlocks,

  -- * cardano-ledger specific types and conversion functions
  EmulatorEra,
  PParams,
  TransitionConfig,
  slotLength,
  testnet,
  emulatorGlobals,
  emulatorEraHistory,
) where

import Cardano.Api qualified as C
import Cardano.Api.NetworkId qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Genesis qualified as C
import Cardano.Ledger.Alonzo.PParams qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api.PParams qualified as C
import Cardano.Ledger.Api.Transition qualified as C
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer), boundRational)
import Cardano.Ledger.Binary.Version (Version, natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.CostModels (mkCostModels)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (ExUnits), Prices (Prices))
import Cardano.Ledger.Shelley.API (Coin (Coin), Globals, mkShelleyGlobals)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Node.Emulator.Internal.Node.TimeSlot (
  SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime),
  beginningOfTime,
  nominalDiffTimeToPOSIXTime,
  posixTimeToNominalDiffTime,
  posixTimeToUTCTime,
  utcTimeToPOSIXTime,
 )
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Control.Lens (makeLensesFor, over, (%~), (&), (.~), (^.))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.SOP (K (K))
import Data.SOP.Counting qualified as Ouroboros
import Data.SOP.NonEmpty qualified as Ouroboros
import Data.SOP.Strict (NP (Nil, (:*)))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Word (Word32)
import Ledger.Test (testNetworkMagic, testnet)
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Plutus.Script.Utils.Scripts (Language (PlutusV1))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime, getPOSIXTime))
import Prettyprinter (Pretty (pretty), viaShow, vsep, (<+>))

-- | The default era for the emulator
type EmulatorEra = ConwayEra StandardCrypto

type PParams = C.PParams EmulatorEra
type TransitionConfig = C.TransitionConfig EmulatorEra

data Params = Params
  { pSlotConfig :: !SlotConfig
  , pEmulatorPParams :: !PParams
  , pNetworkId :: !C.NetworkId
  , pEpochSize :: !EpochSize
  , pConfig :: !TransitionConfig
  }
  deriving (Eq, Show, Generic)

instance ToJSON C.NetworkId where
  toJSON C.Mainnet = JSON.String "Mainnet"
  toJSON (C.Testnet (C.NetworkMagic n)) = JSON.Number $ fromIntegral n
instance FromJSON C.NetworkId where
  parseJSON (JSON.String "Mainnet") = pure C.Mainnet
  parseJSON (JSON.Number n) = pure $ C.Testnet $ C.NetworkMagic $ truncate n
  parseJSON v =
    prependFailure "parsing NetworkId failed, " (typeMismatch "'Mainnet' or Number" v)

deriving newtype instance ToJSON C.NetworkMagic
deriving newtype instance FromJSON C.NetworkMagic

makeLensesFor
  [ ("pSlotConfig", "slotConfigL")
  , ("pEmulatorPParams", "emulatorPParamsL")
  , ("pNetworkId", "networkIdL")
  ]
  ''Params

instance Default Params where
  def = paramsFromConfig defaultConfig

instance Pretty Params where
  pretty Params{..} =
    vsep
      [ "Slot config:" <+> pretty pSlotConfig
      , "Network ID:" <+> viaShow pNetworkId
      , "Protocol Parameters:" <+> viaShow pEmulatorPParams
      ]

-- | Convert `Params` to cardano-ledger `PParams`
emulatorPParams :: Params -> PParams
emulatorPParams = pEmulatorPParams

pProtocolParams :: Params -> C.ProtocolParameters
pProtocolParams p = C.fromLedgerPParams C.ShelleyBasedEraConway $ emulatorPParams p

pParamsFromProtocolParams :: C.ProtocolParameters -> PParams
pParamsFromProtocolParams = either (error . show) id . C.toLedgerPParams C.ShelleyBasedEraConway

ledgerProtocolParameters :: Params -> C.LedgerProtocolParameters C.ConwayEra
ledgerProtocolParameters = C.LedgerProtocolParameters . emulatorPParams

{- | Set higher limits on transaction size and execution units.
This can be used to work around @MaxTxSizeUTxO@ and @ExUnitsTooBigUTxO@ errors.
Note that if you need this your Plutus script will probably not validate on Mainnet.
-}
increaseTransactionLimits :: Params -> Params
increaseTransactionLimits = increaseTransactionLimits' 2 10 10

increaseTransactionLimits' :: Word32 -> Natural -> Natural -> Params -> Params
increaseTransactionLimits' size steps mem =
  over emulatorPParamsL $
    (C.ppMaxTxSizeL %~ (size *)) . (C.ppMaxTxExUnitsL %~ f)
  where
    f :: ExUnits -> ExUnits
    f (ExUnits executionSteps executionMemory) =
      ExUnits (steps * executionSteps) (mem * executionMemory)

emulatorProtocolMajorVersion :: Version
emulatorProtocolMajorVersion = natVersion @9

defaultConfig :: TransitionConfig
defaultConfig =
  C.mkLatestTransitionConfig
    emulatorShelleyGenesisDefaults
    emulatorAlonzoGenesisDefaults
    emulatorConwayGenesisDefaults

emulatorShelleyGenesisDefaults :: C.ShelleyGenesis StandardCrypto
emulatorShelleyGenesisDefaults =
  C.shelleyGenesisDefaults
    { C.sgNetworkMagic = case testNetworkMagic of C.NetworkMagic nm -> nm
    , C.sgSystemStart = posixTimeToUTCTime $ POSIXTime beginningOfTime
    , C.sgProtocolParams =
        C.sgProtocolParams C.shelleyGenesisDefaults
          & C.ppProtocolVersionL .~ ProtVer emulatorProtocolMajorVersion 0
          & C.ppMinFeeBL .~ Coin 155_381
          & C.ppMinFeeAL .~ Coin 44
          & C.ppKeyDepositL .~ Coin 2_000_000
    }

emulatorAlonzoGenesisDefaults :: C.AlonzoGenesis
emulatorAlonzoGenesisDefaults =
  C.alonzoGenesisDefaults
    { C.agPrices =
        Prices (fromJust $ boundRational (577 % 10_000)) (fromJust $ boundRational (721 % 10_000_000))
    , C.agMaxTxExUnits = ExUnits 14_000_000 10_000_000_000
    , C.agCostModels = mkCostModels costModels
    }
  where
    costModel lang = fromJust $ defaultCostModelParams >>= Alonzo.costModelFromMap lang . projectLangParams lang
    costModels = Map.fromList $ map (\lang -> (lang, costModel lang)) [minBound .. maxBound]
    projectLangParams lang m =
      Map.restrictKeys
        (Map.mapKeys (mapParamNames lang) m)
        (Set.fromList (Alonzo.costModelParamNames lang))
    mapParamNames PlutusV1 "blake2b_256-cpu-arguments-intercept" = "blake2b-cpu-arguments-intercept"
    mapParamNames PlutusV1 "blake2b_256-cpu-arguments-slope" = "blake2b-cpu-arguments-slope"
    mapParamNames PlutusV1 "blake2b_256-memory-arguments" = "blake2b-memory-arguments"
    mapParamNames PlutusV1 "verifyEd25519Signature-cpu-arguments-intercept" = "verifySignature-cpu-arguments-intercept"
    mapParamNames PlutusV1 "verifyEd25519Signature-cpu-arguments-slope" = "verifySignature-cpu-arguments-slope"
    mapParamNames PlutusV1 "verifyEd25519Signature-memory-arguments" = "verifySignature-memory-arguments"
    mapParamNames _ name = name

emulatorConwayGenesisDefaults :: C.ConwayGenesis StandardCrypto
emulatorConwayGenesisDefaults = C.conwayGenesisDefaults

paramsFromConfig :: TransitionConfig -> Params
paramsFromConfig tc =
  Params
    { pSlotConfig =
        SlotConfig
          { scSlotZeroTime = utcTimeToPOSIXTime $ C.sgSystemStart sg
          , scSlotLength =
              getPOSIXTime $ nominalDiffTimeToPOSIXTime $ C.Ledger.fromNominalDiffTimeMicro $ C.sgSlotLength sg
          }
    , pEmulatorPParams = tc ^. C.tcInitialPParamsG
    , pNetworkId = C.fromShelleyNetwork (C.sgNetworkId sg) (C.NetworkMagic $ C.sgNetworkMagic sg)
    , pEpochSize = C.sgEpochLength sg
    , pConfig = tc
    }
  where
    sg = tc ^. C.tcShelleyGenesisL

-- | Calculate the cardano-ledger `SlotLength`
slotLength :: Params -> SlotLength
slotLength Params{pSlotConfig} = mkSlotLength $ posixTimeToNominalDiffTime $ POSIXTime $ scSlotLength pSlotConfig

keptBlocks :: Params -> Integer
keptBlocks Params{pConfig} = fromIntegral $ C.sgSecurityParam (pConfig ^. C.tcShelleyGenesisL)

-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: EpochSize
emulatorEpochSize = EpochSize 432_000

-- | A sensible default 'Globals' value for the emulator
emulatorGlobals :: Params -> Globals
emulatorGlobals params@Params{pEpochSize, pConfig} =
  mkShelleyGlobals
    (pConfig ^. C.tcShelleyGenesisL)
    (fixedEpochInfo pEpochSize (slotLength params))
    emulatorProtocolMajorVersion

-- | A sensible default 'EraHistory' value for the emulator
emulatorEraHistory :: Params -> C.EraHistory
emulatorEraHistory params = C.EraHistory (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
  where
    one =
      Ouroboros.nonEmptyHead $
        Ouroboros.getSummary $
          Ouroboros.neverForksSummary (pEpochSize params) (slotLength params)
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil
