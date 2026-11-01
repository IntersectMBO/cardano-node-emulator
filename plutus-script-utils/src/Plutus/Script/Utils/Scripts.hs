{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Script.Utils.Scripts
  ( Script (..),
    PV1.ScriptHash (..),
    ToScript (..),
    ToScriptHash (..),
    ToCardanoScriptHash (..),
    Language (..),
    Versioned (..),
    ToVersioned (..),
    Validator (..),
    ToValidator (..),
    ValidatorHash (..),
    ToValidatorHash (..),
    MintingPolicy (..),
    ToMintingPolicy (..),
    MintingPolicyHash (..),
    ToMintingPolicyHash (..),
    StakeValidator (..),
    ToStakeValidator (..),
    StakeValidatorHash (..),
    ToStakeValidatorHash (..),
    toCurrencySymbol,
  )
where

import Cardano.Api qualified as C.Api
import Cardano.Ledger.Plutus.Language (Language (PlutusV1, PlutusV2, PlutusV3))
import Codec.CBOR.Extras (SerialiseViaFlat (SerialiseViaFlat))
import Codec.CBOR.Write qualified as CBOR
import Codec.Serialise (Serialise (encode), deserialiseOrFail)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), ToJSONKey, object, (.:), (.=))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address (ToAddress (toAddress), ToCardanoAddress (toCardanoAddress), ToCredential (toCredential))
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Bytes (LedgerBytes (LedgerBytes))
import PlutusTx (CompiledCode, makeLift)
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude (BuiltinUnit)
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Extras (PrettyShow (PrettyShow))

-- * Script hashes

-- | Extracting Plutus script hashes
class ToScriptHash a where
  toScriptHash :: a -> PV1.ScriptHash

instance ToScriptHash PV1.ScriptHash where
  toScriptHash = id

instance ToScriptHash C.Api.ScriptHash where
  toScriptHash = PV1.ScriptHash . Builtins.toBuiltin . C.Api.serialiseToRawBytes

instance ToScriptHash PV1.CurrencySymbol where
  toScriptHash = coerce

-- | Extracting Cardano script hashes
class ToCardanoScriptHash a where
  toCardanoScriptHash :: a -> C.Api.ScriptHash

instance ToCardanoScriptHash C.Api.ScriptHash where
  toCardanoScriptHash = id

-- * Scripts

-- | Script wrapper around a ShortByteString (SerialisedScript)
newtype Script = Script {unScript :: PV1.SerialisedScript}
  deriving stock (Eq, Ord, Generic)
  deriving (Serialise) via PV1.SerialisedScript

instance Show Script where
  showsPrec _ _ = showString "<Script>"

-- Bypass the on-chain Serialise size checks for scripts sent in JSON events.
instance ToJSON Script where
  toJSON (Script p) =
    JSON.String $
      TE.decodeUtf8 $
        Base16.encode $
          CBOR.toStrictByteString $
            encode (SerialiseViaFlat p)

instance FromJSON Script where
  parseJSON value = do
    bytes <- JSON.withText "ByteString" (either fail pure . Base16.decode . TE.encodeUtf8) value
    case deserialiseOrFail (BSL.fromStrict bytes) of
      Left err -> fail $ show err
      Right (SerialiseViaFlat p) -> pure $ Script p

encodeBuiltinByteString :: Builtins.BuiltinByteString -> JSON.Value
encodeBuiltinByteString =
  JSON.String . TE.decodeUtf8 . Base16.encode . Builtins.fromBuiltin

decodeBuiltinByteString :: JSON.Value -> Parser Builtins.BuiltinByteString
decodeBuiltinByteString value =
  Builtins.toBuiltin
    <$> JSON.withText "ByteString" (either fail pure . Base16.decode . TE.encodeUtf8) value

-- | Extracting scripts
class ToScript a where
  toScript :: a -> Script

instance ToScript Script where
  toScript = id

instance ToScript PV1.SerialisedScript where
  toScript = Script

-- | Instance for staking and minting scripts in PlutusV1 and PlutusV2
instance ToScript (CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toScript = toScript . serialiseCompiledCode

-- | Instance for spending scripts in PlutusV1 and PlutusV2
instance ToScript (CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toScript = toScript . serialiseCompiledCode

-- | Instance for all scripts in PlutusV3
instance ToScript (CompiledCode (BuiltinData -> BuiltinUnit)) where
  toScript = toScript . serialiseCompiledCode

-- * Scripts with a version attached

deriving anyclass instance Serialise Language

instance Pretty Language where
  pretty PlutusV1 = "Plutus V1"
  pretty PlutusV2 = "Plutus V2"
  pretty PlutusV3 = "Plutus V3"

-- | A script of some kind with its Plutus language version
data Versioned script = Versioned
  { unversioned :: script,
    version :: Language
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance (Pretty script) => Pretty (Versioned script) where
  pretty Versioned {unversioned, version} = pretty unversioned <> " (" <> pretty version <> ")"

instance (ToScript a) => ToCardanoScriptHash (Versioned a) where
  toCardanoScriptHash (Versioned (toScript -> Script script) lang) = case lang of
    PlutusV1 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV1 $ C.Api.PlutusScriptSerialised script
    PlutusV2 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV2 $ C.Api.PlutusScriptSerialised script
    PlutusV3 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV3 $ C.Api.PlutusScriptSerialised script

instance (ToScript a) => ToScript (Versioned a) where
  toScript = toScript . unversioned

instance (ToScript a) => ToScriptHash (Versioned a) where
  toScriptHash = toScriptHash . toCardanoScriptHash

instance (ToScript a) => ToCredential (Versioned a) where
  toCredential = PV1.ScriptCredential . toScriptHash

instance (ToScript a) => ToAddress (Versioned a) where
  toAddress = (`PV1.Address` Nothing) . toCredential

class ToVersioned s a where
  toVersioned :: a -> Versioned s

instance ToVersioned a (Versioned a) where
  toVersioned = id

-- | Anything that can be seen as a versioned script can be assigned a Cardano
-- address. However this address will bear no staking.
instance (ToScript a) => ToCardanoAddress (Versioned a) where
  toCardanoAddress networkId =
    flip (C.Api.makeShelleyAddressInEra C.Api.shelleyBasedEra networkId) C.Api.NoStakeAddress
      . C.Api.PaymentCredentialByScript
      . toCardanoScriptHash

-- * Validators

-- | 'Validator' is a wrapper around 'Script's which are used as validators in
-- transaction outputs.
newtype Validator = Validator {getValidator :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Pretty) via (PrettyShow Validator)

instance Show Validator where
  show = const "Validator { <script> }"

class ToValidator a where
  toValidator :: a -> Validator

instance ToValidator Validator where
  toValidator = id

instance ToValidator Script where
  toValidator = Validator

instance ToScript Validator where
  toScript = getValidator

instance (ToValidator a) => ToValidator (Versioned a) where
  toValidator = toValidator . unversioned

-- | Instance for PlutusV1 and PlutusV2 validators
instance ToValidator (CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toValidator = toValidator . toScript

instance ToVersioned Script (Versioned Validator) where
  toVersioned = fmap getValidator

instance ToVersioned Validator (Versioned Script) where
  toVersioned = fmap Validator

-- * Validators hashes

newtype ValidatorHash = ValidatorHash {getValidatorHash :: Builtins.BuiltinByteString}
  deriving (IsString, Show, Pretty) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Hashable, Serialise)
  deriving anyclass (FromJSONKey, ToJSONKey)

instance ToJSON ValidatorHash where
  toJSON = object . pure . ("getValidatorHash" .=) . encodeBuiltinByteString . getValidatorHash

instance FromJSON ValidatorHash where
  parseJSON = JSON.withObject "ValidatorHash" $ \value ->
    ValidatorHash <$> (value .: "getValidatorHash" >>= decodeBuiltinByteString)

makeLift ''ValidatorHash

class ToValidatorHash a where
  toValidatorHash :: a -> ValidatorHash

instance ToValidatorHash ValidatorHash where
  toValidatorHash = id

instance ToValidatorHash PV1.ScriptHash where
  toValidatorHash = coerce

instance ToValidatorHash (Versioned Validator) where
  toValidatorHash = toValidatorHash . toScriptHash . fmap toScript

instance ToValidatorHash (Versioned Script) where
  toValidatorHash = toValidatorHash . toScriptHash

instance ToScriptHash ValidatorHash where
  toScriptHash = coerce

-- * Minting policies

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy {getMintingPolicy :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Pretty) via (PrettyShow MintingPolicy)

instance Show MintingPolicy where
  show = const "MintingPolicy { <script> }"

class ToMintingPolicy a where
  toMintingPolicy :: a -> MintingPolicy

instance ToMintingPolicy MintingPolicy where
  toMintingPolicy = id

instance ToMintingPolicy Script where
  toMintingPolicy = MintingPolicy

instance ToScript MintingPolicy where
  toScript = getMintingPolicy

instance (ToMintingPolicy a) => ToMintingPolicy (Versioned a) where
  toMintingPolicy = toMintingPolicy . unversioned

-- | Instance for PlutusV1 and PlutusV2 minting policies
instance ToMintingPolicy (CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toMintingPolicy = toMintingPolicy . toScript

instance ToVersioned Script (Versioned MintingPolicy) where
  toVersioned = fmap getMintingPolicy

instance ToVersioned MintingPolicy (Versioned Script) where
  toVersioned = fmap MintingPolicy

-- * Minting policies hashes

-- | Script runtime representation of a @Digest SHA256@.
newtype MintingPolicyHash = MintingPolicyHash {getMintingPolicyHash :: Builtins.BuiltinByteString}
  deriving (IsString, Show, Pretty) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Hashable, Serialise)
  deriving anyclass (FromJSONKey, ToJSONKey)

instance ToJSON MintingPolicyHash where
  toJSON = object . pure . ("getMintingPolicyHash" .=) . encodeBuiltinByteString . getMintingPolicyHash

instance FromJSON MintingPolicyHash where
  parseJSON = JSON.withObject "MintingPolicyHash" $ \value ->
    MintingPolicyHash <$> (value .: "getMintingPolicyHash" >>= decodeBuiltinByteString)

makeLift ''MintingPolicyHash

class ToMintingPolicyHash a where
  toMintingPolicyHash :: a -> MintingPolicyHash

instance ToMintingPolicyHash MintingPolicyHash where
  toMintingPolicyHash = id

instance ToMintingPolicyHash PV1.ScriptHash where
  toMintingPolicyHash = coerce

instance ToMintingPolicyHash (Versioned MintingPolicy) where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash . fmap toScript

instance ToMintingPolicyHash (Versioned Script) where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash

instance ToMintingPolicyHash PV1.CurrencySymbol where
  toMintingPolicyHash = coerce

instance ToScriptHash MintingPolicyHash where
  toScriptHash = coerce

-- | CurrencySymbol and MintingPolicyHash are isomorphic, so anything that can be
-- translated to a MintingPolicyHash can be seen as a CurrencySymbol
{-# INLINEABLE toCurrencySymbol #-}
toCurrencySymbol :: (ToMintingPolicyHash script) => script -> PV1.CurrencySymbol
toCurrencySymbol = coerce . toMintingPolicyHash

-- * Stake validators

-- | 'StakeValidator' is a wrapper around 'Script's which are used as validators
-- for any other purpose than Minting or Spending
newtype StakeValidator = StakeValidator {getStakeValidator :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Pretty) via (PrettyShow MintingPolicy)

instance Show StakeValidator where
  show = const "StakeValidator { <script> }"

class ToStakeValidator a where
  toStakeValidator :: a -> StakeValidator

instance ToStakeValidator StakeValidator where
  toStakeValidator = id

instance ToStakeValidator Script where
  toStakeValidator = StakeValidator

instance ToScript StakeValidator where
  toScript = getStakeValidator

instance (ToStakeValidator a) => ToStakeValidator (Versioned a) where
  toStakeValidator = toStakeValidator . unversioned

instance ToStakeValidator (CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toStakeValidator = toStakeValidator . toScript

instance ToVersioned Script (Versioned StakeValidator) where
  toVersioned = fmap getStakeValidator

instance ToVersioned StakeValidator (Versioned Script) where
  toVersioned = fmap StakeValidator

-- * Stake validators hashes

-- | Script runtime representation of a @Digest SHA256@.
newtype StakeValidatorHash = StakeValidatorHash {getStakeValidatorHash :: Builtins.BuiltinByteString}
  deriving (IsString, Show, Pretty) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Hashable, Serialise)
  deriving anyclass (FromJSONKey, ToJSONKey)

instance ToJSON StakeValidatorHash where
  toJSON = object . pure . ("getStakeValidatorHash" .=) . encodeBuiltinByteString . getStakeValidatorHash

instance FromJSON StakeValidatorHash where
  parseJSON = JSON.withObject "StakeValidatorHash" $ \value ->
    StakeValidatorHash <$> (value .: "getStakeValidatorHash" >>= decodeBuiltinByteString)

makeLift ''StakeValidatorHash

class ToStakeValidatorHash a where
  toStakeValidatorHash :: a -> StakeValidatorHash

instance ToStakeValidatorHash StakeValidatorHash where
  toStakeValidatorHash = id

instance ToStakeValidatorHash PV1.ScriptHash where
  toStakeValidatorHash = coerce

instance ToStakeValidatorHash (Versioned StakeValidator) where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash . fmap toScript

instance ToStakeValidatorHash (Versioned Script) where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash

instance ToScriptHash StakeValidatorHash where
  toScriptHash = coerce
