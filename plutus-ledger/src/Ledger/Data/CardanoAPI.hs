module Ledger.Data.CardanoAPI
  ( datumHash,
    redeemerHash,
    dataHash,
    toCardanoAPIData,
    toMaybeDatumHash,
  )
where

import Cardano.Api qualified as C.Api
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx.Builtins qualified as Builtins

datumHash :: PV1.Datum -> PV1.DatumHash
datumHash = PV1.DatumHash . dataHash . PV1.getDatum

redeemerHash :: PV1.Redeemer -> PV1.RedeemerHash
redeemerHash = PV1.RedeemerHash . dataHash . PV1.getRedeemer

dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash =
  Builtins.toBuiltin
    . C.Api.serialiseToRawBytes
    . C.Api.hashScriptDataBytes
    . toCardanoAPIData

toCardanoAPIData :: Builtins.BuiltinData -> C.Api.HashableScriptData
toCardanoAPIData =
  C.Api.unsafeHashableScriptData
    . C.Api.fromPlutusData
    . Builtins.builtinDataToData

toMaybeDatumHash :: PV2.OutputDatum -> Maybe PV1.DatumHash
toMaybeDatumHash PV2.NoOutputDatum = Nothing
toMaybeDatumHash (PV2.OutputDatumHash dh) = Just dh
toMaybeDatumHash (PV2.OutputDatum dat) = Just $ datumHash dat
