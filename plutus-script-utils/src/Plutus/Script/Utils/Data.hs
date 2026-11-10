module Plutus.Script.Utils.Data (ToOutputDatum (toOutputDatum)) where

import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2

-- * Converting to Datum

class ToOutputDatum a where
  toOutputDatum :: a -> PV2.OutputDatum

instance ToOutputDatum PV2.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum PV2.Datum where
  toOutputDatum = PV2.OutputDatum

instance ToOutputDatum () where
  toOutputDatum () = PV2.NoOutputDatum

instance ToOutputDatum PV2.DatumHash where
  toOutputDatum = PV2.OutputDatumHash

instance ToOutputDatum PV1.BuiltinData where
  toOutputDatum = toOutputDatum . PV1.Datum
