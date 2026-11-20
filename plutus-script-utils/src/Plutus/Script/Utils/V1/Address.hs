module Plutus.Script.Utils.V1.Address where

import Plutus.Script.Utils.Credential (ToCredential (toCredential))
import PlutusLedgerApi.V1 qualified as Api

-- | 'ToAddress' is more contentious than other similar type classes and should
-- be used with caution because most of the instances will just ignore the
-- staking credential. One can see 'ToAdress' as a synonym of
-- 'ToAddressWithEmptyStakingCredential', which would be a (much too long) more
-- accurate name.
class ToAddress a where
  toAddress :: a -> Api.Address

instance ToAddress Api.ScriptHash where
  toAddress = (`Api.Address` Nothing) . toCredential

instance ToAddress Api.PubKeyHash where
  toAddress = (`Api.Address` Nothing) . toCredential

instance ToAddress Api.Credential where
  toAddress = (`Api.Address` Nothing)

instance ToAddress Api.Address where
  toAddress = id

-- | Addresses contain an optional staking credential, so it makes more sense to
-- have a class which returns it.
class ToMaybeStakingCredential a where
  toMaybeStakingCredential :: a -> Maybe Api.StakingCredential

instance ToMaybeStakingCredential Api.StakingCredential where
  toMaybeStakingCredential = Just

instance ToMaybeStakingCredential Api.Address where
  toMaybeStakingCredential = Api.addressStakingCredential

instance ToMaybeStakingCredential (Maybe Api.StakingCredential) where
  toMaybeStakingCredential = id
