{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V4.Address where

import Plutus.Script.Utils.Credential (ToCredential (toCredential))
import PlutusLedgerApi.V4 qualified as Api
import PlutusTx.Prelude (Maybe (Just, Nothing), id, (.))

-- | 'ToAddress' is more contentious than other similar type classes and should
-- be used with caution because most of the instances will just ignore the
-- staking credential. One can see 'ToAdress' as a synonym of
-- 'ToAddressWithEmptyAccountId', which would be a (much too long) more
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
class ToMaybeAccountId a where
  toMaybeAccountId :: a -> Maybe Api.AccountId

instance ToMaybeAccountId Api.AccountId where
  toMaybeAccountId = Just

instance ToMaybeAccountId Api.Address where
  toMaybeAccountId = Api.addressStakingAccountId

instance ToMaybeAccountId (Maybe Api.AccountId) where
  toMaybeAccountId = id
