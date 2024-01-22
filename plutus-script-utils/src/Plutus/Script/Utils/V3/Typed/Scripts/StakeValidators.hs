{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Script.Utils.V3.Typed.Scripts.StakeValidators (
  mkForwardingStakeValidator,
  forwardToValidator,
) where

import Plutus.Script.Utils.Scripts (
  StakeValidator,
  ValidatorHash (ValidatorHash),
  mkStakeValidatorScript,
 )
import Plutus.Script.Utils.Typed (mkUntypedStakeValidator)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2.Tx (TxOut (TxOut, txOutAddress))
import PlutusLedgerApi.V3 (
  Address (Address, addressCredential),
  Credential (ScriptCredential),
  ScriptHash (ScriptHash),
  txInInfoResolved,
 )
import PlutusLedgerApi.V3.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Certifying, Rewarding),
  TxInfo (TxInfo, txInfoInputs),
 )
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, ($), (.), (==))

-- TODO: we should add a TypedStakeValidator interface here

{- | A stake validator that checks whether the validator script was run
  in the right transaction.
-}
mkForwardingStakeValidator :: ValidatorHash -> StakeValidator
mkForwardingStakeValidator vshsh =
  mkStakeValidatorScript
    $ $$( PlutusTx.compile
            [||
            \(hsh :: ValidatorHash) ->
              mkUntypedStakeValidator (forwardToValidator hsh)
            ||]
        )
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh

{-# INLINEABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
forwardToValidator (ValidatorHash h) _ ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextPurpose} =
  let checkHash TxOut{txOutAddress = Address{addressCredential = ScriptCredential (ScriptHash vh)}} = vh == h
      checkHash _ = False
      result = any (checkHash . txInInfoResolved) txInfoInputs
   in case scriptContextPurpose of
        Rewarding _ -> result
        Certifying _ -> result
        _ -> False
