{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       mkValidatorScript, scriptContextTxInfo)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool (False), (.), traceIfFalse, (&&), ($))
import           Utilities            (wrap, Network, writeValidatorToFile)
import Plutus.V2.Ledger.Contexts
    ( txSignedBy, txInfoValidRange, TxInfo )
import Plutus.V1.Ledger.Interval (contains, from)
import Prelude (IO, String)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = traceIfFalse "deadline is not reached yet" deadlineReached && traceIfFalse "You are not a owner of this UTXO" signedByOnwer
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByOnwer :: Bool
        signedByOnwer = txSignedBy info beneficiary

        deadlineReached :: Bool
        deadlineReached = contains (from deadline) $ txInfoValidRange info


{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrap . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)


--- Helper Function ---
saveVal :: PubKeyHash -> IO ()
saveVal = writeValidatorToFile "./assets/manhdt_ppp4_week2_2.plutus" . validator
