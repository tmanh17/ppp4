{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains, to)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (||), (+))
import           Utilities                 (wrap, writeValidatorToFile, Network, printDataToJSON, validatorAddressBech32, posixTimeFromIso8601)
import Prelude (IO, String)
import Data.Maybe (fromJust)
---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum


-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = traceIfFalse "Only onwer is able to spend Tx before the deadline reachs" ((deadlinePassed && signedBySecondBeneficiary) || (deadlineNotReached && signedByFisrtBeneficiary))
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByFisrtBeneficiary :: Bool
        signedByFisrtBeneficiary = txSignedBy info $ beneficiary1 dat

        signedBySecondBeneficiary :: Bool
        signedBySecondBeneficiary = txSignedBy info $ beneficiary2 dat

        deadlinePassed :: Bool
        deadlinePassed = contains (from $ deadline dat + 1) $ txInfoValidRange info

        deadlineNotReached :: Bool
        deadlineNotReached = contains (to $ deadline dat) $ txInfoValidRange info





{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])


--- Helper Function ---
saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/manhdt_ppp4_week2_1.plutus" validator


vestingAddressBech32 :: Network -> String
vestingAddressBech32 network = validatorAddressBech32 network validator

printVestingDatumJSON :: PubKeyHash -> PubKeyHash -> String -> IO ()
printVestingDatumJSON pkh1 pkh2 time = printDataToJSON $ VestingDatum
    { beneficiary1 = pkh1
    , beneficiary2 = pkh2
    , deadline    = fromJust $ posixTimeFromIso8601 time
    }
