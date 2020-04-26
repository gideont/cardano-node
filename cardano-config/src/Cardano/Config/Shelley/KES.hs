{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.KES
  ( KESError
  , SignKey
  , VerKey
  , decodeKESSigningKey
  , decodeKESVerificationKey
  , encodeKESSigningKey
  , encodeKESVerificationKey
  , genKESKeyPair
  , readKESSigningKey
  , readKESVerKey
  , renderKESError
  , writeKESSigningKey
  , writeKESVerKey
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Config.TextView
import           Cardano.Crypto.KES.Class
import qualified Shelley.Spec.Ledger.Keys as Ledger
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto)


-- Local aliases for shorter types:
type VerKey  = Ledger.VKeyES TPraosStandardCrypto
type SignKey = Ledger.SKeyES TPraosStandardCrypto


genKESKeyPair :: Natural -> IO (VerKey, SignKey)
genKESKeyPair duration = do
  signKeyKES <- genKeyKES duration
  let verKeyKes = deriveVerKeyKES signKeyKES
  pure (Ledger.VKeyES verKeyKes, Ledger.SKeyES signKeyKES)

data KESError = ReadKESSigningKeyError !TextViewFileError
              | ReadKESVerKeyError !TextViewFileError
              | WriteKESSigningKeyError !TextViewFileError
              | WriteKESVerKeyError !TextViewFileError

renderKESError :: KESError -> Text
renderKESError kesErr =
  case kesErr of
    ReadKESSigningKeyError err -> "KES signing key read error: " <> renderTextViewFileError err
    ReadKESVerKeyError err -> "KES verification key read error: " <> renderTextViewFileError err
    WriteKESSigningKeyError err -> "KES signing key write error: " <> renderTextViewFileError err
    WriteKESVerKeyError err -> "KES verification key write error: " <> renderTextViewFileError err

encodeKESSigningKey :: SignKey -> TextView
encodeKESSigningKey (Ledger.SKeyES sKeyEs) =
  encodeToTextView tvType' tvTitle' CBOR.toCBOR sKeyEs
 where
  tvType' = "SKeyES TPraosStandardCrypto"
  tvTitle' = "KES Signing Key"

decodeKESSigningKey :: TextView -> Either TextViewError SignKey
decodeKESSigningKey tView = do
  expectTextViewOfType "SKeyES TPraosStandardCrypto" tView
  decodeFromTextView (Ledger.SKeyES <$> CBOR.fromCBOR) tView

encodeKESVerificationKey :: VerKey -> TextView
encodeKESVerificationKey vKeyEs =
  encodeToTextView tvType' tvTitle' CBOR.toCBOR vKeyEs
 where
  tvType' = "VKeyES TPraosStandardCrypto"
  tvTitle' = "KES Verification Key"

decodeKESVerificationKey :: TextView -> Either TextViewError VerKey
decodeKESVerificationKey tView = do
  expectTextViewOfType "VKeyES TPraosStandardCrypto" tView
  decodeFromTextView CBOR.fromCBOR tView

readKESSigningKey :: FilePath -> ExceptT KESError IO SignKey
readKESSigningKey fp =
  firstExceptT ReadKESSigningKeyError
    . newExceptT $ readTextViewEncodedFile decodeKESSigningKey fp

writeKESSigningKey :: FilePath -> SignKey -> ExceptT KESError IO ()
writeKESSigningKey fp sKey =
  firstExceptT WriteKESSigningKeyError
    . newExceptT $ writeTextViewEncodedFile encodeKESSigningKey fp sKey

readKESVerKey :: FilePath -> ExceptT KESError IO VerKey
readKESVerKey fp = do
  firstExceptT ReadKESVerKeyError
    . newExceptT $ readTextViewEncodedFile decodeKESVerificationKey fp

writeKESVerKey :: FilePath -> VerKey -> ExceptT KESError IO ()
writeKESVerKey fp vKeyKES =
  firstExceptT WriteKESVerKeyError
    . newExceptT $ writeTextViewEncodedFile encodeKESVerificationKey fp vKeyKES
