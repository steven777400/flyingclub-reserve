{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric              #-}
module Database.Persist.Types.PIN (PIN, toMaybePIN, toPIN, verifyPIN) where

import           Control.Exception.StackError
import           Database.Persist.Sql
import Data.Aeson
import Data.Text.Encoding
import           Data.ByteString
import qualified Data.ByteString.Char8 as C8
import           Data.Char
import Crypto.Scrypt
import qualified System.IO.Unsafe
import           GHC.Generics
import           Prelude                      hiding (error)

newtype PIN = PIN ByteString
    deriving (Generic)
    

toMaybePIN :: ByteString -> Maybe PIN
toMaybePIN pin  |   C8.all isDigit pin &&
                    C8.length pin >= 4  = Just $ PIN encryptedPin
                |   otherwise           = Nothing
    where encryptedPin = getEncryptedPass $ System.IO.Unsafe.unsafePerformIO $ encryptPassIO' (Pass pin)
    -- Justification: the IO part is only used to query /dev/urandom and it doesn't matter if it is repeated
    -- or in or out of any order
    -- further, pushing this into the IO monad would cause unneeded complexity
    
toPIN :: ByteString -> PIN
toPIN pin = case (toMaybePIN pin) of
    Just p -> p
    Nothing -> error "PIN must be four or more digits"                                            

verifyPIN :: ByteString -> PIN -> Bool
verifyPIN enteredDigits (PIN ep) = verifyPass' (Pass enteredDigits) (EncryptedPass ep)
  
instance Show PIN where
    show (PIN _) = "***"    


instance FromJSON PIN where
    parseJSON (String x) = return $ ((toPIN).encodeUtf8) x
    parseJSON x = error $ "Unable to parseJSON for PIN (non-string) " ++ show x
   
instance ToJSON PIN where
    toJSON (PIN _) = "***"    
    
instance PersistField PIN where
  toPersistValue (PIN pin) = PersistByteString pin

  fromPersistValue (PersistByteString t) = Right $ PIN t
  fromPersistValue _ = error "PIN must be converted from Persist"

instance PersistFieldSql PIN where
    sqlType _ = SqlString