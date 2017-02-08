{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Types.PhoneNumber (PhoneNumber, toMaybePhoneNumber, toPhoneNumber, unPhoneNumber) where

import           Control.Exception.Format
import           Control.Exception.StackError
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Char8        as C8
import           Data.Char
import           Data.Text.Encoding
import           Database.Persist.Sql
import           GHC.Generics
import           Prelude                      hiding (error)

newtype PhoneNumber = PhoneNumber ByteString
    deriving (Eq, Generic)

toMaybePhoneNumber :: ByteString -> Maybe PhoneNumber
toMaybePhoneNumber ph   | C8.length digits == 10    = Just $ PhoneNumber digits
                        | C8.length digits == 11 && C8.head digits == '1'
                                                    = Just $ PhoneNumber (C8.tail digits)
                        | otherwise = Nothing
    where digits = C8.filter isDigit ph


toPhoneNumber :: ByteString -> PhoneNumber
toPhoneNumber ph = case (toMaybePhoneNumber ph) of
    Just p -> p
    Nothing -> throw $ FormatException "PhoneNumber must be ten digits or 1+ten digits"


unPhoneNumber :: PhoneNumber -> ByteString
unPhoneNumber (PhoneNumber ph) = (areacode `C8.snoc` '-') `C8.append`
                                (prefix `C8.snoc` '-') `C8.append`
                                number
        where   (areacode,rest) = C8.splitAt 3 ph
                (prefix,number) = C8.splitAt 3 rest

instance Show PhoneNumber where
    show ph = show $ unPhoneNumber ph


instance FromJSON PhoneNumber where
    parseJSON (String x) = return $ ((toPhoneNumber).encodeUtf8) x
    parseJSON x = error $ "Unable to parseJSON for PhoneNumber (non-string) " ++ show x

instance ToJSON PhoneNumber where
    toJSON (PhoneNumber pn) = ((String).decodeUtf8) pn

instance PersistField PhoneNumber where
  toPersistValue (PhoneNumber ph) = PersistByteString ph

  fromPersistValue (PersistByteString t) = Right $ PhoneNumber t
  fromPersistValue _ = error "PhoneNumbers must be converted from Persist"

instance PersistFieldSql PhoneNumber where
    sqlType _ = SqlOther "CHAR(10)"
