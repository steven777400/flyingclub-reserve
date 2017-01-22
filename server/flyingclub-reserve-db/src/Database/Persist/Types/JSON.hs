{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Database.Persist.Types.JSON (Data.Aeson.Value) where

import           Control.Exception.StackError
import           Data.Aeson
import           Data.ByteString.Lazy
import           Database.Persist.Sql
import           Prelude                      hiding (error)


-- https://dev.mysql.com/doc/refman/5.7/en/json.html
-- "As of MySQL 5.7.8, MySQL supports a native JSON data type"
-- but we only have 5.5 or something lame.

instance PersistField Value where
  toPersistValue = (PersistByteString).toStrict.encode

  fromPersistValue (PersistByteString t) = case eitherDecodeStrict' t of
      Right val -> Right val
      Left err -> error $ "Can't decode "++(show t)++ " due to "++err
  fromPersistValue _ = error "JSON values must be converted from PersistByteString"

instance PersistFieldSql Value where
  sqlType _ = SqlString
