{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Database.Persist.Types.UUID (module Data.UUID) where

import           Control.Exception.StackError
import           Data.Aeson
import           Data.Text
import           Data.UUID
import           Database.Persist.Sql
import           Prelude                      hiding (error)
import           Web.HttpApiData
import           Web.PathPieces

parseUUID :: Text -> Either Text UUID
parseUUID x = case fromText x of
        Just uuid -> Right uuid
        _ -> error $ "Unable to parseUrlPiece for UUID " ++ show x



instance PathPiece UUID where
    fromPathPiece = fromString.unpack
    toPathPiece = pack.toString

instance PersistField UUID where
    toPersistValue = (PersistText).toText

    fromPersistValue (PersistText t) = parseUUID t
    fromPersistValue _ = error "UUID values must be converted from PersistText"

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "CHAR(36)"
