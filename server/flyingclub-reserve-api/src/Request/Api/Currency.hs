{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Request.Api.Currency (
  getCurrency, getCurrencies,
  createCurrency, updateCurrency, deleteCurrency) where

import           Control.Monad.Trans
import           Data.Aeson
import           Database.Persist                  ((=.))
import           Database.Persist.Audit.Class
import           Database.Persist.Audit.Operations
import           Database.Persist.Owner.Class
import           Database.Persist.Owner.Instances  ()
import           Database.Persist.Owner.Operations
import qualified Database.Persist.Schema           as S
import qualified Database.Persist.Sql              as DB
import           Database.Persist.Types.UUID
import           Database.Persist.Types.UserType
import qualified GHC.Generics                      as G
import           Request.Api.AuthorizedAction
import           System.Random


getCurrencies :: AuthorizedAction [DB.Entity S.Currency]
getCurrencies = authorize Officer $ const $ DB.selectList [notDeleted] []

getCurrency :: DB.Key S.User -> AuthorizedAction [DB.Entity S.Currency]
getCurrency userId =
  (authorize Officer $ const getC) <>
  (authorizeUser userId Social $ const $ 
    (mapE (\cx -> cx { S.currencyComment = "" })) <$> getC)
  where getC = DB.selectList [ownedBy userId, notDeleted] []
      
createCurrency = undefined
updateCurrency = undefined
deleteCurrency = undefined