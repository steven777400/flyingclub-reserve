{-# LANGUAGE DeriveGeneric #-}

module Request.Api.User where

import           Control.Monad.Trans
import           Data.Aeson
import           Database.Persist
import           Database.Persist.Audit.Class
import           Database.Persist.Audit.Operations
import           Database.Persist.Owner.Class
import           Database.Persist.Owner.Instances  ()
import           Database.Persist.Owner.Operations
import qualified Database.Persist.Schema           as S
import           Database.Persist.Sql
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import qualified GHC.Generics                      as G
import           Request.Api.AuthorizedAction

data UserDetails = UserDetails
  { user       :: Entity S.User
  , addressess :: [Entity S.Address]
  , phones     :: [Entity S.Phone]
  , emails     :: [Entity S.Email]
  } deriving (G.Generic)

instance ToJSON UserDetails

getUsers :: AuthorizedAction [Entity S.User]
getUsers = authorize Social $ const $ selectList [notDeleted] []

getUserDetails :: Key S.User -> AuthorizedAction UserDetails
getUserDetails userId =
  (authorize Officer $ const getUD) <>
  (authorizeUser userId Social $ const getUD) <>
  (authorize Social $
   const $
   fmap
     (\(UserDetails user addrs phones emails) ->
        UserDetails
          user
          (filterE S.addressShowInDirectory addrs)
          (filterE S.phoneShowInDirectory phones)
          (filterE S.emailShowInDirectory emails))
     getUD)
  where
    getUD = do
      user <- getOrNotFound userId
      addrs <- selectList [ownedBy userId, notDeleted] []
      phones <- selectList [ownedBy userId, notDeleted] []
      emails <- selectList [ownedBy userId, notDeleted] []
      return $ UserDetails user addrs phones emails
