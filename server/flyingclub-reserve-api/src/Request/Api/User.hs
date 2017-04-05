{-# LANGUAGE DeriveGeneric #-}

module Request.Api.User (UserDetails(..),
  getUsers, getUserDetails,
  createUser) where

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
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import qualified GHC.Generics                      as G
import           Request.Api.AuthorizedAction
import           System.Random

data UserDetails = UserDetails
  { user       :: DB.Entity S.User
  , addressess :: [DB.Entity S.Address]
  , phones     :: [DB.Entity S.Phone]
  , emails     :: [DB.Entity S.Email]
  } deriving (G.Generic)

instance ToJSON UserDetails

getUsers :: AuthorizedAction [DB.Entity S.User]
getUsers = authorize Social $ const $ DB.selectList [notDeleted] []

getUserDetails :: DB.Key S.User -> AuthorizedAction UserDetails
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
      addrs <- DB.selectList [ownedBy userId, notDeleted] []
      phones <- DB.selectList [ownedBy userId, notDeleted] []
      emails <- DB.selectList [ownedBy userId, notDeleted] []
      return $ UserDetails user addrs phones emails

createUser :: S.User -> PIN -> AuthorizedAction (DB.Key S.User)
createUser user pin =
  authorize Officer $ \(DB.Entity officerId _) -> do
    userId <- insert officerId user
    authId <- liftIO $ S.AuthenticationKey <$> (randomIO :: IO UUID)
    DB.insertKey authId $ S.Authentication userId 0 pin
    return userId

-- TODO can we make a function to avoid this name dup?
-- TODO tests
updateUser :: DB.Key S.User -> S.User -> AuthorizedAction ()
updateUser userId user = authorize Officer $ \(DB.Entity officerId _) ->
  update officerId userId [S.UserFirstname =. S.userFirstname user
                          ,S.UserLastname =. S.userLastname user
                          ,S.UserPermission =. S.userPermission user] >> return ()
