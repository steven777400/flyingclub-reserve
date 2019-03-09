{-# LANGUAGE DeriveGeneric #-}

module Request.Api.User (UserDetails(..),
  getUsers, getUserDetails, getUsersDetails,
  createUser, updateUser, deleteUser,
  createAddress, updateAddress, deleteAddress,
  createEmail, updateEmail, deleteEmail,
  createPhone, updatePhone, deletePhone) where

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

      
-- note: between getUserDetails and getUsersDetails it is not just singular/plural
-- there is also the security difference: a user can get their own private details,
-- but only officers get private details of everyone
getUsersDetails :: AuthorizedAction [UserDetails]
getUsersDetails =
  (authorize Officer $ const getUD) <>  
  (authorize Social $
  const $
  fmap
    (map (\(UserDetails user addrs phones emails) ->
       UserDetails
         user
         (filterE S.addressShowInDirectory addrs)
         (filterE S.phoneShowInDirectory phones)
         (filterE S.emailShowInDirectory emails)))
    getUD)
    
  where
    getUD = do
      users <- DB.selectList [notDeleted] []
      addrs <- DB.selectList [notDeleted] []
      phones <- DB.selectList [notDeleted] []
      emails <- DB.selectList [notDeleted] []
      return $ map (\u@(DB.Entity k _)->UserDetails u 
        (filterE (\a->S.addressUserId a == k) addrs)
        (filterE (\a->S.phoneUserId a == k) phones)
        (filterE (\a->S.emailUserId a == k) emails)
        ) users


createUser :: S.User -> PIN -> AuthorizedAction (DB.Key S.User)
createUser user pin =
  authorize Officer $ \(DB.Entity officerId _) -> do
    userId <- insert officerId user
    authId <- liftIO $ S.AuthenticationKey <$> (randomIO :: IO UUID)
    DB.insertKey authId $ S.Authentication userId 0 pin
    return userId


-- TODO tests
updateUser :: DB.Key S.User -> S.User -> AuthorizedAction ()
updateUser userId user = authorize Officer $ \(DB.Entity officerId _) ->
  update officerId userId [S.UserFirstname =. S.userFirstname user
                          ,S.UserLastname =. S.userLastname user
                          ,S.UserPermission =. S.userPermission user] >> return ()

deleteUser :: DB.Key S.User -> AuthorizedAction ()
deleteUser userId = authorize Officer $ \(DB.Entity officerId _) ->
  delete officerId userId


-- we don't expose this because we might not any arbitrary ownable to be created/deleted this way!
create' :: (Audit a, Owner a) => a -> AuthorizedAction (DB.Key a)
create' object =
  (authorize Officer cr) <>
  (authorizeUser (owner object) Social cr)
  where cr (DB.Entity userId _) = insert userId object

delete' :: (Audit a, Owner a) => DB.Key a -> AuthorizedAction ()
delete' objectId = authorizeOwnerOfficer objectId $ \(DB.Entity userId _) ->
  delete userId objectId

createAddress :: S.Address -> AuthorizedAction (DB.Key S.Address)
createAddress = create'

updateAddress :: DB.Key S.Address -> S.Address -> AuthorizedAction ()
updateAddress addressId address =
  authorizeOwnerOfficer addressId $ \(DB.Entity userId _) ->
    update userId addressId   [S.AddressAddress =. S.addressAddress address
                              ,S.AddressCity =. S.addressCity address
                              ,S.AddressState =. S.addressState address
                              ,S.AddressZip =. S.addressZip address
                              ,S.AddressShowInDirectory =. S.addressShowInDirectory address] >> return ()


deleteAddress :: DB.Key S.Address -> AuthorizedAction ()
deleteAddress = delete'


createEmail :: S.Email -> AuthorizedAction (DB.Key S.Email)
createEmail = create'

updateEmail :: DB.Key S.Email -> S.Email -> AuthorizedAction ()
updateEmail emailId email =
  authorizeOwnerOfficer emailId $ \(DB.Entity userId _) ->
    update userId emailId   [S.EmailDescription =. S.emailDescription email
                            ,S.EmailAddress =. S.emailAddress email
                            ,S.EmailReceiveNotification =. S.emailReceiveNotification email
                            ,S.EmailShowInDirectory =. S.emailShowInDirectory email] >> return ()

deleteEmail :: DB.Key S.Email -> AuthorizedAction ()
deleteEmail = delete'


createPhone :: S.Phone -> AuthorizedAction (DB.Key S.Phone)
createPhone = create'

updatePhone :: DB.Key S.Phone -> S.Phone -> AuthorizedAction ()
updatePhone phoneId phone =
  authorizeOwnerOfficer phoneId $ \(DB.Entity userId _) ->
    update userId phoneId   [S.PhoneDescription =. S.phoneDescription phone
                            ,S.PhoneNumber =. S.phoneNumber phone
                            ,S.PhoneReceiveSms =. S.phoneReceiveSms phone
                            ,S.PhoneShowInDirectory =. S.phoneShowInDirectory phone] >> return ()

deletePhone :: DB.Key S.Email -> AuthorizedAction ()
deletePhone = delete'
