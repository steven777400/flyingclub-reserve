{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Request.Api.Document (
  getDocuments, getDocument,  
  createDocument, deleteDocument) where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.Char
import qualified Data.ByteString                   as B
import qualified Data.Text                         as T
import           Database.Persist                  ((=.))
import           Database.Persist.Audit.Class
import           Database.Persist.Audit.Operations
import           Database.Persist.Owner.Class
import           Database.Persist.Owner.Instances  ()
import           Database.Persist.Owner.Operations
import qualified Database.Persist.Schema           as S
import qualified Database.Persist.Sql              as DB
import           Database.Persist.Types            (SelectOpt(Desc))
import           Database.Persist.Types.DocumentScope
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import qualified GHC.Generics                      as G
import           Request.Api.AuthorizedAction
import           System.FilePath
import           System.Random

-- TODO update authorize level to only return
-- scoped documents appropriately!
-- duh

getDocuments :: AuthorizedAction [DB.Entity S.Document]
getDocuments = authorize Social $ const $ DB.selectList [] [{-- TODO Desc S.AuditWhen --}]

getDocument :: DB.Key S.Document -> AuthorizedAction (DB.Entity S.Document)
getDocument docId = authorize Social $ const $ getNotDeletedOrNotFound docId

data Upload = Upload 
  { associatedUserId  :: Maybe S.UserId
  , scope             :: DocumentScope
  , fileName          :: FilePath
  , comment           :: T.Text  
  } deriving (G.Generic)
  
instance FromJSON Upload where

sanitizeChar :: Char -> Char
sanitizeChar c | isAlphaNum c                       = c
               | Prelude.elem c [' ', '.', '_']     = '_'
               | otherwise                          = '~'

createDocument :: Upload -> B.ByteString -> AuthorizedAction (DB.Key S.Document)
createDocument upload content = authorize Officer $ \(DB.Entity officerId _) -> 
  createDocument' officerId upload content
      

createDocument' :: DB.Key S.User -> Upload -> B.ByteString -> S.SqlM (DB.Key S.Document)
createDocument' officerId Upload{..} content = do
  fileId <- liftIO (randomIO :: IO UUID)  
  -- place the file into a location on disk,
  -- give it a unique id
  -- create the file path based on
  -- the root path, the scope, the user if existing
  -- TODO get the root path from config
  let rootPath = "./"  
  let userPath = case associatedUserId of
        Nothing -> ""
        Just x -> show x
  
  let relativeFolder = show scope </> userPath
  -- the filename itself will be the part of the uuid
  -- TODO combined with the date
  -- and sanitized name
  
  let targetFilename = (take 8 $ show fileId) <> 
        "~" <> 
        (map sanitizeChar fileName)
  -- copy file to that location
  liftIO $ B.writeFile (rootPath </> relativeFolder </> targetFilename) content
  
  let doc = S.Document 
        associatedUserId 
        scope
        fileId
        targetFilename
        fileName
        comment
        Nothing

  -- add an entry to the database
  insert officerId doc


deleteDocument = undefined