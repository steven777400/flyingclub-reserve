{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Environment.Sqlite where

import           Control.Concurrent                       (myThreadId)
import           Control.Monad                            (when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Pool
import           Data.Text
import           Database.Persist.Environment.Environment
import           Database.Persist.Schema
import           Database.Persist.Sqlite
import           System.Directory

runInMemory :: SqlM a -> IO a
runInMemory = runResourceT . runStderrLoggingT . withSqliteConn ":memory:" . runSqlConn

runInDb :: IO Environment
runInDb = do
  tid <- myThreadId
  let sqlfile = "test" ++ (show tid) ++".sqlite"
  doesFileExist sqlfile >>= flip when (removeFile sqlfile)
  runStderrLoggingT $ withSqlitePool (pack sqlfile) 1 $ \pool -> liftIO $ do
    runPool pool runAdjustedMigration
    return $ Environment $ runPool pool
  where
    runPool :: Pool SqlBackend -> SqlM a -> IO a
    runPool pool x = runResourceT $ runStderrLoggingT $ runSqlPool x pool
