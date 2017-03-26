{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Environment.Sqlite where

import           Control.Monad                            (when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Pool
import           Database.Persist.Environment.Environment
import           Database.Persist.Schema
import           Database.Persist.Sqlite
import           System.Directory

runInMemory :: SqlM a -> IO a
runInMemory = runResourceT . runStderrLoggingT . withSqliteConn ":memory:" . runSqlConn

runInDb :: IO Environment
runInDb = do
  doesFileExist "test.sqlite" >>= flip when (removeFile "test.sqlite")
  runStderrLoggingT $ withSqlitePool "test.sqlite" 1 $ \pool -> liftIO $ do
    runPool pool runAdjustedMigration
    return $ Environment $ runPool pool
  where
    runPool :: Pool SqlBackend -> SqlM a -> IO a
    runPool pool x = runResourceT $ runStderrLoggingT $ runSqlPool x pool
