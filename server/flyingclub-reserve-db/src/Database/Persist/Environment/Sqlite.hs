{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Environment.Sqlite where

import           Control.Monad                            (when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Database.Persist.Environment.Environment
import           Database.Persist.Schema
import           Database.Persist.Sqlite
import           System.Directory

runInDb :: IO Environment
runInDb = do
  doesFileExist "test.sqlite" >>= flip when (removeFile "test.sqlite")
  runStderrLoggingT $ withSqlitePool "test.sqlite" 1 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool runAdjustedMigration
    return $ Environment $ flip runSqlPersistMPool pool
