{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Environment.Sqlite where

import           Database.Persist.Schema
import           Database.Persist.Sqlite

runInDb :: SqlM a -> IO a
runInDb sql = runSqlite ":memory:" $
    runAdjustedMigration >>
    sql
