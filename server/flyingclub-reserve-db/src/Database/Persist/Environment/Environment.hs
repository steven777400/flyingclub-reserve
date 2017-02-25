{-# LANGUAGE RankNTypes #-}
module Database.Persist.Environment.Environment where

import           Database.Persist.Schema

data Environment = Environment {
  sql :: forall a.SqlM a -> IO a
}
