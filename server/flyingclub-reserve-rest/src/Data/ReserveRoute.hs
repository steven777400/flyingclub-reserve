{-# LANGUAGE RankNTypes           #-}
module Data.ReserveRoute where

import Database.Persist.Schema



data ReserveRoute = ReserveRoute {
  sql :: forall a.SqlM a -> IO a
}
