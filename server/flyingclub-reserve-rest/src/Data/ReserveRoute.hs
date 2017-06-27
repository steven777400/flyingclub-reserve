{-# LANGUAGE RankNTypes #-}
module Data.ReserveRoute where

import           Data.Time.LocalTime.TimeZone.Series
import           Database.Persist.Schema
import           Network.Wai

data ReserveRoute = ReserveRoute {
  sql            :: forall a.SqlM a -> IO a,
  logger         :: Middleware,
  timeZoneSeries :: TimeZoneSeries
}
