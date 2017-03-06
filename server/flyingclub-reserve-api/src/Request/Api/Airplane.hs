module Request.Api.Airplane (getAirplanes, getAirplanesDeleted, findAirplanes) where

import           Data.List
import qualified Data.Text                         as T
import           Database.Persist.Audit.Operations
import qualified Database.Persist.Schema           as S
import           Database.Persist.Sql
import           Database.Persist.Types.UserType
import           Request.Api.AuthorizedAction

getAirplanes :: AuthorizedAction [Entity S.Airplane]
getAirplanes = authorize Social $ const $ selectList [notDeleted] []

getAirplanesDeleted :: AuthorizedAction [Entity S.Airplane]
getAirplanesDeleted = authorize Officer $ const $ selectList [] []

-- Note: toCaseFold is used for case insensitive comparisons
findAirplanes :: T.Text -> AuthorizedAction [Entity S.Airplane]
findAirplanes postfix = authorize Social $ const $
  filter (((T.isSuffixOf) postfixCI).(T.toCaseFold).(S.airplaneTail).entityVal) <$> selectList [notDeleted] []
  where postfixCI = T.toCaseFold postfix
