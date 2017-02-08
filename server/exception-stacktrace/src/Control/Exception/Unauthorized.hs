module Control.Exception.Unauthorized where

import Control.Exception.StackError
import Control.Exception
import Data.Typeable
import GHC.Stack

data UnauthorizedException = UnauthorizedException CallStack
    deriving (Show)

instance Exception UnauthorizedException





