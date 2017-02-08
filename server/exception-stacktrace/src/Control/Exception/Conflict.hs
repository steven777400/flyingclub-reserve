module Control.Exception.Conflict where

import Control.Exception.StackError
import Control.Exception
import Data.Typeable
import GHC.Stack

data ConflictException = ConflictException String CallStack
    deriving (Show)

instance Exception ConflictException
