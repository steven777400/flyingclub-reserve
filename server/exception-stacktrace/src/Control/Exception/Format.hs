module Control.Exception.Format where

import Control.Exception.StackError
import Control.Exception
import Data.Typeable
import GHC.Stack

data FormatException = FormatException String CallStack
    deriving (Show)

instance Exception FormatException
