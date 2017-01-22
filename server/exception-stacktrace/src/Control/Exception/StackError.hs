{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE ExistentialQuantification  #-}
module Control.Exception.StackError (throw, error) where

import Prelude hiding (error)

import qualified Control.Exception as EX
import Data.Typeable
import GHC.Stack

-- Create a stack trace exception which can serve as a base type for all exceptions
-- that include stack traces
-- see also https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Exception.html

data StackTraceException = forall e . EX.Exception e => StackTraceException e CallStack
    deriving Typeable

instance Show StackTraceException where
    show (StackTraceException e cs) = show e ++ ": " ++ show (last $ getCallStack cs)

instance EX.Exception StackTraceException

throw :: (?loc :: CallStack, EX.Exception e) => (CallStack -> e) -> a
throw ex = EX.throw (ex ?loc)

error :: (?loc :: CallStack) => String -> a
error msg = throw (StackTraceException (EX.ErrorCall msg))




