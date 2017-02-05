{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Request.Parser.Utility where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T

approxMatch :: String -> Parser T.Text
approxMatch (splitAt 3 -> (begin, rest)) = asciiCI begin' <* many (satisfy $ inClass rest)
    where   begin' = T.pack begin

digits :: Int -> Int -> Parser Int
digits num maxValue = do
    val <- read <$> count num digit
    if val > maxValue then mzero else return val
