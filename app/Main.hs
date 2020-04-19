module Main where

import Sat
import Control.Monad
import Data.List

main :: IO ()
main = interact (intercalate "\n" . map handleInp . lines)
  where
    handleInp s = (s ++).('\n':).either show (pprintEnvs.falsify) $ parseExpr s
