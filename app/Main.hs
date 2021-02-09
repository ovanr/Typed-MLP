module Main where

import MLP.App (runInIO)
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
   args <- getArgs
   when (null args) $
      fail "Enter config path"

   runInIO (head args)