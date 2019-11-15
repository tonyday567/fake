{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude
import Test.DocTest

main :: IO ()
main = do
  doctest [ "src/Online/Random.hs"]
