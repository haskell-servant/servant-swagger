{-# LANGUAGE CPP #-}
module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
#if MIN_VERSION_servant(0,5,0)
  glob "src/**/*.hs" >>= doctest
#else
  return ()
#endif
