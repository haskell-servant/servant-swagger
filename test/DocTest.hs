{-# LANGUAGE CPP #-}
module Main (main) where

import Data.List (isPrefixOf)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
#if MIN_VERSION_servant(0,5,0)
  files <- glob "src/**/*.hs"
  mMacrosFile <- getCabalMacrosFile
  doctest $ "-isrc" : "-Iinclude" :
              (maybe [] (\ f -> ["-optP-include", "-optP" ++ f]) mMacrosFile) ++
              files
#else
  return ()
#endif


getCabalMacrosFile :: IO (Maybe FilePath)
getCabalMacrosFile = do
  exists <- doesDirectoryExist "dist"
  if exists
    then do
      contents <- getDirectoryContents "dist"
      let rest = "build" </> "autogen" </> "cabal_macros.h"
      whenExists $ case filter ("dist-sandbox-" `isPrefixOf`) contents of
        [x] -> "dist" </> x </> rest
        [] -> "dist" </> rest
        xs -> error $ "ran doctests with multiple dist/dist-sandbox-xxxxx's: \n"
                    ++ show xs ++ "\nTry cabal clean"
    else return Nothing
 where
  whenExists :: FilePath -> IO (Maybe FilePath)
  whenExists file = do
    exists <- doesFileExist file
    return $ if exists
      then Just file
      else Nothing
