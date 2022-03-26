{-# LANGUAGE RecordWildCards #-}
module Main where

-- Goal:
-- * describe a network of communicating programs in a single file, like in
--   CloudHaskell, but much simpler.
-- * running the file should produce multiple executables and optionally copy
--   them to other machines, then run all those executables, which then
--   communicate with each other and optionally with users.
-- 
-- Approach:
-- * the file is a Haskell program
-- * compiling the Haskell program produces a single executable
-- * that executable may behave as a different executable when given a flag
-- * running the executable with no flags runs a script which copies the
--   executable to other machines (via scp) and runs each executable with a
--   different flag (via ssh).

import System.Directory (getCurrentDirectory)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))


data Location = Location
  { locationHost
      :: String
      -- ^ e.g. "192.168.0.100"
  , locationPath
      :: FilePath
      -- ^ e.g. "/home/alice/bin/the-executable"
  , locationPort
      :: Int
  }

copyToLocation
  :: Location
  -> IO ()
copyToLocation (Location {..}) = do
  exePath <- getExecutablePath
  print ["scp", exePath, locationHost ++ ":" ++ locationPath]

runWithArgs
  :: Location
  -> [String]
  -> IO ()
runWithArgs (Location {..}) args = do
  print $ ["ssh", locationHost, "--", locationPath] ++ args

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  let location
        :: Location
      location = Location
        { locationHost
            = "localhost"
        , locationPath
            = pwd </> "bin"
        , locationPort
            = 8080
        }
  copyToLocation location
  runWithArgs location ["--behave-as-foo"]
