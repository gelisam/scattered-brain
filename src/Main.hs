module Main where

-- Goal:
-- * describe a network of communicating programs in a single file, like in
--   CloudHaskell.
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
--   different flag.


main :: IO ()
main = putStrLn "typechecks."
