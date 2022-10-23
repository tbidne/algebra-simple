module Main (main) where

import System.Environment.Guard
  ( ExpectEnv (..),
    guardOrElse',
  )
import Test.DocTest qualified as DT

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DT.mainFromCabal "algebra-simple" [])
    (putStrLn "*** Doctests Disabled ***")
