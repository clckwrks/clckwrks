#!/usr/bin/env runghc

module Main where

import Distribution.Simple
import Distribution.Simple.Program

hsx2hsProgram = simpleProgram "hsx2hs"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
         hookedPrograms = [hsx2hsProgram]
       }
