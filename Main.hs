{-# LANGUAGE RecordWildCards #-}
module Main where

import Clckwrks
import CMS

data SiteURL = Clck ClckURL

main :: IO ()
main = 
  let c = defaultClckwrksConfig  { clckURL = Clck }
  in simpleClckwrks c
