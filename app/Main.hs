{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Error

import           BarthPar.Actions
import           Opts


main :: IO ()
main = runScript $ action =<< parseOpts
