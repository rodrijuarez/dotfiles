#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
{-# LANGUAGE OverloadedStrings #-} --
                                    --

import Control.Monad (liftM2)
import Data.Monoid ((<>))
import Filesystem.Path (filename)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Prelude hiding (FilePath)
import System.Directory (getHomeDirectory)
import Turtle (FilePath, cp)

main = do
  putStrLn "Hello"
