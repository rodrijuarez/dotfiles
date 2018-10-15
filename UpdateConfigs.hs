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
import Turtle (cp)

main = do
  copyVimrc
  copyPluginsVim
  copyMappingsVim
  copyKittyConf

dotfiles = (<> decodeString "dotfiles/") <$> (decodeString <$> getHomeDirectory)

fromHome name = (<> decodeString name) <$> (decodeString <$> getHomeDirectory)

fromDotfiles name = (dotfiles <> (return $ decodeString name))

cpDotfile originalPath = do
  name <- filename <$> originalPath
  from <- originalPath
  to <- (fromDotfiles $ encodeString name)
  cp from to

copyVimrc = cpDotfile (fromHome ".vimrc")

copyPluginsVim = cpDotfile (fromHome ".plugins.vim")

copyMappingsVim = cpDotfile (fromHome ".mappings.vim")

copyKittyConf = cpDotfile (fromHome ".config/kitty/kitty.conf")
