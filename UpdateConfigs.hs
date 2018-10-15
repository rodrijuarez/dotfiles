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
  copyVimrc
  copyPluginsVim
  copyMappingsVim
  copyKittyConf

copyVimrc :: IO ()
copyVimrc = cpDotfile (fromHome ".vimrc")

copyPluginsVim :: IO ()
copyPluginsVim = cpDotfile (fromHome ".plugins.vim")

copyMappingsVim :: IO ()
copyMappingsVim = cpDotfile (fromHome ".mappings.vim")

copyKittyConf :: IO ()
copyKittyConf = cpDotfile (fromHome ".config/kitty/kitty.conf")

dotfilesDir :: IO FilePath
dotfilesDir =
  (<> decodeString "dotfiles/") <$> (decodeString <$> getHomeDirectory)

fromHome :: String -> IO FilePath
fromHome name = (<> decodeString name) <$> (decodeString <$> getHomeDirectory)

fromDotfiles :: String -> IO FilePath
fromDotfiles name = (<> decodeString name) <$> dotfilesDir

cpDotfile :: IO FilePath -> IO ()
cpDotfile originalPath = do
  fromPath <- originalPath
  to <- (fromDotfiles . encodeString $ filename fromPath)
  cp fromPath to
