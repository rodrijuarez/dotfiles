#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, replace, unpack)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Prelude hiding (FilePath)
import Turtle

parser :: Parser (Text, Text, FilePath, Int)
parser =
  (,,,) <$> argText "from" "From string" <*> argText "to" "To string" <*>
  argPath "src" "Src directory to have as reference" <*>
  argInt "print" "Information mode, no replacement"

main = do
  (from, to, src, print) <- options "A simple renamer utility" parser
  sh $ renameAll from to src
  return ()

renameAll :: Text -> Text -> FilePath -> Shell ()
renameAll from to src = do
  file <- find (has (text from)) src
  renameFileTo from to file

renameFileTo :: Text -> Text -> FilePath -> Shell ()
renameFileTo from to file = mv file newFile
  where
    newFile = decodeString . unpack . replace from to . pack $ encodeString file
