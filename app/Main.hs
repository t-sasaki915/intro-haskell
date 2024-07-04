module Main (main) where

import Chapter1 (chapter1)
import Chapter2 (chapter2)
import Page (constructHtml)

import Data.ByteString (writeFile, toStrict)
import Lucid (renderBS)
import Prelude hiding (writeFile)

main :: IO ()
main = writeFile "index.html" (toStrict $ renderBS (constructHtml [chapter1, chapter2]))
