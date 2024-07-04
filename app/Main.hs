module Main (main) where

import HtmlGenerator (generateHtmls)

import System.Directory (createDirectoryIfMissing)

main :: IO ()
main =
    createDirectoryIfMissing False "./out" >>
        generateHtmls
