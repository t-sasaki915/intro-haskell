module Main (main) where

import HtmlGenerator (generateHtmls)

import Control.Monad (void)

main :: IO ()
main = void generateHtmls
