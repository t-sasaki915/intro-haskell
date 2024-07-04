{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lucid

main :: IO ()
main = print $ renderBS indexHtml

indexHtml :: Html ()
indexHtml = do
    doctype_
    html_ [lang_ "ja"] $ do
        head_ $ do
            title_ [] "TEST"
        
        body_ $ do
            h1_ [] "TESTTEST"
