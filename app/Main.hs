module Main (main) where

import HtmlGenerator (generateHtmls)

import Control.Monad (filterM)
import System.Directory
import System.Environment (getArgs)
import System.FilePath ((</>), takeFileName)

copyAsset :: FilePath -> IO ()
copyAsset path =
    putStrLn ("Copying " ++ takeFileName path ++ "...") >>
        copyFile path ("." </> "out" </> "assets" </> takeFileName path)

main :: IO ()
main = do
    args <- getArgs

    createDirectoryIfMissing False ("." </> "out")
    createDirectoryIfMissing False ("." </> "out" </> "assets")

    dirContents <- map (head args </>) <$> listDirectory (head args)
    assetPaths  <- filterM doesFileExist dirContents
    mapM_ copyAsset assetPaths

    generateHtmls
