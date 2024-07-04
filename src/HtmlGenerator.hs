{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerator (generateHtmls) where

import Chapter1 (chapter1)
import Chapter2 (chapter2)
import Chapter3 (chapter3)
import Chapter4 (chapter4)
import Chapters (ConstructedChapter(..), Chapter(..), constructChapters)

import Data.ByteString (writeFile, toStrict)
import Data.Text (empty, pack)
import Lucid
import Prelude hiding (writeFile)

chaptersToGenerate :: [Chapter]
chaptersToGenerate =
    [ chapter1
    , chapter2
    , chapter3
    , chapter4
    ]

indexJs :: Html ()
indexJs = toHtmlRaw $ concat
    [ "window.addEventListener(\"load\", jsMain);"
    , ""
    , "function jsMain()"
    , "{"
    , "    hljs.highlightAll();"
    , "    renderMathInElement(document.body);"
    , "}"
    , ""
    , "function loadLazyImage(elem, url)"
    , "{"
    , "    const imgElem = document.createElement(\"img\");"
    , "    imgElem.src = url;"
    , "    elem.appendChild(imgElem);"
    , ""
    , "    elem.removeAttribute(\"onclick\");"
    , "}"
    ]

indexCss :: Html ()
indexCss = toHtmlRaw $ concat
    [ "*:not(span)"
    , "{"
    , "    font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Noto Sans\", Helvetica, Arial, sans-serif;"
    , "    line-height: 1.5;"
    , "}"
    , ""
    , "h2, h3, h4"
    , "{"
    , "    border-bottom: 1px solid #838B8B;"
    , "}"
    , ""
    , ".qot"
    , "{"
    , "    display: inline-block;"
    , "    padding: 0.1em 0.2em;"
    , "    background-color: #E7EDF3;"
    , "    border-radius: 3px;"
    , "}"
    , ""
    , "img"
    , "{"
    , "    max-width: 100%;"
    , "    height: auto;"
    , "}"
    , ""
    , ".no-text-colour span"
    , "{"
    , "    color: #000000;"
    , "}"
    ]

baseHtml :: String -> Html () -> Html ()
baseHtml title content = do
    doctype_
    html_ [lang_ "ja"] $ do
        head_ $ do
            title_ [] (toHtml $ title ++ " - TSasakiのHaskell入門")
            meta_ [charset_ "UTF-8"]
            meta_ [name_ "viewport", content_ "width=device-width,initial-scale=1"]
            meta_ [name_ "description", content_ "An introduction to Haskell for programming beginners"]
            link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"]
            link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css"]
            style_ [] indexCss
            script_ [defer_ "defer", src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"] empty
            script_ [defer_ "defer", src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/haskell.min.js"] empty
            script_ [defer_ "defer", src_ "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js"] empty
            script_ [defer_ "defer", src_ "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js"] empty
            script_ [] indexJs
        body_ $ do
            a_ [href_ "./index.html"] (h1_ [] "TSasakiのHaskell入門")
            content
            br_ []
            p_ [] "Copyright 2024 TSasaki."

indexHtml :: Html ()
indexHtml = baseHtml "目次" $ do
    h2_ [] "目次"
    ul_ [] $
        foldl (\f (ConstructedChapter chaptNum title _) ->
            f >> li_ [] (a_ [href_ (pack $ "./chapter_" ++ show chaptNum ++ ".html")]
                (toHtml (show chaptNum ++ "章 " ++ title))))
                    (toHtml empty)
                        (constructChapters chaptersToGenerate)

writeHtml :: String -> Html () -> IO ()
writeHtml fileName content =
    putStrLn ("Generating " ++ fileName ++ "...") >>
        writeFile fileName (toStrict $ renderBS content)

writeChapterHtml :: ConstructedChapter -> IO ()
writeChapterHtml (ConstructedChapter chaptNum title content) =
    writeHtml ("./out/chapter_" ++ show chaptNum ++ ".html") (baseHtml title content)

generateHtmls :: IO ()
generateHtmls =
    writeHtml "./out/index.html" indexHtml >>
        mapM_ writeChapterHtml (constructChapters chaptersToGenerate)
