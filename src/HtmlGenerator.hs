{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerator (generateHtmls) where

import Chapters (constructChapters)

import Data.ByteString (writeFile, toStrict)
import Data.Text (empty, pack)
import Lucid
import Prelude hiding (writeFile)

indexJs :: Html ()
indexJs = toHtmlRaw $ concat
    [ "window.addEventListener(\"load\", jsMain);"
    , ""
    , "function jsMain()"
    , "{"
    , "    hljs.highlightAll();"
    , "    renderMathInElement(document.body);"
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
    , "h1, h2, h3, h4"
    , "{"
    , "    border-bottom: 1px solid #838B8B;"
    , "}"
    , ""
    , ".snippet"
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
    html_ $ do
        head_ [lang_ "ja"] $ do
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
            a_ [href_ "index.html"] (h1_ [] "TSasakiのHaskell入門")
            content

indexHtml :: (String, Html ())
indexHtml = ("index.html", baseHtml "目次" $ do
    h2_ [] "目次:"
    ul_ [] $
        foldl (\f (chaptNum, (title, _)) ->
            f >> li_ [] (a_ [href_ (pack $ "chapter_" ++ show chaptNum ++ ".html")]
                (toHtml (show chaptNum ++ "章 " ++ title))))
                    (toHtml empty) constructChapters)

chaptersHtml :: [(String, Html ())]
chaptersHtml = map (\a -> (htmlName a, chaptHtml a)) constructChapters
    where
        htmlName (chaptNum, _) = "chapter_" ++ show chaptNum ++ ".html"
        chaptHtml = uncurry baseHtml . snd

generateHtmls :: IO [()]
generateHtmls =
    mapM
        (uncurry writeFile . mapSnd (toStrict . renderBS))
            (indexHtml : chaptersHtml)
    where mapSnd f (a, b) = (a, f b)
