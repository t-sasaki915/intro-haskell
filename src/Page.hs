{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Page
    ( Chapter(..)
    , beginChapter
    , beginChapterDescription
    , beginSection
    , beginContent
    , beginSubsection
    , constructHtml
    ) where

import Data.Text (empty)
import Lucid

data Chapter = Chapter String [ChapterContent]

data ChapterContent = CContent (Html ())
                    | Sect Section

data Section = Section String [SectionContent]

data SectionContent = SContent (Html ())
                    | Subsect Subsection

data Subsection = Subsection String (Html ())

beginChapter :: String -> [ChapterContent] -> Chapter
beginChapter = Chapter

beginChapterDescription :: Html () -> ChapterContent
beginChapterDescription = CContent

beginSection :: String -> [SectionContent] -> ChapterContent
beginSection a = Sect . Section a

beginContent :: Html () -> SectionContent
beginContent = SContent

beginSubsection :: String -> Html () -> SectionContent
beginSubsection a = Subsect . Subsection a

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

constructHtml :: [Chapter] -> Html ()
constructHtml chapters = do
    doctype_
    html_ $ do
        head_ [lang_ "ja"] $ do
            title_ [] "TSasakiのHaskell入門"
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
            h1_ [] "TSasakiのHaskell入門"
            foldl (\f -> (>>) f . uncurry constructChapter) (toHtml empty) (zip [1..] chapters)

constructChapter :: Int -> Chapter -> Html ()
constructChapter chaptNum (Chapter title contents') =
    h2_ [] (toHtml $ show chaptNum ++ " " ++ title) >> contents
    where
        contents = fst $ foldl
            (\(f, sectNum) -> \case
                (CContent cont) ->
                    ( f >> cont
                    , sectNum + 0
                    )
                (Sect sect) ->
                    ( f >> constructSection chaptNum sectNum sect
                    , sectNum + 1
                    )
            )
            (toHtml empty, 1)
            contents'

constructSection :: Int -> Int -> Section -> Html ()
constructSection chaptNum sectNum (Section title contents') =
    let headNum = show chaptNum ++ "." ++ show sectNum in
    h3_ [] (toHtml $ headNum ++ " " ++ title) >> contents
    where
        contents = fst $ foldl
            (\(f, subsectNum) -> \case
                (SContent cont) ->
                    ( f >> cont
                    , subsectNum + 0
                    )
                (Subsect subsect) ->
                    ( f >> constructSubsection chaptNum sectNum subsectNum subsect
                    , subsectNum + 1
                    )
            )
            (toHtml empty, 1)
            contents'

constructSubsection :: Int -> Int -> Int -> Subsection -> Html ()
constructSubsection chaptNum sectNum subsectNum (Subsection title contents) =
    let headNum = show chaptNum ++ "." ++ show sectNum ++ "." ++ show subsectNum in
    h4_ [] (toHtml $ headNum ++ " " ++ title) >> contents
