{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapters (constructChapters) where

import Chapter1 (chapter1)
import Chapter2 (chapter2)
import Page

import Data.Text (empty)
import Lucid

chapters :: [Chapter]
chapters =
    [ chapter1
    , chapter2
    ]

constructChapters :: [(Int, (String, Html()))]
constructChapters = foldl
    (\lst (chaptNum, chapt) ->
        lst ++ [(chaptNum, (title chapt, constructChapter chaptNum chapt))])
            [] (zip [1..] chapters)
    where title (Chapter t _) = t

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
