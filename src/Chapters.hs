{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapters
    ( ConstructedChapter(..)
    , Chapter(..)
    , beginChapter
    , beginChapterDescription
    , beginSection
    , beginContent
    , beginSubsection
    , constructChapters
    ) where

import Data.Text (empty)
import Lucid

data ConstructedChapter = ConstructedChapter Int String (Html ())

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

constructChapters :: [Chapter] -> [ConstructedChapter]
constructChapters = zipWith constructChapter [1..]

constructChapter :: Int -> Chapter -> ConstructedChapter
constructChapter chaptNum (Chapter title contents') =
    ConstructedChapter chaptNum title $
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
