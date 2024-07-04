module Page
    ( Chapter(..)
    , ChapterContent(..)
    , Section(..)
    , SectionContent(..)
    , Subsection(..)
    , beginChapter
    , beginChapterDescription
    , beginSection
    , beginContent
    , beginSubsection
    ) where

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
