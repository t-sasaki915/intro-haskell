{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Component where

import           Data.List (intercalate)
import           Data.Text (empty, pack)
import           Lucid

hlink :: String -> String -> Html ()
hlink link = a_ [href_ (pack link)] . toHtml

latex :: String -> Html ()
latex equ = toHtml $ "\\(" ++ equ ++ "\\)"

latexBlock :: String -> Html ()
latexBlock equ = toHtml $ "$$" ++ equ ++ "$$"

latexAlign :: [String] -> Html ()
latexAlign equs = latexBlock ("\\begin{aligned}" ++ intercalate "\\\\" equs ++ "\\end{aligned}")

dotList :: Html () -> Html ()
dotList = ul_ []

listItem :: String -> Html ()
listItem = li_ [] . toHtml

numberList :: Html () -> Html ()
numberList = ol_ []

lazyImg :: String -> String -> Html ()
lazyImg url alt =
    details_ [onclick_ (pack $ "loadLazyImage(this,\"" ++ url ++ "\")")] $
        summary_ [] (toHtml alt)

haskellCode :: [String] -> Html ()
haskellCode codes =
    pre_ [] (code_ [class_ (pack "language-haskell")]
        (toHtml (unlines codes)))

wrongCode :: [String] -> Html ()
wrongCode codes =
    pre_ [] (code_ [class_ (pack "language-haskell no-text-colour")]
        (toHtml (unlines codes)))

qot :: String -> Html ()
qot = code_ [class_ (pack "qot")] . toHtml

tableContainer :: Html () -> Html ()
tableContainer = table_ []

tableHeader :: [Html ()] -> Html ()
tableHeader heads = tr_ []
    (foldl (>>) (toHtml empty) (map (th_ []) heads))

tableRow :: [Html ()] -> Html ()
tableRow dat = tr_ []
    (foldl (>>) (toHtml empty) (map (td_ []) dat))

problem :: Html () -> Html () -> Html ()
problem prob ans = li_ [] $ do
    prob
    br_ []
    ans

answer :: Html () -> Html ()
answer ans = details_ [] $ do
    summary_ [] "解答を表示"
    ans

hint :: Html () -> Html ()
hint hnt = details_ [] $ do
    summary_ [] "ヒントを表示"
    hnt
