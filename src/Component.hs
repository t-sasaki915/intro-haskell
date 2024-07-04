{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Component where

import Data.Text (pack)
import Lucid

txt :: String -> Html ()
txt = toHtml

hlink :: String -> String -> Html ()
hlink link = a_ [href_ (pack link)] . toHtml

latex :: String -> Html ()
latex equ = toHtml $ "\\(" ++ equ ++ "\\)"

dotList :: Html () -> Html ()
dotList = ul_ []

listItem :: String -> Html ()
listItem = li_ [] . toHtml
