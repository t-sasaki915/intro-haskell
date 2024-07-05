{-# LANGUAGE OverloadedStrings #-}

module Chapter6 (chapter6) where

import Chapters
import Component

chapter6 :: Chapter
chapter6 =
    beginChapter "Haskell型システム入門"
        [ beginChapterDescription $ do
            "この章では、Haskellの型システムの基本を紹介する。"
            "特に、既存の型について説明を行う。"
        
        , beginSection "型とは"
            [ beginContent $ do
                "多くのプログラミング言語には、型というものが存在する。"
                "型をシンプルに説明すると、値の種類であるといえる。"
                "これまで、整数と実数、真偽値という値が登場してきた。"
                "これらは、それぞれ"; qot "Int"; "、"; qot "Double"; "、"; qot "Bool"; "という型を持つ。"
                "逆に言えば、値が"; qot "Int"; "という型を持っていたなら、その値は整数である。"
                "そして、値が"; qot "Bool"; "という型を持っていたなら、その値は真偽値である。"
                "このように、型が値の種類を保証するのである。"
            ]
        
        , beginSection "基本型"
            [ beginContent $ do
                "Haskellで定義されている、基本的な型を一部紹介する。"
                tableContainer $ do
                    tableHeader ["型", "内容", "具体例"]
                    tableRow [qot "Int", "整数", qot "0" >> " " >> qot "5" >> " " >> qot "1234"]
                    tableRow [qot "Double", "実数", qot "0.0" >> " " >> qot "2.4" >> " " >> qot "123.4"]
                    tableRow [qot "Bool", "真偽値", qot "True" >> " " >> qot "False"]
                    tableRow [qot "Char", "文字", qot "'a'" >> " " >> qot "'b'" >> " " >> qot "'A'"]
                    tableRow [qot "String", "文字列", qot "\"a\"" >> " " >> qot "\"Hello\"" >> " " >> qot "\"あああ\""]
            
            , beginSubsection "Integer・Floatについて" $ do
                "Haskellのプログラム内に、"; qot "Integer"; "や"; qot "Float"; "といった型を見るかもしれない。"
                qot "Integer"; "は"; qot "Int"; "とほとんど同じであり、"; qot "Float"; "は"; qot "Double"; "とほとんど同じである。"
                "ただし、全く同じではない。"
                "例えば、"; qot "Int"; "が扱える最大の整数は"; qot "9223372036854775807"; "だが、"; qot "Integer"; "にはそのような上限がない。"
                "また、"; qot "Double"; "は"; qot "Float"; "の2倍の精度で実数を計算できる。"
                "(2倍の精度だから"; qot "Double"; "という名前なのである)。"
                "なお、実数の精度とは、無限小数を有限なメモリの中でどこまで表現するかの程度である。"
                "例えば、"; qot "1.111..."; "という無限小数を、"; qot "1.111"; "として扱う型より、"; qot "1.111111"; "として扱える型のほうが高精度である。"
            ]
        ]
