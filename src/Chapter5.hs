{-# LANGUAGE OverloadedStrings #-}

module Chapter5 (chapter5) where

import Chapters
import Component

chapter5 :: Chapter
chapter5 =
    beginChapter "変数と関数"
        [ beginChapterDescription $ do
            "この章では、Haskellで変数・関数を定義し、使用する方法を説明する。"
            "また、初学者のために変数・関数の命名方法も説明する。"
        
        , beginSection "変数について"
            [ beginContent $ do
                "まず、数学の変数を復習しよう。"
                "数学において変数とは、未知の値にラベルをつけて、後で式に代入できるようにするものであった。"
                "例えば、"; latex "f(x) = x + 1"; " の "; latex "x"; " は未知の数であるが、具体的な数を代入すればすぐに計算が可能だ。"
                "そして、既に分かっている数・式にラベルをつけて、式を簡潔にすることができた。"
                "例えば、これは物理学の式だが、"; latex "y = A\\sin{\\omega t}"; " は変数 "; latex "\\omega = 2\\pi f"; " の定義によって簡潔に記述される。"
                "Haskellにおける変数も、数学とほとんど同じである。"
                "後で代入するためのラベルに使えるし、既知の値・式をまとめることもできる。"
            ]

        , beginSection "変数の定義"
            [ beginContent $ do
                "次に、変数を定義してみよう。"
                "Haskellでは、変数は以下のフォーマットで定義される。"
                haskellCode
                    [ "変数名 = 値または式"
                    ]
                "例えば、"; qot "5"; "という値を持つ変数"; qot "x"; "を定義してみよう。"
                haskellCode
                    [ "x = 5"
                    ]
                "また、"; qot "1 + 2"; "という式の計算結果を持つ変数"; qot "y"; "を定義してみよう。"
                haskellCode
                    [ "y = 1 + 2"
                    ]
                "ここで、変数の名前は重複してはいけない。"
                "例えば、以下のプログラムはエラーである。"
                wrongCode
                    [ "z = 1"
                    , "z = 2"
                    ]
                "C言語やPythonなどを経験している読者は、このプログラムは変数の再代入に見えるかもしれない。"
                "しかし、Haskellでは、一度定義した変数の値は変更できない。"
            
            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "値として"; qot "True"; "を持つ変数"; qot "a"; "を定義せよ。"
                        )
                        (answer $
                            haskellCode
                                [ "a = True"
                                ]
                        )
                    problem
                        (do
                            qot "n - m"; "の計算結果を値として持つ変数"; qot "abc"; "を定義せよ。"
                        )
                        (answer $
                            haskellCode
                                [ "abc = n - m"
                                ]
                        )
                    problem
                        (do
                            qot "a + b"; "が"; qot "c"; "と等しいかどうかを値をして持つ変数"; qot "x"; "を定義せよ。"
                        )
                        (do
                            hint $ do
                                "「"; qot "a + b"; "が"; qot "c"; "と等しいかどうか」は、"
                                haskellCode
                                    [ "a + b == c"
                                    ]
                                "で表される。"
                            answer $
                                haskellCode
                                    [ "x = a + b == c"
                                    ]
                        )
            ]

        , beginSection "変数の命名"
            [ beginContent $ do
                "数学では、変数名は "; latex "x"; " "; latex "y"; " "; latex "S"; " など原則1文字であるが、プログラミングでは、変数は英語でシンプルかつわかりやすく命名する必要がある。"
                "そして、Haskellでは、変数の命名にcamelCaseというフォーマットを用いる。"
                "camelCaseとは、1文字目を小文字にし、それ以降の単語の頭文字を大文字にして単語を並べる方法である。"
                "例えば、"; qot "user_data"; "をcamelCaseで書き直すと、"; qot "userData"; "である。"
                "また、"; qot "ProcessExitCode"; "はcamelCaseで"; qot "processExitCode"; "である。"
                "そして、変数名にスペースを含んではいけない。"
                "加えて、変数名をシンプルかつ分かりやすくするために、単語の選び方も重要である。"
                "変数の内容を説明するための十分最低限な単語を選ぶ必要がある。"
                "例えば、「ユーザーの年齢を値として持つ変数」を命名するとき、そのまま英語にしてしまうと"; qot "aVariableWhoseContentIsTheAgeOfTheUser"; "であり、長すぎである。"
                "この時、この変数の最も重要な情報は「ユーザー」の「年齢」、すなわち"; qot "user"; "と"; qot "age"; "の2単語である。"
                "よって、この2単語から構成される変数名"; qot "userAge"; "が最適であろう。"
            
            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            qot "PROGRAM_VERSION"; "をcamelCaseで書き直せ。"
                        )
                        (answer $
                            qot "programVersion"
                        )
                    
                    problem
                        (do
                            qot "display text colour"; "をcamelCaseで書き直せ。"
                        )
                        (answer $
                            qot "displayTextColour"
                        )
            ]
        
        , beginSection "変数の使用"
            [ beginContent $ do
                "Haskellで定義した変数を使用する方法は、単純に式の項として変数名を書くだけである。"
                "例えば、数を値として持つ変数"; qot "x"; "、"; qot "y"; "が定義されているとする。"
                "このとき、"; qot "x"; "の値と"; qot "y"; "の値の加算は"
                haskellCode
                    [ "x + y"
                    ] 
                "である。"
                "また、変数と具体的な値を用いた式も可能である。"
                haskellCode
                    [ "x + 1"
                    , "y + 1"
                    , "x + y + 1 + 2"
                    ]
            
            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "変数 "; latex "\\omega = 2\\pi f"; " をHaskellで定義せよ。"
                            "ただし、"; latex "\\omega"; " は"; qot "omega"; "、"; latex "\\pi"; " は"; qot "pi"; "を用いよ。"
                        )
                        (answer $
                            haskellCode
                                [ "omega = 2 * pi * f"
                                ]
                        )
            ]
        ]
