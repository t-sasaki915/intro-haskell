module Chapters.Chapter6 (chapter6) where

import Chapter
import Component

chapter6 :: Chapter
chapter6 =
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

                    problem
                        "プレイヤーの名前を値として持つ変数の名前を考えよ。"
                        (answer $
                            qot "playerName"
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

        , beginSection "関数"
            [ beginContent $ do
                "Haskellの関数は、すべてラムダ式であると考えられる。"
                "ラムダ式を値として持つ変数を定義し、関数として使用する。"
                haskellCode
                    [ "plusOne = \\a -> a + 1"
                    ]
                "しかし、これでは無駄が多い。"
                "そこで、Haskellは関数の定義のために特殊な記法を用意している。"
                haskellCode
                    [ "plusOne a = a + 1"
                    ]
                "関数名"; qot "plusOne"; "の隣にラムダ式の変数名"; qot "a"; "を置き、そのあとに式を直接書くことができる。"
                "これは、ラムダ式の組み合わせでも同様である。"
                haskellCode
                    [ "sum = \\a -> \\b -> a + b"
                    ]
                "これは、5章で説明したラムダ式の短縮を用いて"
                haskellCode
                    [ "sum = \\a b -> a + b"
                    ]
                "となり、さらにラムダ式の変数"; qot "a"; "と"; qot "b"; "を関数名の隣に持ってきて"
                haskellCode
                    [ "sum a b = a + b"
                    ]
                "である。"

            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "以下のラムダ式を用いて、関数"; qot "square"; "をHaskellで定義せよ。"
                            "ただし、できるだけシンプルな記法を用いよ。"
                            latexBlock "\\lambda n. n \\cdot n"
                        )
                        (answer $
                            haskellCode
                                [ "square n = n * n"
                                ]
                        )

                    problem
                        (do
                            "以下のラムダ式を用いて、関数"; qot "sum3"; "をHaskellで定義せよ。"
                            "ただし、できるだけシンプルな記法を用いよ。"
                            latexBlock "\\lambda a. \\lambda b. \\lambda c. a + b + c"
                        )
                        (answer $
                            haskellCode
                                [ "sum3 a b c = a + b + c"
                                ]
                        )

                    problem
                        (do
                            "以下の関数をHaskellで再定義せよ。"
                            latexBlock "f(a, b) = a - b"
                        )
                        (do
                            hint $ do
                                "与えられた関数をHaskellで再定義するためには、まずカリー化を行う必要がある。"
                                latexBlock "f(a) = \\lambda b. a - b"

                            answer $
                                haskellCode
                                    [ "f a b = a - b"
                                    ]
                        )
            ]

        , beginSection "関数の使用"
            [ beginContent $ do
                "Haskellの関数は、ラムダ式にラベルをつけたものであるから、関数の使用方法はラムダ式と同じである。"
                "つまり、"
                haskellCode
                    [ "ラムダ式 値"
                    ]
                "のラムダ式の部分を関数名に置き換え、"
                haskellCode
                    [ "関数名 値"
                    ]
                "である。"
                "例えば、関数"; qot "plusOne a = a + 1"; "は以下のように使用できる。"
                haskellCode
                    [ "plusOne 1"
                    , "plusOne 3"
                    , "plusOne (1 + 3)"
                    ]
                "実行結果"
                haskellCode
                    [ "2"
                    , "4"
                    , "5"
                    ]
                "そして、関数"; qot "sum a b = a + b"; "のような、内部的にカリー化されている関数は、値を複数取る関数であるかのように使用できる。"
                "つまり、"
                haskellCode
                    [ "sum 1 2"
                    , "sum 3 4"
                    , "sum (1 + 2) (2 + 2)"
                    ]
                "を実行すると"
                haskellCode
                    [ "3"
                    , "7"
                    , "7"
                    ]
                "になる。"
                "もちろん、カリー化されていることを尊重し、"; qot "(sum x) y"; "と書いてもよい。"

            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "以下の関数適用の式を、シンプルに書き直せ。"
                            haskellCode
                                [ "((((f 1) 2) 3) 4)"
                                ]
                        )
                        (answer $
                            haskellCode
                                [ "f 1 2 3 4"
                                ]
                        )
                    
                    problem
                        (do
                            "以下の関数適用の式を、カリー化されていることがわかりやすい方法で書き直せ。"
                            haskellCode
                                [ "g 1 2 3"
                                ]
                        )
                        (answer $
                            haskellCode
                                [ "(((g 1) 2) 3)"
                                ]
                        )

                    problem
                        (do
                            "以下の関数を用いて、与えられた値に2を加える関数"; qot "plusTwo"; "を定義せよ。"
                            haskellCode
                                [ "sum a b = a + b"
                                ]
                        )
                        (do
                            hint $ do
                                "関数の部分適用を用いて、"; qot "a"; "が"; qot "2"; "に固定された"; qot "sum"; "関数を生成できる。"
                                haskellCode
                                    [ "sum 2"
                                    ]
                            
                            answer $
                                haskellCode
                                    [ "plusTwo = sum 2"
                                    ]
                        )
            ]
        ]
