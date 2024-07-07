{-# LANGUAGE OverloadedStrings #-}

module Chapters.Chapter5 (chapter5) where

import Chapter
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
        
        , beginSection "関数について"
            [ beginContent $ do
                "数学において関数とは値と値の関係、片方の値が決まればもう片方も一意に決まるような関係であった。"
                "Haskellにおける関数も全く同じである。"
            ]
        
        , beginSection "関数の使用"
            [ beginContent $ do
                "関数を定義する方法を説明する前に、Haskellで関数を使う方法を説明する。"
                "関数に値を適用する式は、以下のフォーマットである。"
                haskellCode
                    [ "関数名 値"
                    ]
                "関数名を最初に置き、その後に関数に適用する値を配置する。"
                "例えば、Haskellには数の平方根を求める関数"; qot "sqrt"; "がある。"
                "これに2を適用して、2の平方根を求めるプログラムは"
                haskellCode
                    [ "sqrt 2"
                    ]
                "であり、これをTryHaskellなどで実行すると"
                haskellCode
                    [ "1.4142135623730951"
                    ]
                "が得られる。"
                "ところで、4章で紹介した"; qot "not"; "も関数である。"
            ]

        , beginSection "ラムダ計算"
            [ beginContent $ do
                "次の説明を行うにあたって、まずラムダ計算について簡単に紹介する。"
                "ラムダ計算とは、数学における関数の記法の一つである。"
                "一般に、数学で関数を定義するときは "; latex "f(x) = x + 1"; " のように、"
                latexBlock "関数名(変数名) = 変数についての式"
                "といった形式で記述されるが、ラムダ記法で定義された関数は関数名を持たない。"
                "まず、ラムダ記法を紹介する。"
                latexBlock "λ変数名. 変数についての式"
                "先ほどの関数 "; latex "f"; " をラムダ記法で書き直すと、"
                latexBlock "\\lambda x. x + 1"
                "となる。"
                "関数名 "; latex "f"; " が失われた。"
                "また、ラムダ記法で記述された関数に値を適用する方法は、"
                latexBlock "(ラムダ記法の式) 値"
                "である。"
                "例えば、先ほどの関数 "; latex "f"; " をラムダ記法で書き直したものに"; qot "3"; "を適用する式は"
                latexBlock "(\\lambda x. x + 1) 3"
                "であり、この式の値は"; latex "3 + 1"; "、"; qot "4"; "になる。"

            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "関数 "; latex "f(x) = x^2 + 2x + 1"; " をラムダ記法で記述せよ。"
                        )
                        (answer $
                            latex "\\lambda x. x^2 + 2x + 1"
                        )
                    
                    problem
                        (do
                            "関数 "; latex "g(a) = f(a + 1)"; " をラムダ記法で記述せよ。"
                        )
                        (answer $
                            latex "\\lambda a. f(a + 1)"
                        )
            ]
        
        , beginSection "関数を返す関数"
            [ beginContent $ do
                "数学では、関数が受け取れる値は原則一つであり、"; latex "f(x, y) = x + y"; " や "; latex "f(1, 2)"; " といった記述は一般的ではない。"
                "では、"; latex "f(x, y) = x + y"; " のように値を複数受け取りたい場合はどうすればよいか。"
                "このような関数は、先ほど紹介したラムダ計算で実現できる。"
                "例えば、先ほどの "; latex "f(x, y)"; " は、ラムダ計算を用いて以下のように記述できる。"
                latexBlock "f(x) = \\lambda y. x + y" 
                "関数 "; latex "f"; " は、値を1つだけ受け取る一般的な関数になった。"
                "そして、関数 "; latex "f"; " の値が、ラムダ式、つまり関数になっていることに気づいただろうか。"
                "これこそが、「関数を返す関数」である。"
                "例えば、"; latex "f"; " に具体的な数"; qot "4"; "を適用してみよう。"
                latexBlock "f(4) = \\lambda y. 4 + y"
                "さらに、"; latex "f(4)"; " に具体的な数"; qot "3"; "を適用してみよう。"
                latexBlock "\\begin{aligned}\\{f(4)\\} 3 &= (\\lambda y. 4 + y) 3 \\\\&= 4 + 3 \\\\&= 7\\end{aligned}"
                "このように、2度の値適用によって、解を得ることができた。"
                "これは値を2つ取る関数の実現といってよいだろう。"
                "複数の値を取れる関数そのものを実現するのではなく、関数を返す関数を用いてこれを実現することで、部分適用などといったHaskellの重要な概念の理解へとつながることになる。"
            ]
        
        , beginSection "複数の値をとるHaskellの関数"
            [ beginContent $ do
                "Haskellのみならず多くのプログラミング言語では、複数の値をとる関数の存在は必須である。"
                "例えば、Haskellには"; qot "plus"; "という、"; qot "+"; "の記号を用いずに加算を行う関数がある。"
                "先ほどの数学の例を参考に"; qot "plus"; "関数に"; qot "4"; "、"; qot "3"; "を適用してみよう。"
                haskellCode
                    [ "(plus 4) 3"
                    ]
                "このようになるのではないだろうか。"
                "しかし、プログラミングにおいて複数の値をとる関数は極めて一般的であり、このようにいちいち"; qot "( )"; "を付けることはプログラムを読みにくくする。"
                "よって、Haskellでは"; qot "( )"; "を省略し、"
                haskellCode
                    [ "plus 4 3"
                    ]
                "と記述できる。"
                "これは、値を3つ以上とる関数でも同様である。"
                "例えば、3つの数をとり、それらの合計を求める"; qot "sum"; "関数の存在を仮定する。"
                "この関数で"; qot "1"; "、"; qot "2"; "、"; qot "3"; "の合計を求める方法は、まず"
                haskellCode
                    [ "((sum 1) 2) 3"
                    ]
                "そして、"; qot "( )"; "を省略して"
                haskellCode
                    [ "sum 1 2 3"
                    ]
                "である。"
            ]

        , beginSection "関数の部分適用"
            [ beginContent $ do
                "3つの数をとり、それらの合計を求める関数"; qot "sum"; "を仮定する。"
                "ここで、求められている数は3つだが、2つだけ適用するとどうなるだろうか。"
                haskellCode
                    [ "sum 1 2"
                    ]
                "一般的に、これはエラーであるが、Haskellは例外である。"
                "つまり、これは正しい式である。"
                "では、この式の値は何であろうか。"
                qot "sum 1 2"; "は、「1つの数をとり、"; qot "1"; "、"; qot "2"; "との合計を求める関数」となる。"
                qot "sum"; "が求める3つの数のうち、2つが具体化され、残ったのは値を1つとる関数である。"
                "このように、関数に不十分な量の値を適用し、結果として関数を得る行為を、関数の部分適用という。"
                "また、このことは先ほどの式"
                haskellCode
                    [ "(plus 3) 4"
                    ]
                "からわかることでもある。"
                qot "plus"; "は値を2つとる関数であるが、"; qot "plus 3"; "で部分適用し、「1つ値をとり、それに"; qot "3"; "を加える関数」を得る。"
                "次に、その関数に"; qot "4"; "を適用し、"; qot "7"; "を得ているのである。"
            ]
        ]
