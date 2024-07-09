module Chapters.Chapter4 (chapter4) where

import Chapter
import Component

chapter4 :: Chapter
chapter4 =
    beginChapter "基本的な式"
        [ beginChapterDescription $ do
            "この章では、Haskellで基本的な式を記述する方法を説明する。"
            "3章で開発環境を整えたところだが、この章のサンプルプログラムについては"; hlink "https://www.tryhaskell.org/" "TryHaskell"; "の使用をおすすめする。"
            "TryHaskellの使用方法は非常に簡単で、"; qot "λ"; "の隣にプログラムを入力してEnterキーを押すだけである。"

        , beginSection "四則演算"
            [ beginContent $ do
                "加算・減算は数学とまったく同じように記述できる。"
                "ただし、乗算は"; qot "×"; "や"; qot "・"; "ではなく、"; qot "*"; "を使う必要がある。"
                "そして、除算は"; qot "÷"; "ではなく"; qot "/"; "を使う。"
                "つまり、Haskellで四則演算は下記のように記述できる。"
                haskellCode
                    [ "1 + 2"
                    , "2 - 1"
                    , "2 * 4"
                    , "4 / 2"
                    ]
                "上の式を実行すると、結果はそれぞれ"
                haskellCode
                    [ "3"
                    , "1"
                    , "8"
                    , "2.0"
                    ]
                "となる。"
                "Haskellで除算を行うと、解が整数であるかどうかに関わらず結果は実数となる点に注意が必要である。"
                "結果として実数が出てきたということは、Haskellでは実数を扱うことができる。"
                haskellCode
                    [ "1.0 + 2.0"
                    , "2.3 - 0.3"
                    , "2.1 * 4.8"
                    , "4.5 / 1.5"
                    ]
                "これらの実行結果は以下である。"
                haskellCode
                    [ "3.0"
                    , "2.0"
                    , "10.08"
                    , "3.0"
                    ]
                "また、式は連結でき、"; qot "( )"; "を用いた計算の優先順位の指定も可能だ。"
                haskellCode
                    [ "1 + 2 + 3"
                    , "1 + 2 - 3 + 4"
                    , "1 + 2 - (3 + 4)"
                    ]
                "これらの実行結果は以下である。"
                haskellCode
                    [ "6"
                    , "4"
                    , "-4"
                    ]
            
            , beginSubsection "問題" $ do
                numberList $ do
                    problem
                        (do
                            "数式 "; latex "1 - 2 + \\Bigr (3 \\cdot \\frac{1.0}{25} \\Bigr )"; " をHaskellで記述せよ。"
                        )
                        (answer $
                            haskellCode
                                [ "1 - 2 + (3 * 1.0 / 25)"
                                ]
                        )
            ]
        
        , beginSection "真偽値"
            [ beginContent $ do
                "Haskellには、"; qot "True"; "・"; qot "False"; "という値がある。"
                "文字通り、"; qot "True"; "は真、"; qot "False"; "は偽を表す。"
                "これは次に紹介する等式・不等式の結果であったり、処理が成功したか・失敗したかを表すことができる。"
                "また、オン・オフの2択をとる設定は真偽値で実現できる。"
            ]
        
        , beginSection "等式"
            [ beginContent $ do
                "Haskellには等式が存在する。"
                "ただし、数学と同じ "; latex "a = b"; " のような形式ではなく、"; qot "a == b"; "のような形式をとらなければならない。"
                qot "="; "の代わりに、"; qot "=="; "を用いるのである。"
                "そして、Haskellの等式を実行した結果は真偽値である。"
                "つまり、等式が成り立てば"; qot "True"; "、成り立たなければ"; qot "False"; "となる。"
                "例えば、"
                haskellCode
                    [ "1 == 1"
                    , "1 == 1.0"
                    , "2 == 3"
                    , "2 == 2.1"
                    ]
                "これらを実行すると、結果はそれぞれ"
                haskellCode
                    [ "True"
                    , "True"
                    , "False"
                    , "False"
                    ]
                "となる。"
                "また、数式 "; latex "a \\neq b"; " はHaskellで"; qot "a /= b"; "と表記できる。"
                qot "≠"; "の代わりに、"; qot "/=" ; "を用いる。"
                "例えば、"
                haskellCode
                    [ "1 /= 1"
                    , "1 /= 1.0"
                    , "2 /= 3"
                    , "2 /= 2.1"
                    ]
                "の実行結果は以下になる。"
                haskellCode
                    [ "False"
                    , "False"
                    , "True"
                    , "True"
                    ]
                "加えて、"; qot "=="; "や"; qot "/="; "は真偽値の比較にも使用できる。"
                "例えば、"
                haskellCode
                    [ "True == True"
                    , "False == True"
                    , "True /= False"
                    , "False /= False"
                    ]
                "を実行すると、次の結果を得る。"
                haskellCode
                    [ "True"
                    , "False"
                    , "True"
                    , "False"
                    ]
            ]

        , beginSection "不等式"
            [ beginContent $ do
                "2つの不等式 "; latex "a > b"; " と "; latex "a < b"; " は、Haskellでそのまま"; qot "a > b"; "、"; qot "a < b"; "と表記できる。"
                "ただし、"; latex "a \\geq b"; " と "; latex "a \\leq b"; " は、"; qot ">="; "と"; qot "<="; "を用いて、"; qot "a >= b"; "、"; qot "a <= b"; "である。"
                qot "=>"; "や"; qot "=<"; "はよくある間違いである。"
                "不等号が先であると覚えよう。"
                "実行結果は、もちろん真偽値である。"
                haskellCode
                    [ "1 > 2"
                    , "1 >= 1"
                    , "2 < 1"
                    , "1 <= 2"
                    ]
                "実行結果"
                haskellCode
                    [ "False"
                    , "True"
                    , "False"
                    , "True"
                    ]
                "また、真偽値の大小はどう考えられるだろうか。"
                "コンピュータではオンを"; qot "1"; "、オフを"; qot "0"; "で表現する。"
                "オン・オフは真偽値で表現できるから、"; qot "True"; "は"; qot "1"; "、"; qot "False"; "は"; qot "0"; "と表現できる。"
                "したがって、以下が成り立つ (= 実行結果が"; qot "True"; "になる)。"
                haskellCode  
                    [ "True > False"
                    , "True >= False"
                    , "False < True"
                    , "False <= True"
                    ]
                "ただし、Haskellでは数と真偽値を比較することはできない。"
                "つまり、以下のようなプログラムはエラーとなる。"
                wrongCode
                    [ "True > 0"
                    , "True <= 0"
                    , "1 == True"
                    , "1.0 /= False"
                    ]
            ]

        , beginSection "論理記号"
            [ beginContent $ do
                "数学における "; latex "a \\wedge b"; " と "; latex "a \\vee b"; " はそれぞれ"; qot "a && b"; "と"; qot "a || b"; "である。"
                "また、"; latex "\\neg a"; " は"; qot "not a"; "と表記できる。"
                "ここで、"; qot "a"; "、"; qot "b"; "は真偽値でなければならない。"
                "例えば、"
                haskellCode
                    [ "True && True"
                    , "True && False"
                    , "True || False"
                    , "False || False"
                    , "not False"
                    , "not True"
                    ]
                "これらの実行結果は"
                haskellCode
                    [ "True"
                    , "False"
                    , "True"
                    , "False"
                    , "True"
                    , "False"
                    ]
                "である。"
                "ここで、等式と不等式を考えよう。"
                "等式と不等式の実行結果は真偽値であるから、これらの論理記号で使用できる。"
                haskellCode
                    [ "1 == 1 && 2 == 2"
                    , "1 == 1 && 1 > 2"
                    , "1 + 1 == 2 || 1 + 1 == 1"
                    , "1 + 1 > 2 || 1 + 1 > 3"
                    , "not (1 + 1 == 1)"
                    , "not (1 + 1 == 2)"
                    ]
                "実行結果"
                haskellCode
                    [ "True"
                    , "False"
                    , "True"
                    , "False"
                    , "True"
                    , "False"
                    ]
                "ここで、"; qot "not (1 + 1 == 1)"; "の"; qot "( )"; "を外すと、どうなるだろうか。"
                "結果を言うと、"; qot "not 1 + 1 == 1"; "は実行できない。"
                "なぜなら、Haskellはこの式を"; qot "((not 1) + 1) == 1"; "として解釈してしまうからである。"
                qot "not 1"; "に注目すると、真偽値ではない"; qot "1"; "に対して"; qot "not"; "を使用していることがわかる。"

            , beginSubsection "問題" $ do
                numberList $ do
                    problem
                        (do
                            "数式 "; latex "a \\ne b"; " を、"; qot "not"; "を用いてHaskellで記述せよ。"
                        )
                        (answer $
                            haskellCode
                                [ "not (a == b)"
                                ]
                        )
                    problem
                        (do
                            "以下の2つの式を実行すると、結果はそれぞれ"; qot "False"; "、"; qot "True"; "となる。"
                            "結果が異なる理由を、Haskellが"; qot "not"; "を解釈する方法に注目して説明せよ。"
                            haskellCode
                                [ "not (True >= False)"
                                , "not True >= False"
                                ]
                        )
                        (answer $ do
                            "前者の"; qot "not"; "が否定するのは"; qot "True >= False"; "であるから、"; qot "True < False"; "が得られる。"
                            "後者の"; qot "not"; "が否定するのは"; qot "True"; "のみであるから、"; qot "False >= False"; "が得られる。"
                            "前者と後者で"; qot "not"; "が及ぶ範囲が違い、式が異なるため結果も異なる。"
                        )
            ]
        
        , beginSection "\\(a < b < c\\) のような式"
            [beginContent $ do
                "Haskellでは、"; latex "a < b < c"; " や "; latex "a = b = c"; " のような式を以下のように書くことはできない。"
                wrongCode
                    [ "a < b < c"
                    , "a == b == c"
                    ]
                "これはHaskellの仕様によるものである。"
                "代わりに、"; qot "&&"; "や"; qot "||"; "といった論理記号を用いて表記する必要がある。"
                "例えば、例に挙げた2つの数式を正しくHaskellで記述すると以下のようになる。"
                haskellCode
                    [ "a < b && b < c"
                    , "a == b && b == c"
                    ]
            ]
        ]
