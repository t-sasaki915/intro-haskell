module Chapter4 (chapter4) where

import Chapters
import Component

chapter4 :: Chapter
chapter4 =
    beginChapter "基本的な式"
        [ beginChapterDescription $ do
            txt "この章では、Haskellで基本的な式を記述する方法を説明する。"
            txt "3章で開発環境を整えたところだが、この章のサンプルプログラムについては"; hlink "https://www.tryhaskell.org/" "TryHaskell"; txt "の使用をおすすめする。"
            txt "TryHaskellの使用方法は非常に簡単で、"; qot "λ"; txt "の隣にプログラムを入力してEnterキーを押すだけである。"

        , beginSection "四則演算"
            [ beginContent $ do
                txt "加算・減算は数学とまったく同じように記述できる。"
                txt "ただし、乗算は"; qot "×"; txt "や"; qot "・"; txt "ではなく、"; qot "*"; txt "を使う必要がある。"
                txt "そして、除算は"; qot "÷"; txt "ではなく"; qot "/"; txt "を使う。"
                txt "つまり、Haskellで四則演算は下記のように記述できる。"
                haskellCode
                    [ "1 + 2"
                    , "2 - 1"
                    , "2 * 4"
                    , "4 / 2"
                    ]
                txt "上の式を実行すると、結果はそれぞれ"
                haskellCode
                    [ "3"
                    , "1"
                    , "8"
                    , "2.0"
                    ]
                txt "となる。"
                txt "Haskellで除算を行うと、解が整数であるかどうかに関わらず結果は実数となる点に注意が必要である。"
                txt "結果として実数が出てきたということは、Haskellでは実数を扱うことができる。"
                haskellCode
                    [ "1.0 + 2.0"
                    , "2.3 - 0.3"
                    , "2.1 * 4.8"
                    , "4.5 / 1.5"
                    ]
                txt "これらの実行結果は以下である。"
                haskellCode
                    [ "3.0"
                    , "2.0"
                    , "10.08"
                    , "3.0"
                    ]
                txt "また、式は連結でき、"; qot "( )"; txt "を用いた計算の優先順位の指定も可能だ。"
                haskellCode
                    [ "1 + 2 + 3"
                    , "1 + 2 - 3 + 4"
                    , "1 + 2 - (3 + 4)"
                    ]
                txt "これらの実行結果は以下である。"
                haskellCode
                    [ "6"
                    , "4"
                    , "-4"
                    ]
            
            , beginSubsection "問題" $ do
                numberList $ do
                    problem
                        (do
                            txt "数式 "; latex "1 - 2 + \\Bigr (3 \\cdot \\frac{1.0}{25} \\Bigr )"; txt " をHaskellで記述せよ。"
                        )
                        (answer $
                            haskellCode
                                [ "1 - 2 + (3 * 1.0 / 25)"
                                ]
                        )
            ]
        
        , beginSection "真偽値"
            [ beginContent $ do
                txt "Haskellには、"; qot "True"; txt "・"; qot "False"; txt "という値がある。"
                txt "文字通り、"; qot "True"; txt "は真、"; qot "False"; txt "は偽を表す。"
                txt "これは次に紹介する等式・不等式の結果であったり、処理が成功したか・失敗したかを表すことができる。"
                txt "また、オン・オフの2択をとる設定は真偽値で実現できる。"
            ]
        
        , beginSection "等式"
            [ beginContent $ do
                txt "Haskellには等式が存在する。"
                txt "ただし、数学と同じ "; latex "a = b"; txt " のような形式ではなく、"; qot "a == b"; txt "のような形式をとらなければならない。"
                qot "="; txt "の代わりに、"; qot "=="; txt "を用いるのである。"
                txt "そして、Haskellの等式を実行した結果は真偽値である。"
                txt "つまり、等式が成り立てば"; qot "True"; txt "、成り立たなければ"; qot "False"; txt "となる。"
                txt "例えば、"
                haskellCode
                    [ "1 == 1"
                    , "1 == 1.0"
                    , "2 == 3"
                    , "2 == 2.1"
                    ]
                txt "これらを実行すると、結果はそれぞれ"
                haskellCode
                    [ "True"
                    , "True"
                    , "False"
                    , "False"
                    ]
                txt "となる。"
                txt "また、数式 "; latex "a \\neq b"; txt " はHaskellで"; qot "a /= b"; txt "と表記できる。"
                qot "≠"; txt "の代わりに、"; qot "/=" ; txt "を用いる。"
                txt "例えば、"
                haskellCode
                    [ "1 /= 1"
                    , "1 /= 1.0"
                    , "2 /= 3"
                    , "2 /= 2.1"
                    ]
                txt "の実行結果は以下になる。"
                haskellCode
                    [ "False"
                    , "False"
                    , "True"
                    , "True"
                    ]
                txt "加えて、"; qot "=="; txt "や"; qot "/="; txt "は真偽値の比較にも使用できる。"
                txt "例えば、"
                haskellCode
                    [ "True == True"
                    , "False == True"
                    , "True /= False"
                    , "False /= False"
                    ]
                txt "を実行すると、次の結果を得る。"
                haskellCode
                    [ "True"
                    , "False"
                    , "True"
                    , "False"
                    ]
            ]

        , beginSection "不等式"
            [ beginContent $ do
                txt "2つの不等式 "; latex "a > b"; txt " と "; latex "a < b"; txt " は、Haskellでそのまま"; qot "a > b"; txt "、"; qot "a < b"; txt "と表記できる。"
                txt "ただし、"; latex "a \\geq b"; txt " と "; latex "a \\leq b"; txt " は、"; qot ">="; txt "と"; qot "<="; txt "を用いて、"; qot "a >= b"; txt "、"; qot "a <= b"; txt "である。"
                qot "=>"; txt "や"; qot "=<"; txt "はよくある間違いである。"
                txt "不等号が先であると覚えよう。"
                txt "実行結果は、もちろん真偽値である。"
                haskellCode
                    [ "1 > 2"
                    , "1 >= 1"
                    , "2 < 1"
                    , "1 <= 2"
                    ]
                txt "実行結果"
                haskellCode
                    [ "False"
                    , "True"
                    , "False"
                    , "True"
                    ]
                txt "また、真偽値の大小はどう考えられるだろうか。"
                txt "コンピュータではオンを"; qot "1"; txt "、オフを"; qot "0"; txt "で表現する。"
                txt "オン・オフは真偽値で表現できるから、"; qot "True"; txt "は"; qot "1"; txt "、"; qot "False"; txt "は"; qot "0"; txt "と表現できる。"
                txt "したがって、以下が成り立つ (= 実行結果が"; qot "True"; txt "になる)。"
                haskellCode  
                    [ "True > False"
                    , "True >= False"
                    , "False < True"
                    , "False <= True"
                    ]
                txt "ただし、Haskellでは数と真偽値を比較することはできない。"
                txt "つまり、以下のようなプログラムはエラーとなる。"
                wrongCode
                    [ "True > 0"
                    , "True <= 0"
                    , "1 == True"
                    , "1.0 /= False"
                    ]
            ]

        , beginSection "論理記号"
            [ beginContent $ do
                txt "数学における "; latex "a \\wedge b"; txt " と "; latex "a \\vee b"; txt " はそれぞれ"; qot "a && b"; txt "と"; qot "a || b"; txt "である。"
                txt "また、"; latex "\\neg a"; txt " は"; qot "not a"; txt "と表記できる。"
                txt "ここで、"; qot "a"; txt "、"; qot "b"; txt "は真偽値でなければならない。"
                txt "例えば、"
                haskellCode
                    [ "True && True"
                    , "True && False"
                    , "True || False"
                    , "False || False"
                    , "not False"
                    , "not True"
                    ]
                txt "これらの実行結果は"
                haskellCode
                    [ "True"
                    , "False"
                    , "True"
                    , "False"
                    , "True"
                    , "False"
                    ]
                txt "である。"
                txt "ここで、等式と不等式を考えよう。"
                txt "等式と不等式の実行結果は真偽値であるから、これらの論理記号で使用できる。"
                haskellCode
                    [ "1 == 1 && 2 == 2"
                    , "1 == 1 && 1 > 2"
                    , "1 + 1 == 2 || 1 + 1 == 1"
                    , "1 + 1 > 2 || 1 + 1 > 3"
                    , "not (1 + 1 == 1)"
                    , "not (1 + 1 == 2)"
                    ]
                txt "実行結果"
                haskellCode
                    [ "True"
                    , "False"
                    , "True"
                    , "False"
                    , "True"
                    , "False"
                    ]
                txt "ここで、"; qot "not (1 + 1 == 1)"; txt "の"; qot "( )"; txt "を外すと、どうなるだろうか。"
                txt "結果を言うと、"; qot "not 1 + 1 == 1"; txt "は実行できない。"
                txt "なぜなら、Haskellはこの式を"; qot "((not 1) + 1) == 1"; txt "として解釈してしまうからである。"
                qot "not 1"; txt "に注目すると、真偽値ではない"; qot "1"; txt "に対して"; qot "not"; txt "を使用していることがわかる。"

            , beginSubsection "問題" $ do
                numberList $ do
                    problem
                        (do
                            txt "数式 "; latex "a \\ne b"; txt " を、"; qot "not"; txt "を用いてHaskellで記述せよ。"
                        )
                        (answer $
                            haskellCode
                                [ "not (a == b)"
                                ]
                        )
                    problem
                        (do
                            txt "以下の2つの式を実行すると、結果はそれぞれ"; qot "False"; txt "、"; qot "True"; txt "となる。"
                            txt "結果が異なる理由を、Haskellが"; qot "not"; txt "を解釈する方法に注目して説明せよ。"
                            haskellCode
                                [ "not (True >= False)"
                                , "not True >= False"
                                ]
                        )
                        (answer $ do
                            txt "前者の"; qot "not"; txt "が否定するのは"; qot "True >= False"; qot "であるから、"; qot "True < False"; txt "が得られる。"
                            txt "後者の"; qot "not"; txt "が否定するのは"; qot "True"; txt "のみであるから、"; qot "False >= False"; txt "が得られる。"
                            txt "前者と後者で"; qot "not"; txt "が及ぶ範囲が違い、式が異なるため結果も異なる。"
                        )
            ]
        
        , beginSection "\\(a < b < c\\) のような式"
            [beginContent $ do
                txt "Haskellでは、"; latex "a < b < c"; txt " や "; latex "a = b = c"; txt " のような式を以下のように書くことはできない。"
                wrongCode
                    [ "a < b < c"
                    , "a == b == c"
                    ]
                txt "これはHaskellの仕様によるものである。"
                txt "代わりに、"; qot "&&"; txt "や"; qot "||"; txt "といった論理記号を用いて表記する必要がある。"
                txt "例えば、例に挙げた2つの数式を正しくHaskellで記述すると以下のようになる。"
                haskellCode
                    [ "a < b && b < c"
                    , "a == b && b == c"
                    ]
            ]
        ]
