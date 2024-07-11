module Chapters.Chapter5 (chapter5) where

import Chapter
import Component

chapter5 :: Chapter
chapter5 =
    beginChapter "ラムダ計算"
        [ beginChapterDescription $ do
            "ここで、1つ数学の分野であるラムダ計算を学ぼう。"
            "ラムダ計算は、Haskellのみならず多くのプログラミング言語における関数を理解するために非常に役に立つ。"
            "このテキストの前提知識である高校数学を逸脱している分野であるが、基礎を理解することは全く難しくないはずである。"
            "プログラミングの経験がある読者は、ラムダ式や無名関数という言葉に聞き馴染みがあるかもしれない。"
            "ラムダ計算とは、ラムダ式や無名関数そのものである。"
            "もともと数学の分野であったラムダ計算を、コンピューターサイエンスに応用したのである。"

        , beginSection "ラムダ計算とは"
            [ beginContent $ do
                "まず、ラムダ計算とは何であるか説明する。"
                "先ほど、ラムダ計算とは無名関数であると述べた通り、ラムダ計算は関数の新しい書き方である。"
                "また、ラムダ計算の記法、ラムダ記法で記述された関数のことをラムダ式と呼ぶ。"
                "ラムダ計算は、特にコンピューターサイエンスにおいて極めて重大な存在である。"
                "特にHaskellは、根幹にラムダ計算の考え方が組み込まれているから、避けて通ることはできない存在である。"
            ]

        , beginSection "ラムダ記法"
            [ beginContent $ do
                "ここに関数がある。"
                latexBlock "f(x) = x + 1"
                "慣れ親しんだ、普通の数学の関数である。"
                "これをラムダ記法で表すとどうなるだろうか。"
                latexBlock "\\lambda x. x + 1"
                "変わった点は多くあるが、最も注目すべきは関数名 "; latex "f"; " が失われている点である。"
                "このことから、ラムダ記法で記述された関数は名前を持たない。"
                "したがって、無名関数と呼ばれることがあるのだ。"
                "次に、この物理学の関数を考える。"
                latexBlock "y(t) = A \\sin{\\omega t}"
                "これをラムダ記法を用いて再現すると"
                latexBlock "\\lambda t. A \\sin{\\omega t}"
                "このようになる。"
                "難しく考える必要はなく、関数名を "; latex "\\lambda"; " に置き換え、"; qot "( )"; "を消して"; qot "="; "を"; qot "."; "にするだけだ。"
                "関数の内容に変更を加える必要はない。"
            
            , beginSubsection "値の適用" $ do
                "ラムダ式への値の適用は、以下のように行われる。"
                latexBlock "ラムダ式\\quad 値"
                "例えば、ラムダ式 "; latex "\\lambda x. x + 1"; " に値 "; latex "3"; " を適用する式は"
                latexBlock "(\\lambda x. x + 1) 3"
                "であり、その答えは"
                latexAlign
                    [ "(\\lambda x. x + 1) 3 &= 3 + 1"
                    , "&= 4"
                    ]
                "である。"

            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "以下の関数 "; latex "y"; " をラムダ記法で記せ。"
                            latexBlock "y(x) = ax^2 + bx + c"
                        )
                        (answer $
                            latexBlock "\\lambda x. ax^2 + bx + c"
                        )
                    
                    problem
                        (do
                            "以下の関数 "; latex "f"; " をラムダ記法で記せ。"
                            latexBlock "f(t) = \\lim_{n \\rightarrow \\infty} \\frac{t}{n}"
                        )
                        (answer $
                            latexBlock "\\lambda t. \\lim_{n \\rightarrow \\infty} \\frac{t}{n}"
                        )
                    
                    problem
                        (do
                            "以下の関数 "; latex "f"; " をラムダ記法で記せ。"
                            latexBlock "f(x) = g(x) + h(x)"
                        )
                        (answer $
                            latexBlock "\\lambda x. g(x) + h(x)"
                        )
                    
                    problem
                        (do
                            "以下のラムダ式に、値 "; latex "4"; " を代入せよ。"
                            latexBlock "\\lambda x. \\lim_{n \\rightarrow \\infty} \\frac{x}{n}"
                        )
                        (answer $
                            latexAlign
                                [ "\\Bigr (\\lambda x. \\lim_{n \\rightarrow \\infty} \\frac{x}{n} \\Bigr ) 4 &= \\lim_{n \\rightarrow \\infty} \\frac{4}{n} \\quad (ここまででもよい)"
                                , "&= 0"
                                ]
                        )
                    
                    problem
                        (do
                            "以下のラムダ式に、値 "; latex "a + 1"; " を代入せよ。"
                            latexBlock "\\lambda t. t - a"
                        )
                        (answer $
                            latexAlign
                                [ "(\\lambda t. t - a) (a + 1) &= (a + 1) - a \\quad (ここまででもよい)"
                                , "&= a - a + 1"
                                , "&= 1"
                                ]
                        )
            ]
        
        , beginSection "カリー化"
            [ beginContent $ do
                "2つの値の合計を求める関数 "; latex "sum"; " がある。"
                latexBlock "sum(a, b) = a + b"
                "この関数はラムダ式でどう表現できるだろうか。"
                "単純に考えると、このようになる。"
                latexBlock "\\lambda a, b. a + b"
                "しかし、ラムダ式がとれる値は常に1つであるから、この式は誤りである。"
                "正しいラムダ式は、以下である。"
                latexBlock "\\lambda a. \\lambda b. a + b"
                "複数の値をとる関数は、複数のラムダ式の組み合わせで表現できる。"
                "値 "; latex "a"; " を受け取り、「値 "; latex "b"; " を受け取り、"; latex "a + b"; " を行う関数」を返しているのである。"
                "このラムダ式を "; latex "sum"; " とおいて、値を適用してみる。"
                latexAlign
                    [ "sum &= \\lambda a. \\lambda b. a + b"
                    , "sum \\: 3 &= (\\lambda a. \\lambda b. a + b) 3"
                    , "&= \\lambda b. 3 + b"
                    ]
                "1度の値適用で、"; latex "a"; " が具体化された "; latex "b"; " についてのラムダ式"; latex "\\lambda b. 3 + b"; "が得られた。"
                "さらに、得られたラムダ式に値を適用してみる。"
                latexAlign
                    [ "(sum \\: 3) 4 &= (\\lambda b. 3 + b) 4"
                    , "&= 3 + 4"
                    , "&= 7"
                    ]
                "このように、2つの値を取り、合計を求める関数がラムダ式で実現された。"
                "上記で行ったような、複数の値を取る関数を、複数の「1つの値を取る関数」の組み合わせで表現することを、カリー化という。"
                "次に、以下の関数をカリー化してみよう。"
                latexBlock "product5(a, b, c, d, e) = a \\cdot b \\cdot c \\cdot d \\cdot e"
                "5つの値を取る関数は、5つのラムダ式を用いて表現できるはずである。"
                latexBlock "product5 = \\lambda a. \\lambda b. \\lambda c. \\lambda d. \\lambda e. a \\cdot b \\cdot c \\cdot d \\cdot e"
                "あるいは、"; latex "\\lambda a."; " を通常の関数に戻し、"
                latexBlock "product5(a) = \\lambda b. \\lambda c. \\lambda d. \\lambda e. a \\cdot b \\cdot c \\cdot d \\cdot e"
                "としても、この2つは等しい関数である。 "

            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "以下の関数 "; latex "f"; " をラムダ式で表現せよ。"
                            latexBlock "f(x, y) = x^y"
                        )
                        (answer $
                            latexBlock "\\lambda x. \\lambda y. x^y"
                        )

                    problem
                        (do
                            "以下の関数 "; latex "g"; " をラムダ式で表現せよ。"
                            latexBlock "g(a, b, c) = f(a, b + c)"
                        )
                        (answer $
                            latexBlock "\\lambda a. \\lambda b. \\lambda c. f(a, b + c)"
                        )

                    problem
                        (do
                            "カリー化を用いて、以下の関数 "; latex "f"; " を元に値を1つとる関数 "; latex "f'"; " を定義せよ。"
                            latexBlock "f(a, b) = a \\cdot b"
                        )
                        (answer $
                            latexBlock "f'(a) = \\lambda b. a \\cdot b"
                        )

                    problem
                        (do
                            "以下のカリー化された関数 "; latex "g'"; " を元に、値を2つとる関数 "; latex "g"; " を定義せよ。"
                            latexBlock "g'(x) = \\lambda y. \\lambda z. x + y + z"
                        )
                        (answer $
                            latexBlock "g(x, y) = \\lambda z. x + y + z"
                        )
            ]
        ]
