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
        
        , beginSection "部分適用"
            [ beginContent $ do
                "前提を何も示さずにカリー化を説明したが、カリー化は何の役に立つのだろうか。"
                "ここで、以下の2つの関数を考える。"
                latexAlign
                    [ "sum(a, b) &= a + b"
                    , "sum'(a) &= \\lambda b. a + b"
                    ]
                "前者は通常の、複数の値を取る関数であり、後者はそれをカリー化したものである。"
                "ここで、この2つの関数に、1つだけ値を適用するとどうなるだろうか。"
                "前者は、値を2つとる関数であるから、当然そのようなことはできない。"
                "しかし、後者は、値を1つだけとる関数の組み合わせであるから、それが可能である。"
                latexAlign
                    [ "sum'(3) &= \\lambda b. 3 + b"
                    ]
                latex "sum'(3)"; " を仮に "; latex "f"; " とおいて、さらに値を適用してみよう。"
                latexAlign
                    [ "f &= sum'(3)"
                    , "f \\: 4 &= \\{sum'(3)\\} 4"
                    , "&= (\\lambda b. 3 + b) 4"
                    , "&= 3 + 4"
                    , "&= 7"
                    ]
                "このように、2つの値を用いて計算を行う関数であるにもかかわらず、中途半端に値を適用したものを好きに扱うことができるのである。"
                "そして、任意のタイミングで残りの値を適用し、計算結果を得ることができる。"
                "見方を変えると、"
                latexAlign
                    [ "succ &= sum'(1)"
                    , "succ \\: 5 &= 5 + 1 = 6"
                    , "succ \\: 3 &= 3 + 1 = 4"
                    , "succ \\: 8 &= 8 + 1 = 9"
                    ]
                latex "sum'"; " 関数に "; latex "1"; " だけを適用することにより、「適用された値に "; latex "1"; " を加える関数」を生成することができる。"
                "このような、カリー化された関数に不十分な値を適用し、関数を得る行為を部分適用といい、Haskell含め関数型プログラミング言語では極めて重要な考え方である。"
            ]

        , beginSection "Haskellのラムダ式"
            [ beginContent $ do
                "最後に、Haskellでラムダ式を記述する方法を説明する。"
                "Haskellのラムダ式は、以下の書式で記述される。"
                haskellCode
                    [ "\\変数名 -> 変数についての式"
                    ]
                "例えば、"
                latexBlock "\\lambda x. x + 2"
                "は、Haskellで"
                haskellCode
                    [ "\\x -> x + 2"
                    ]
                "である。"
                qot "λ"; "の代わりに"; qot "\\"; "を用い、"; qot "."; "の代わりに"; qot "->"; "を使うのである。"
                "そして、Haskellでは特別に、連続したラムダ式を短縮して記述できる。"
                "つまり、"
                latexBlock "\\lambda x. \\lambda y. \\lambda z. x + y + z"
                "は、Haskellで"
                haskellCode
                    [ "\\x -> \\y -> \\z -> x + y + z"
                    ]
                "であるが、短縮して"
                haskellCode
                    [ "\\x y z -> x + y + z"
                    ]
                "とすることができる。"
                "これは、複数の値を取るラムダ式が使えると考えるより、自動でラムダ式の組み合わせに展開してくれる機能であると考えるほうが妥当である。"
                "加えて、Haskellのラムダ式への値の適用は、数学と同じように行える。"
                "つまり、"
                latexAlign
                    [ "(\\lambda x. x + 1) 3"
                    , "(\\lambda x. x + 1) (a + b)"
                    , "f \\: 1 \\quad (f はラムダ式とする)"
                    ]
                "はそれぞれ"
                haskellCode
                    [ "(\\x -> x + 1) 3"
                    , "(\\x -> x + 1) (a + b)"
                    , "f 1 -- f はラムダ式とする"
                    ]
                "と等しい。"
            
            , beginSubsection "問題" $
                numberList $ do
                    problem
                        (do
                            "以下のラムダ式をHaskellで記述せよ。"
                            latexBlock "\\lambda a. a + 2"
                        )
                        (answer $
                            haskellCode
                                [ "\\a -> a + 2"
                                ]
                        )
                    
                    problem
                        (do
                            "以下のラムダ式を省略せずにHaskellで記述せよ。"
                            latexBlock "\\lambda a. \\lambda b. \\lambda c. \\lambda d. a + b + c + d"
                        )
                        (answer $
                            haskellCode
                                [ "\\a -> \\b -> \\c -> \\d -> a + b + c + d"
                                ]
                        )
                    
                    problem
                        (do
                            "以下のラムダ式を最もシンプルにHaskellで記述せよ。"
                            latexBlock "\\lambda a. \\lambda b. \\lambda c. \\lambda d. \\lambda e. a \\cdot b \\cdot c \\cdot d \\cdot e"
                        )
                        (answer $
                            haskellCode
                                [ "\\a b c d e -> a * b * c * d * e"
                                ]
                        )
            ]
        ]
