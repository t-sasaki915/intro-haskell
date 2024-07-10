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
        ]
