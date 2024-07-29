module Chapters.Chapter1 (chapter1) where

import           Chapter
import           Component

chapter1 :: Chapter
chapter1 =
    beginChapter "はじめに"
        [ beginChapterDescription $ do
            "このテキストは、"; hlink "https://github.com/t-sasaki915" "筆者"; "のHaskellの理解度を確かめることを主な目的として作成された。"
            "また、筆者は専門家ではないので、このテキスト内に間違いを見つけた人や、より良い説明を思いついた人は、ぜひ"; hlink "https://github.com/t-sasaki915/intro-haskell" "GitHub"; "で提案してほしい。"
            "このテキストが誰かのHaskell学習に役立つようなことがあれば、大変うれしく思う。"

        , beginSection "テキストを読むにあたって"
            [ beginContent $ do
                "このテキストを読むにあたり、以下のものは必須となる。"
                dotList $ do
                    listItem "Windowsパソコン (Windows 10以上。LinuxやmacOSは筆者がわからない)"
                    listItem "PowerShellの基本操作・ソフトのインストール方法などの知識"
                    listItem "高校程度の数学知識 (微分積分などを行うわけではない)"
                    listItem "最低限の英単語"
                "また、必須ではないが、以下の知識や経験がある場合は、内容理解の役に立つだろう。"
                dotList $ do
                    listItem "C言語やPythonなどのプログラミング経験"
                    listItem "LispやOCamlなどの関数型プログラミング経験"
                    listItem "圏論やラムダ計算の知識"
                "PowerShellなどの知識は、環境構築の時のみ必要となる。"
                "C言語やPythonなどを経験している読者は、現在のプログラミングに対する考え方を多少変える必要がある。"
                "例えば、変数の再代入が必要な処理は、書き直しが必要である。"
            ]
        ]
