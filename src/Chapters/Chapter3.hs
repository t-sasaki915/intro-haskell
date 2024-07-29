module Chapters.Chapter3 (chapter3) where

import           Chapter
import           Component

chapter3 :: Chapter
chapter3 =
    beginChapter "環境構築"
        [ beginChapterDescription
            "この章では、Windows 10以上を搭載したパソコンと、Visual Studio Codeを用いた環境構築方法を説明する。"

        , beginSection "Visual Studio Codeのインストール"
            [ beginContent $ do
                hlink "https://code.visualstudio.com/" "Microsoftのサイト"; "からインストーラを入手し、インストールしよう。"
                "特に注意すべき点はないが、"; qot "エクスプローラーのファイルコンテキストメニューに[Codeで開く]アクションを追加する"; "にチェックを入れておくと便利である。"
                "インストール後にVisual Studio Codeが起動した場合は、一度閉じる必要がある。"
            ]

        , beginSection "GHCupのインストール"
            [ beginContent $ do
                "次に、Haskellコンパイラをはじめ様々なツールをインストールしてくれる、GHCupをインストールする。"
                "まず、"; hlink "https://www.haskell.org/ghcup/" "GHCupのサイト"; "にアクセスしよう。"
                "次に、"; qot "Set-ExecutionPolicy"; "から始まるコマンドの右隣にあるコピーボタン(参考画像1)をクリックしよう。"
                lazyImg "./assets/ghcup_copy_button.png" "参考画像1を表示"
                "次に、管理者権限ではないPowerShellを開き、コピーしたコマンドを実行しよう。"
                "コマンドプロンプトでは実行不可能である。"
                "実行すると、GHCupインストーラからいくつかの質問が来るはずだ。"
                "下の表に、筆者がおすすめする回答を記す。"
                tableContainer $ do
                    tableHeader ["質問", "回答"]
                    tableRow [qot "Press enter to accept the default [C:\\]:", "(何も入力せずEnter)"]
                    tableRow [qot "Press enter to accept the default [C:\\\\cabal]:", "(何も入力せずEnter)"]
                    tableRow [qot "Do you want to install the haskell-language-server (HLS)for development purposes as well?", qot "Y"]
                    tableRow [qot "Do you want to install stack as well?", qot "Y"]
                    tableRow [qot "Do you want to create convenience desktop shortcuts (e.g. for uninstallation and msys2 shell)?", qot "N"]
                    tableRow [qot "Do you want GHCup to install a default MSys2 toolchain (recommended)?", qot "Y"]
                "これらの質問に答えた後、しばらく待つと"; qot "MinGW x64"; "というタイトルのウインドウが現れるはずである。"
                "しばらく待ち、"; qot "MinGW x64"; "のウインドウに緑の文と、白文字で"; qot "Press any key to exit"; "と表示されれば(参考画像2)、GHCupのインストールは成功である。"
                "何かキーを押して終了しよう。"
                lazyImg "./assets/ghcup_finish.png" "参考画像2を表示"
            ]

        , beginSection "拡張機能のインストール"
            [ beginContent $ do
                "次に、快適な開発環境のために、Visual Studio Codeに拡張機能をインストールする。"
                "まずはVisual Studio Codeを起動し、画面左にある4つの四角形で構成されたアイコン(参考画像3)をクリックしよう。"
                lazyImg "./assets/extensions_icon.png" "参考画像3を表示"
                "そして、"; qot "EXTENSIONS: MARKETPLACE"; "の下にある検索欄に"; qot "haskell"; "と入力する。"
                "一番上に表示される"; qot "Haskell"; "という項目の、"; qot "Install"; "ボタン(参考画像4)をクリックして、少し待てば拡張機能のインストールは完了だ。"
                "インストールが完了すると、"; qot "Install"; "ボタンは消えるはずである。"
                lazyImg "./assets/instal_haskell_extension.png" "参考画像4を表示"
            ]

        , beginSection "Visual Studio Codeの設定"
            [ beginContent $ do
                "次に、Visual Studio Codeの設定を行う。"
                "Visual Studio Codeの画面左下にある歯車アイコンをクリックし、"; qot "Settings"; "を選択(参考画像5)しよう。"
                lazyImg "./assets/select_settings.png" "参考画像5を表示"
                "そして、"; qot "Search settings"; "と書かれた検索欄で"; qot "manage hls"; "を検索し、"; qot "Haskell: Manage HLS"; "という項目が出てくることを確認しよう。"
                "最後に、"; qot "PATH"; "と書かれた場所をクリックし、"; qot "GHCup"; "を選択(参考画像6)すれば、Visual Studio Codeの設定は完了である。"
                lazyImg "./assets/select_ghcup.png" "参考画像6を表示"
                "ここで、Visual Studio Codeのメニューから"; qot "File"; "→"; qot "Auto Save"; "を選択して、オートセーブを有効にしておくと便利である。"
            ]

        , beginSection "サンプルプログラムの実行"
            [ beginContent $ do
                "最後に、サンプルプログラムを実行してみよう。"
                "拡張子が"; qot ".hs"; "であるファイルを作成し、それをVisual Studio Codeで開く。"
                "開いた際に、Visual Studio Codeからhlsのアップデートを促すメッセージ(参考画像7)が表示されるかもしれない。"
                "表示された場合は、"; qot "Yes"; "を押して続行する。"
                lazyImg "./assets/hls_update.png" "参考画像7を表示"
                haskellCode
                    [ "main :: IO ()"
                    , "main = putStrLn \"Hello World.\""
                    ]
                "サンプルプログラムの入力が完了したら、Visual Studio Codeのメニューから"; qot "Terminal"; "→"; qot "New Terminal"; "を選択し、ターミナルを起動しよう。"
                "画面下部に表示されたPowerShellで、先ほど作成した"; qot ".hs"; "ファイルのところまで移動し、"; qot "stack runhaskell ファイル名.hs"; "を実行しよう。"
                "最後に"; qot "Hello World."; "と表示されれば、サンプルプログラムの実行は成功である。"
                "初回は時間がかかり、たくさんのログが出力されるが、2回目以降は比較的高速になり、"; qot "Hello World."; "のみが表示されるようになるはずだ。"
            ]
        ]
