safeexec
========

`erlang:open_port/2`により起動した外部コマンドの終了処理を安全・確実に行えるようにするためのライブラリ。  
このライブラリは、C言語で記述されたsafeexec コマンドとそれを利用するErlangインタフェースを提供する。

対応OS
------
- Linux
- Mac
  - experimental
  - safeexecコマンドがSIGKILLで落とされた場合に、子プロセスが残ってしまうことがある

safeexecコマンド
---------------

基本的にはただ引数で指定された外部コマンドを実行するだけのコマンド:

    $ safeexec コマンド名 コマンド引数...

safeexe 経由で実行された外部コマンドは、以下の性質を備えるようになる:

- ErlangVM(プロセス)終了時に、確実に終了する
- `erlang:port_close/1`あるいは`リンクプロセスのダウン`により、外部コマンドに紐づくポートが終了した場合に、確実に終了する

つまり、外部コマンドを safeexec 経由で呼び出すことにより、  
利用者は外部コマンドポートを、Erlangの通常のプロセスに近い終了モデル(リンクしているプロセスが終了した場合は自分も終了する)で扱えるようになる。


Erlangインタフェース
--------------------

`safeexec:open_port/2`関数が利用できる。
以下の二点を除いて`erlang:open_port/2`と全く同じ動作をする:

- 第一引数が`{spawn, Command}`の場合は`Command`の前にsafeexecコマンドのパスが付与される
- 第一引数が`{spawn_executable, FileName}`の場合は`FileName`の値はsafeexecコマンドのパスに置換され、もともとの`FileName`の値は`args`オプションの先頭に挿入される
- `{spawn_driver, Command}`および`{fd, In, Out}`は非サポート
