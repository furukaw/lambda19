# lambda19

2019年度 計算機言語演習 用リポジトリ。

## 各ディレクトリの内容

```
.
├── 1
├── 2                授業内容のメモ（見ないでください）
├── ...
├── cbn_ltr
│   ├── interpreter  call-by-name のインタプリタ
│   └── stepper      call-by-name のステッパ
├── cbv_ltr
│   ├── interpreter  call-by-value のインタプリタ
│   └── stepper      call-by-value のステッパ
├── generator
│   └── old_lang     ランダム項生成器
└── text             教科書の内容（見ないでください）
```

## 問題点

- generator はそこそこの確率で返ってこないので、すぐ実行が終了しなかったら中断してやり直してください。（原因不明）

## 使い方

### 共通

macOS で

1. ターミナルで ocaml コマンドが使えるようにする（やり方はよく分からないので省略）。可能なら OCaml 4.04.0
1. OCamlMakefile を入手してそのパスを知る
1. 使うディレクトリ内の `Makefile` の、 `OCAMLMAKEFILE = ~/include/OCamlMakefile` のパスをそのパスに変える
1. そのディレクトリで `make` する
1. 生成された実行ファイルを実行する

### interpreter

そのディレクトリで、

```
./interpreter
```

として `return` キーを押したあと、プログラムを入力する。改行などを入れるのは自由。

入力し終わったら `control + D` を押す。

### stepper

そのディレクトリで、

```
./stepper
```

として `return` キーを押したあと、プログラムを入力する。改行などを入れるのは自由。

入力し終わったら `control + D` を押す。

### generator

#### 型を指定する場合

そのディレクトリで、

```
./generator 2
```

などと入力する。 `2` は生成する式の深さで、自由に変える。
深さ 1 の式は `true` など、深さ 2 の式は `if true then true else true` など。

`return` キーを押した後、型を入力する。

受け入れられる型:

```
A = bool | bool + bool | bool -> bool | (A)
```

改行などを入れるのは自由。

入力し終わったら `control + D` を押す。

#### 型を指定しない場合

そのディレクトリで、

```
./generator 2 3
```

などと入力する。 `2` は生成する式の深さ、 `3` は生成する式の型の深さで、自由に変える。
深さ 1 の型は `bool` 、深さ 2 の式は `bool + bool` と `bool -> bool` 。

`return` キーを押す。
