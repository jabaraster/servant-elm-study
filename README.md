# servant-elm-study

# TODO
* エンドポイントの追加 →成功
* 静的ファイルの配信 →成功
* 静的ファイルをバイナリに埋め込む →成功
* 環境変数によって静的ファイルを動的に読み込むようにする →成功
* データストアとの連携
* AWSの何らかのサービスを使ってデプロイ

# 遭遇した（あるいはしている）問題

## dockerがmkdirに失敗する
エラーメッセージでググるとFile Sharingの設定をいじれ、と出てくるが、それでも解消しない場合、既に存在するディレクトリを作成しようとしてエラーになっている可能性を疑うべし.

# シングルバイナリを作るために試したこと

```shinsakata/ghc-single-binary-builder```を使ってビルドを試みる。

stack.yamlのresolverを変えて２通り試してみたが、どちらも手詰まり。

# AWS Beandtalkにデプロイするのに試したこと

```
eb init -p docker --profile jabara-admin servant-elm-study
# eb init --profile jabara-admin
eb create --profile jabara-admin servant-elm-study
```