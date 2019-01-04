# 住所録API

## Description

Haskell Servant PostgreSQLで住所録WebAPI

## Author

[守田裕一](info@reju.jp)

## エンドポイント

|method|endpoint|説明|
|---|---|---|
|GET|/users|ユーザーリスト|
|GET|/users/:id|指定ユーザーの情報|
|POST|/users|新規ユーザー登録|
|PUT|/users/:id|指定ユーザの更新|
|DELETE|/users/:id|指定ユーザの削除|
|||
|GET|/tags|タグリスト|
|GET|/tags/:id|指定タグの情報|
|POST|/tags|新規タグの登録|
|PUT|/tags/:id|指定タグの更新|
|DELETE|/tags/:id|指定タグの削除|

