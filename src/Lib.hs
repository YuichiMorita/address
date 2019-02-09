{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.ByteString.Char8 (pack)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT,ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Text (Text)

import Database.Persist
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import Database.Persist.Postgresql
import Database.Persist.TH

------------------
-- データベース接続
------------------
-- PostgreSQL 設定
connStr :: ConnectionString
connStr = pack "host=localhost dbname=postgres user=postgres password=password port=5432"

-- 接続数
openConnectionCount :: Int
openConnectionCount = 10

-- PostgreSQLデータベース接続
pgPool :: IO ConnectionPool
pgPool = runStdoutLoggingT $ createPostgresqlPool connStr openConnectionCount

-- マイグレーション
doMigration :: IO()
doMigration = runStdoutLoggingT $ runResourceT $ withPostgresqlConn connStr $ runReaderT $ runMigration $ migrateAll

------------------
-- 型定義
------------------
--　自作コンフィグ型
data MyAppConfig = MyAppConfig
  { getPool :: ConnectionPool -- DBアクセス用プール
  , getApplicationText :: Text -- アプリ設定テキスト
  , getApplicationFlag :: Bool -- アプリ設定フラグ
  }

--　自作ハンドラ型
-- https://qiita.com/cyclone_t/items/8b5a80162c4dff3a8770
-- Servantでは、APIハンドラの引数はいじれないですが、ハンドラで扱うモナドは変更できます。Servantでの「参照用グローバル値」はハンドラでのモナドにReaderTを適用することで実装できます。
type MyAppHandler = ReaderT MyAppConfig Handler

-- APIハンドラ登録
-- MyAppHandler型からMyAppServer型を生成
type MyAppServer api = ServerT api MyAppHandler

-- ユーザー種別型
data AccountType = Indivisual | Company deriving (Eq,Show)
$(deriveJSON defaultOptions ''AccountType)
derivePersistField "AccountType"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Account
      type AccountType
      company Text Maybe
      title Text Maybe
      name Text
      accountMemo Text Maybe
      deriving Show

  Telltype
      telltype Text
      deriving Show

  Tell
      accountId AccountId
      telltypeId TelltypeId
      number Text
      tellMemo Text Maybe
      deriving Show

  Emailtype
      emailtype Text
      deriving Show

  Email
    accountId AccountId
      emailtypeId EmailtypeId
      email Text
      emailMemo Text Maybe
      deriving Show

  Addresstype
      adresstype Text
      deriving Show

  Address
      accountId AccountId
      addrtypeId AddresstypeId
      postal Text
      pref Text
      addr1 Text
      addr2 Text Maybe
      bld Text Maybe
      place_name Text
      addressMemo Text Maybe
      deriving Show

  Url
      accountId AccountId
      urltype Text
      url Text
      urlMemo Text Maybe
      deriving Show

  Tag
      tag Text
      deriving Show

  TagAccount
      accountId AccountId
      tagId TagId
      UniqueTagAccount accountId tagId
      deriving Show
|]


------------------
-- エンドポイント
------------------
type API = "users" :> Get '[JSON] [Account]
        :<|> "users" :> Capture "id" Uuid :> Get '[JSON] Account
--      :<|> "users" :> ReqBody '[JSON] Account :> Post '[JSON] Account
--      :<|> "users" :> Capture "id" Uuid :> ReqBody '[JSON] Account :> Put '[JSON] Account
--      :<|> "users" :> Capture "id" Uuid :> DeleteNoContent '[JSON] NoContent
--      :<|> "tags" :> Get '[JSON] [Tag]
--      :<|> "tags" :> Capture "id" Integer :> Get '[JSON] Tag
--      :<|> "tags" :> ReqBody '[JSON] Tag :> Post '[JSON] Tag
--      :<|> "tags" :> Capture "id" Integer :> ReqBody '[JSON] Tag :> Put '[JSON] Tag
--      :<|> "tags" :> Capture "id" Integer :> DeleteNoContent '[JSON] NoContent

------------------
-- ハンドラ
------------------
users :: [Account]
users =  [ Account "11111-1111-1111" Indivisual Nothing Nothing "山田 太郎" Nothing [] [] [] [] ["個人","Reju"]
        , Account "22222-2222-2222" Company (Just "株式会社井上") (Just "代表取締役") "井上 太郎" (Just "メモメモ") [Tell "携帯電話" "090-1111-1111" Nothing] [Email "勤務先" "info@iinoue.com" Nothing] [Address "勤務先" "530-0053" "大阪府" "大阪市北区末広町" "3-13" (Just "扇町松島ビル9F") Nothing Nothing] [URL "勤務先" "http://www.reju.jp/" Nothing] ["法人","Reju","REX"]
        ]
-- /users
allAccounts :: Handler [Account]
allAccounts = return users

-- /users/:Uuid
yamada :: Uuid -> Handler Account
yamada _ = return (users !! 1)

-- データベース接続を引数に取ってサーバAPIを返す
server :: ConnectionPool -> Server API
server conn = allAccounts
    :<|> yamada

------------------
-- サーバ
------------------
api :: Proxy API
api = Proxy

-- Application はWaiの型らしい
-- type Application = Request -> ResourceT IO Response
myAppApp :: MyAppConfig -> Application
myAppApp = serve myAppApp . myAppToServer

myAppToServer :: MyAppConfig -> Server MyAppAPI
myAppToServer cfg = enter(runReaderTNat cfg :: MyAppHandler :~> Handler) myAppToServer

-- サーバ起動
startApp :: IO ()
startApp = do
  pool <- pgPool
  args <- getArgs
  let cfg = MyAppConfig {getPool = pool,getApplicationText = "AddressAPI", getApplicationFlag = True} --コンフィグ
      arg1 = if not(null args) then Just(head args) else Nothing -- 実行時の第１引数を所得
  case arg1 of
    Just "migrate"  -> putStrLn "-- Executing Migration  --" >> doMigration -- 引数 migrateでマイグレーション実行
    _               -> putStrLn "-- Start Address API Server --" >> run 8080 $ myAppApp cfg --引数なしでサーバ起動

