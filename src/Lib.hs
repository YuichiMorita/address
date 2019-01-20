{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Logger(runStdoutLoggingT)
import Data.ByteString.Char8

import Database.Persist.Postgresql

------------------
-- データベース接続
------------------
-- PostgreSQLデータベース接続
connectDB :: IO ConnectionPool
connectDB =
  runStdoutLoggingT $ createPostgresqlPool conf connections
  where
    conf = pack "host=localhost dbname=postgres user=postgres password=password port=5432"
    connections = 10

------------------
-- 型定義
------------------
--UUID
type Uuid = String

-- ユーザー種別型
data UserTypes = Indivisual | Company deriving (Eq,Show)
$(deriveJSON defaultOptions ''UserTypes)


-- 電話種別型
type TellTypes = String

-- 電話番号型
data Tell = Tell
    {telltype :: TellTypes
    ,number :: String
    ,tellmemo :: Maybe String
    } deriving(Eq,Show)
$(deriveJSON defaultOptions ''Tell)

-- メール種別型
type EmailTypes = String

-- メール型
data Email = Email
    {emailtype::EmailTypes
    ,email :: String
    ,emailmemo :: Maybe String
    } deriving(Eq,Show)
$(deriveJSON defaultOptions ''Email)

-- 住所種別型
type AddrTypes = String

-- 住所型
data Address = Address
    {addrtype :: AddrTypes
    ,postal :: String
    ,pref :: String
    ,addr1 :: String
    ,addr2 ::String
    ,bld :: Maybe String
    ,place_name :: Maybe String
    ,addrmemo :: Maybe String
    } deriving(Eq,Show)

$(deriveJSON defaultOptions ''Address)

-- URL種別型
type UrlTypes = String

-- URL型
data URL = URL
    {urltypes :: UrlTypes
    ,url :: String
    ,urlmemo :: Maybe String

    } deriving(Eq,Show)
$(deriveJSON defaultOptions ''URL)

-- tag型
type Tag = String

-- ユーザー型
data User = User
  { id        :: Uuid
  , usertype :: UserTypes
  , company :: Maybe String
  , title  :: Maybe String
  , name  :: String
  , memo  :: Maybe String
  , tells :: [Tell]
  , emails :: [Email]
  , addrs :: [Address]
  , urls :: [URL]
  , tags :: [Tag]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

------------------
-- エンドポイント
------------------
type API = "users" :> Get '[JSON] [User]
        :<|> "users" :> Capture "id" Uuid :> Get '[JSON] User
--      :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
--      :<|> "users" :> Capture "id" Uuid :> ReqBody '[JSON] User :> Put '[JSON] User
--      :<|> "users" :> Capture "id" Uuid :> DeleteNoContent '[JSON] NoContent
--      :<|> "tags" :> Get '[JSON] [Tag]
--      :<|> "tags" :> Capture "id" Integer :> Get '[JSON] Tag
--      :<|> "tags" :> ReqBody '[JSON] Tag :> Post '[JSON] Tag
--      :<|> "tags" :> Capture "id" Integer :> ReqBody '[JSON] Tag :> Put '[JSON] Tag
--      :<|> "tags" :> Capture "id" Integer :> DeleteNoContent '[JSON] NoContent

------------------
-- ハンドラ
------------------
users :: [User]
users =  [ User "11111-1111-1111" Indivisual Nothing Nothing "山田 太郎" Nothing [] [] [] [] ["個人","Reju"]
        , User "22222-2222-2222" Company (Just "株式会社井上") (Just "代表取締役") "井上 太郎" (Just "メモメモ") [Tell "携帯電話" "090-1111-1111" Nothing] [Email "勤務先" "info@iinoue.com" Nothing] [Address "勤務先" "530-0053" "大阪府" "大阪市北区末広町" "3-13" (Just "扇町松島ビル9F") Nothing Nothing] [URL "勤務先" "http://www.reju.jp/" Nothing] ["法人","Reju","REX"]
        ]
-- /users
allUsers :: Handler [User]
allUsers = return users

-- /users/:Uuid
yamada :: Uuid -> Handler User
yamada _ = return (users !! 1)

-- データベース接続を引数に取ってサーバAPIを返す
server :: IO ConnectionPool -> Server API
server conn = allUsers
    :<|> yamada

------------------
-- サーバ
------------------
api :: Proxy API
api = Proxy

-- Application はWaiの型らしい
-- type Application = Request -> ResourceT IO Response
app :: Application
app = serve api $ server connectDB


-- runはWarpの関数らしい
-- run::Port -> Application -> IO()
startApp :: IO ()
startApp = run 8080 app
