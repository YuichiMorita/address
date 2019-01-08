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

type Uuid = String

data User = User
  { id        :: Uuid
  , company :: Maybe String
  , title  :: Maybe String
  , name  :: String
  , memo  :: Maybe String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

users :: [User]
users = [ User "11111-1111-1111" Nothing Nothing "山田 太郎" Nothing
        , User "22222-2222-2222" (Just "株式会社井上") (Just "代表取締役") "井上 太郎" (Just "メモメモ")
        ]

yamada :: String -> User
yamada _ = users !! 2

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

server :: Server API
server = return users
        :<|> return yamada "test"

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 app
