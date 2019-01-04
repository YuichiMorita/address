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

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type Uuid = String

type API = "users" :> Get '[JSON] [User]
        :<|> "users" :> Capture "id" Uuid :> Get '[JSON] User
        :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
        :<|> "users" :> Capture "id" Uuid :> ReqBody '[JSON] User :> Put '[JSON] User
        :<|> "users" :> Capture "id" Uuid :> DeleteNoContent '[JSON] NoContent
        :<|> "tags" :> Get '[JSON] [Tag]
        :<|> "tags" :> Capture "id" Integer :> Get '[JSON] Tag
        :<|> "tags" :> ReqBody '[JSON] Tag :> Post '[JSON] Tag
        :<|> "tags" :> Capture "id" Integer :> ReqBody '[JSON] Tag :> Put '[JSON] Tag
        :<|> "tags" :> Capture "id" Integer :> DeleteNoContent '[JSON] NoContent

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
