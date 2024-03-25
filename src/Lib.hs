{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (secondsToDiffTime)
import Database.PostgreSQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectUser
      ),
    Connection,
    Only (fromOnly),
    connect,
    defaultConnectInfo,
  )
import Models.Person
  ( Person,
    PersonBaseData,
    createPerson,
    deletePerson,
    getPeople,
    getPerson,
    updatePerson,
  )
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.WebSockets (PendingConnection, acceptRequest, receiveData, sendTextData, withPingThread)
import Servant
  ( Application,
    Capture,
    Delete,
    Get,
    Handler,
    JSON,
    Patch,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.API.WebSocket (WebSocketPending)
import WsApp (clientState, wsApp)

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectDatabase = "hsdatastore",
      connectUser = "hs_datastore_admin",
      connectPassword = "jw8s0F4"
    }

type API =
  "people" :> Get '[JSON] [Person]
    :<|> "person" :> Capture "uid" Int :> Get '[JSON] [Person]
    :<|> "new-person" :> ReqBody '[JSON] PersonBaseData :> Post '[JSON] [Int64]
    :<|> "update-person" :> Capture "uid" Int :> ReqBody '[JSON] PersonBaseData :> Patch '[JSON] Bool
    :<|> "delete-person" :> Capture "uid" Int :> Delete '[JSON] Bool
    :<|> "wschat" :> WebSocketPending

startApp :: IO ()
startApp = do
  conn <- connect localPG
  run 8080 $ app conn

app :: Connection -> Application
app = simpleCors . serve api . server

api :: Proxy API
api = Proxy

server :: Connection -> Server API
server conn =
  people
    :<|> person
    :<|> newPerson
    :<|> modifyPerson
    :<|> removePerson
    :<|> wsChat
  where
    people :: Handler [Person]
    people = liftIO $ getPeople conn

    person :: Int -> Handler [Person]
    person id = liftIO $ getPerson conn id

    newPerson :: PersonBaseData -> Handler [Int64]
    newPerson personData = liftIO $ fmap (fmap fromOnly) (createPerson conn personData)

    modifyPerson :: Int -> PersonBaseData -> Handler Bool
    modifyPerson uid personData = liftIO $ updatePerson conn uid personData

    removePerson :: Int -> Handler Bool
    removePerson uid = liftIO $ deletePerson conn uid

    wsChat :: (MonadIO m) => PendingConnection -> m ()
    wsChat pending = do
      clients <- liftIO $ newMVar clientState
      liftIO $ wsApp clients pending
