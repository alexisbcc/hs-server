{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Data.Time.Clock (secondsToDiffTime)
import Models.Person
        (Person
        , PersonBaseData
        , getPeople
        , getPerson
        , createPerson
        , updatePerson
        , deletePerson)
import Data.Int (Int64)


localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectDatabase = "bloghaskell"
        , connectUser = "haskell_server"
        , connectPassword = "doncamatic"
        }

type API = "people" :> Get '[JSON] [Person]
        :<|> "person" :> Capture "uid" Int :> Get '[JSON] [Person]
        :<|> "new-person" :> ReqBody '[JSON] PersonBaseData :> Post '[JSON] [Int64]
        :<|> "update-person" :> Capture "uid" Int :> ReqBody '[JSON] PersonBaseData :> Patch '[JSON] Bool
        :<|> "delete-person" :> Capture "uid" Int :> Delete '[JSON] Bool

startApp :: IO ()
startApp = do
        conn <- connect localPG
        run 8080 $ app conn

app :: Connection -> Application
app = serve api . server

api :: Proxy API
api = Proxy
 
server :: Connection -> Server API
server conn = people
      :<|> person
      :<|> newPerson
      :<|> modifyPerson
      :<|> removePerson

  where people :: Handler [Person]
        people = liftIO $ getPeople conn

        person :: Int -> Handler [Person]
        person id = liftIO $ getPerson conn id

        newPerson :: PersonBaseData -> Handler [Int64]
        newPerson personData = liftIO $ fmap (fmap fromOnly) (createPerson conn personData)

        modifyPerson :: Int -> PersonBaseData -> Handler Bool
        modifyPerson uid personData = liftIO $ updatePerson conn uid personData

        removePerson :: Int -> Handler Bool
        removePerson uid = liftIO $ deletePerson conn uid
