{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import qualified Database.PostgreSQL.Simple as D
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad
import Control.Applicative
import Database.PostgreSQL.Simple.URL
import Control.Monad.IO.Class
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types.Status
import System.Environment
import Control.Exception 
import Database.PostgreSQL.Simple.Errors

                     
data Resultado= Resultado{tipo :: Maybe String, mensaje :: Maybe String} deriving (Show,Generic)
instance ToJSON Resultado


data Menu = Menu { idMenu :: Maybe Int, name :: Maybe String, description :: Maybe String, price :: Maybe Int, restaurant :: Maybe Int } deriving (Show,Generic)
instance ToJSON Menu
instance FromJSON Menu

data Client= Client{username :: Maybe String,
                    nameClient :: Maybe String,
                    lastname :: Maybe String,
                    idClient:: Maybe String,
                    email:: Maybe String,
                    phone:: Maybe String,
                    cellphone:: Maybe String,
                    password:: Maybe String }
                    deriving (Show,Generic)

instance ToJSON Client
instance FromJSON Client

matchesId :: Int -> Menu -> Bool
matchesId id menu = case idMenu menu of
        Nothing -> False
        Just int -> int == id


success :: String
success = "Success"

error' :: String
error' = "Error"

instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Client where
  toRow d = [toField (username d),
             toField (nameClient d),
             toField (lastname d),
             toField (idClient d),
             toField (email d),
             toField (phone d),
             toField (cellphone d),
             toField (password d)]



instance FromRow Menu where
  fromRow = Menu <$> field <*> field <*> field <*> field <*> field
  
  
instance ToRow Menu where
  toRow d = [toField (idMenu d), toField (name d), toField (description d), toField (price d), toField (restaurant d)]



getAllMenus :: D.Connection -> IO [Menu]
getAllMenus c = do
  list <- (D.query_ c "select * from menu" :: IO [Menu])
  return list
  
  
getAllClientes :: D.Connection -> IO [Client]
getAllClientes c = do
  list <- (D.query_ c "select * from client" :: IO [Client])
  return list



main = do
  putStrLn "Starting Server..."
  conn <- D.connectPostgreSQL  "postgresql://107.170.37.136:5432/restaurants"
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
     
    get "/" $ do
      text ("Bienvenido a un servicio REST construido con Haskell, ingrese a /menus para ver la lista de menus")

    get "/menus" $ do
      variable <- liftIO (getAllMenus conn)
      json variable

    get "/menus/:id" $ do
      id <- param "id"
      allMenus <- liftIO (getAllMenus conn)
      json (filter (matchesId id) allMenus)
      
    post "/menus" $ do
      menu <- (jsonData :: ActionM Menu)
      response <- liftIO $ try $ D.execute conn "insert into menu (name,description,price,restaurant) values (?,?,?,?)" ((name menu), (description menu), (price menu),(restaurant menu))
      case response of
        Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Menu agregado"}) >> status created201
        Left e -> json (Resultado {tipo= Just error', mensaje= Just (show $ D.sqlErrorMsg e)})


    post "/clientes" $ do
      client <- (jsonData :: ActionM Client)
      response <- liftIO $ try $ D.execute conn "insert into client (username,name,lastname,id,email,password,phone,cellphone) values (?,?,?,?,?,?,?,?)" ((username client),(nameClient client),(lastname client),(idClient client),(email client),(password client),(phone client),(cellphone client))
      case response of
        Right _ -> json (Resultado {tipo= Just success, mensaje= Just "Cliente agregado"}) >> status created201
        Left (constraintViolation -> Just (UniqueViolation _)) -> json (Resultado {tipo= Just error', mensaje= Just "Ya existe un cliente con el mismo username"})>> status badRequest400
        Left e -> json (Resultado {tipo= Just error', mensaje= Just (show $ D.sqlErrorMsg e)})
        
    get "/clientes" $ do
      variable <- liftIO (getAllClientes conn)
      json variable


    


