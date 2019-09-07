{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BS
import           GHC.Generics
import           Network.HTTP.Media       ((//), (/:))
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.JS
import           System.Process
import           Web.FormUrlEncoded       (FromForm (..), parseUnique)

newtype Compiler = Compiler { source :: String }
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromForm Compiler where
  fromForm form = Compiler <$> parseUnique "source" form

type CompilerAPI = "compiler" :> ReqBody '[JSON, FormUrlEncoded] Compiler :> Post '[JSON] String

type API = Get '[HTML] BS.ByteString
  :<|> "static" :> Raw
  :<|> CompilerAPI

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BS.ByteString where
  mimeRender _ bs = bs

server :: BS.ByteString -> Server API
server indexFile = index
                   :<|> serveDirectoryFileServer "./static"
                   :<|> compile
  where
    index = return indexFile
    compile :: Compiler -> Handler String
    compile (Compiler src) = do
      liftIO $ writeFile "./tmp.c" src
      liftIO $ readProcess "./hoc_nyan/hoc" ["./tmp.c"] ""

compilerAPI :: Proxy CompilerAPI
compilerAPI = Proxy

api :: Proxy API
api = Proxy

main :: IO ()
main = do
  writeJSForAPI compilerAPI jquery "./static/client.js"
  indexFile <- BS.readFile "./static/index.html"
  run 8080 (serve api (server indexFile))
