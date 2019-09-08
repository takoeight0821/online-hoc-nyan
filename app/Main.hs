{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BS
import           GHC.Generics
import           Network.HTTP.Media       ((//), (/:))
import           Network.Wai.Handler.Warp
import           Servant
import           System.Process
import           Web.FormUrlEncoded       (FromForm (..), parseUnique)

newtype Compiler = Compiler { source :: String }
  deriving (Show, Generic, ToJSON, FromJSON)

data Output = Output { object :: String, compileError :: String, binary :: String }
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromForm Compiler where
  fromForm form = Compiler <$> parseUnique "source" form

type API = Get '[HTML] BS.ByteString
           :<|> "static" :> Raw
           :<|> "compiler" :> ReqBody '[JSON, FormUrlEncoded] Compiler :> Post '[JSON] Output

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BS.ByteString where
  mimeRender _ bs = bs

server :: BS.ByteString -> Server API
server indexFile = pure indexFile
                   :<|> serveDirectoryFileServer "./static"
                   :<|> compile
  where
    compile :: Compiler -> Handler Output
    compile (Compiler src) = do
      liftIO $ writeFile "./tmp.c" src
      (_, asm, err)<- liftIO $ readProcessWithExitCode "./hoc_nyan/hoc" ["./tmp.c"] ""
      liftIO $ writeFile "./tmp.s" asm
      liftIO $ callProcess "gcc" ["-static", "-c", "./tmp.s"]
      bin <- liftIO $ readProcess "objdump" ["-D", "./tmp.o"] ""
      return $ Output asm err bin

main :: IO ()
main = do
  indexFile <- BS.readFile "./static/index.html"
  run 8080 (serve (Proxy @API) (server indexFile))
