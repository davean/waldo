{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Monoid
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as JS
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import qualified Blaze.ByteString.Builder.Char8 as BB8
import qualified Blaze.ByteString.Builder.ByteString as BBB
import Control.Monad.Trans.Resource (ResourceT, transResourceT)
import System.Environment
import Data.Maybe
import Safe

import Waldo.Waldo
import Waldo.Stalk
import qualified Waldo.StoryExample as SE

main :: IO ()
main = do
    env <- getEnvironment
    {- This is where we "load" a set of scripts to serve.      
     - Conceptually the server can serve any number of scripts.
     -}
    wdata <- loadWaldo [("jarUcyikAg3", SE.loadScriptGen)]
    Warp.runSettings (warpsettings env) (route wdata)
  where
    warpsettings env = Warp.defaultSettings {
        Warp.settingsPort = fromMaybe 3000 (join $ fmap readMay $ lookup "WALDO_PORT" env)
      }

route :: WaldoData -> WAI.Application
route wd req =
    transResourceT (flip runReaderT wd) $ 
    case (WAI.requestMethod req, WAI.pathInfo req) of
      ("GET",  ["story", s]) -> getScript req s
      _ -> return resp404

resp404 :: WAI.Response
resp404 =
  WAI.ResponseBuilder
  HTTP.status404
  [("Content-Type", "text/plain")] $
  BB8.fromString "Not Found"

getScript :: WAI.Request -> T.Text -> ResourceT (ReaderT WaldoData IO) WAI.Response
getScript req storySet = do
    let stalkreq = wai2stalk req
    wd <- lift $ ask
    pd <- liftIO $ stalk (wdStalkDB wd) stalkreq
    case Map.lookup storySet (wdGenScript wd) of
      Nothing -> return resp404
      Just storyGen -> do
        script <- liftIO $ storyGen pd
        return $ WAI.ResponseBuilder
          HTTP.status200
          [("Content-Type", "application/javascript")
          ,("Access-Control-Allow-Origin", "*")] $
          mconcat $ concat [
              [BBB.fromByteString "waldoCallback(" ]
            , map BBB.fromByteString $ BSL.toChunks $ JS.encode script
            , [BBB.fromByteString ")"]
            ]
