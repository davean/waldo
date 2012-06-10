{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Aeson as JS
import Data.Conduit (($$), ($=))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Zlib as CZ
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Control.DeepSeq (deepseq)

import Waldo.Stalk
import Waldo.Waldo
import qualified Waldo.StoryExample as SE

main :: IO ()
main = do
  wd <- loadWaldo [("jarUcyikAg3", SE.loadScriptGen)]
  flip runReaderT wd $ runResourceT $
       CB.sourceFile "stalkreqs.gz" $= CZ.ungzip
    $= CB.lines
    $= CL.concatMap (maybeToList . JS.decode' . s2l::BS8.ByteString -> [StalkRequest])
    $= CL.mapM (\r -> liftIO $ stalk (wdStalkDB wd) r)
    $= CL.mapM (\p -> (liftIO $ (fromJust $ Map.lookup "ghenkEggov8" (wdGenScript wd)) p) >>= \r -> r `deepseq` return r)
--    $$ CL.mapM_ (\_ -> return ())
    $$ CL.mapM_ (\d -> liftIO $ print d)
  return ()

l2s :: BSL8.ByteString -> BS8.ByteString
l2s = BS8.concat . BSL8.toChunks

s2l :: BS8.ByteString -> BSL8.ByteString
s2l = BSL8.fromChunks . (:[])