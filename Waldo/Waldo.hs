module Waldo.Waldo (
    WaldoData(..)
  , loadWaldo
  ) where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Waldo.Stalk
import Waldo.Script

data WaldoData =
  WaldoData {
      wdStalkDB :: StalkDB
    , wdGenScript :: HashMap T.Text (PersonalData -> IO Script)
    }

loadWaldo :: [(T.Text, IO (PersonalData -> IO Script))] ->IO WaldoData
loadWaldo storyGenLoaders = do
    sdb <- loadStalkDB
    storyGens <- forM storyGenLoaders $ \(nm, ldr) -> do
      sgen <- ldr
      return (nm, sgen)
    let wdata = WaldoData { wdStalkDB = sdb
                          , wdGenScript = Map.fromList storyGens
                          }
    putStrLn "Loading completed!"
    return wdata
