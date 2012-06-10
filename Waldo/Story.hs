{-# LANGUAGE OverloadedStrings #-}
module Waldo.Story (
    selectStory
  , knapsackSizer
  , Story(..)
  , StoryGuard, StoryOption
  , isIn, browserIs, osIs, netSpeedIs, orgIs, orgMatch, ispIs
  , refererDomainIs
  , pdTestJustIs
  , giveThem, allocate
  ) where

import Data.Maybe
import Data.List
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import Network.URI
import Text.Regex.TDFA ((=~))

import Waldo.Stalk
import Waldo.Script

type StoryGuard = ReaderT PersonalData Maybe ()
type StoryOption = ReaderT PersonalData Maybe Story

data Story = 
    StoryGoto {
        storyGoto :: T.Text
      }
  | Story {
         storyAltText   :: T.Text
       , storyPanelSets :: [PanelSizes]
       , storyPadX :: Int
       , storyPadY :: Int
       , storyName :: T.Text
       }
  deriving (Show)

selectStory :: ((Int, Int) -> Story -> Maybe Script) -> Script -> [StoryOption] -> PersonalData -> IO Script
selectStory sizer d storyGens pd = do
  --print storyGens
  -- generate stories
  let stories = catMaybes $ map (flip runReaderT pd) storyGens
  --print stories
  -- size the selected scripts
  let scripts = mapMaybe doSize stories
  --print scripts
  -- Get our script, either the default or a selected one.
  return $ fromMaybe d $ listToMaybe scripts
  where
    doSize (s@Story {}) = sizer (pdScreen pd) s
    doSize (StoryGoto t) = Just (ScriptTo t)

knapsackSizer :: Int -> (Int, Int) -> Story -> Maybe Script
knapsackSizer sitePad (w, h) s =
    -- Get the first entry if there is one, the smallest if none of them fit.
    listToMaybe $ (sortCorrectDir sizeLimited) ++ (take 1 areaSortedSized)
  where
    -- selected sort dir by what we know about the screen
    sortCorrectDir =
      if (h > 0) && (w > 0)
      then reverse
      else id
    -- The fitting scripts
    sizeLimited = fitWidth $ fitHeight $ areaSortedSized
    -- sort by area
    areaSortedSized = areaSort allScripts
    -- Of all scripts
    allScripts = do
      combo <- mapM id $ storyPanelSets s
      return $ mkScript (storyName s) (storyAltText s) combo
    areaSort = sortBy (\a b -> compare (scriptArea a) (scriptArea b))
    scriptArea scr = (sHeight scr) * (sWidth scr)
    fitHeight scripts =
      if h > 0
      then filter (\scr -> h > (sHeight scr+storyPadY s+sitePad)) scripts
      else scripts
    fitWidth scripts =
      if w > 0
      then filter (\scr -> w > (sWidth scr+storyPadX s+sitePad)) scripts
      else scripts

refererDomainIs :: String -> StoryGuard
refererDomainIs d =
  asks pdRefURI >>= guard . fromMaybe False . fmap ((isSuffixOf d) . uriRegName) . join . fmap uriAuthority

--refererMatches :: 

pdTestJustIs :: Eq a => (PersonalData -> Maybe a) -> a -> StoryGuard
pdTestJustIs g v = asks g >>= guard . maybe False (v==)

isIn :: ByteString -> StoryGuard
isIn locName = asks pdLocal >>= guard . (not . null . (filter (locName==)))

browserIs :: Browser -> StoryGuard
browserIs b = asks pdBrowser >>= guard . (maybe False (b==))

osIs :: OS -> StoryGuard
osIs os = asks pdOS >>= guard . (maybe False (os==))

netSpeedIs :: NetSpeed -> StoryGuard
netSpeedIs ns = asks pdNetSpeed >>= guard . (maybe False (ns==))

orgIs :: ByteString -> StoryGuard
orgIs o = asks pdOrg >>= guard . (maybe False (o==))

orgMatch :: ByteString -> StoryGuard
orgMatch o = asks pdOrg >>= guard . (maybe False (flip (=~) o))

ispIs :: ByteString -> StoryGuard
ispIs i = asks pdISP >>= guard . (maybe False (i==))

allocate :: MonadPlus m => m () -> a -> m a
allocate cnd r = cnd >> return r

giveThem :: MonadPlus m => m () -> m a -> m a
giveThem = (>>)
