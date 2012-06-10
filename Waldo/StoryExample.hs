{-# LANGUAGE OverloadedStrings #-}
module Waldo.StoryExample (
    loadScriptGen
  ) where

import Control.Monad
import Data.ByteString.Char8 ()
import Data.Text ()

import Waldo.Stalk
import Waldo.Script
import Waldo.Story
import Waldo.CityLoc

loadScriptGen :: IO (PersonalData -> IO Script)
loadScriptGen = do
  defaultScript <- loadDefaultScript
  stories <- story1example
  return $ selectStory (knapsackSizer 100) defaultScript stories

-- If they really defeat our snooping, they get this one.
loadDefaultScript :: IO Script
loadDefaultScript = do
    p1 <- loadImagePanels 1 1 0
    p2 <- loadImagePanels 1 2 0
    p3 <- loadImagePanels 1 3 0
    p4 <- loadImagePanels 1 4 0
    return $ mkScript "failback" alt $ map head [p1, p2, p3, p4 ]
  where
    alt = "This is the testiest test ever!"

story1example :: IO [StoryOption]
story1example = do
  s1p1c0 <- loadImagePanels 1 1 0
  s1p1c1 <- loadImagePanels 1 1 1
  s1p1c2 <- loadImagePanels 1 1 2
  s1p1c3 <- loadImagePanels 1 1 3
  
  s1p2 <- loadImagePanels 1 2 0
              
  s1p3 <- loadImagePanels 1 3 0
  
  s1p4c0 <- loadImagePanels 1 4 0
  s1p4c1 <- loadImagePanels 1 4 1
    
  return [
      do
         isIn "NA" -- Only for North Americans
         p1 <- msum [ orgIs "Massachusetts Institute of Technology" `allocate` s1p1c1
                    , closeTo sydney `allocate` s1p1c2
                    , closeTo sanFran `allocate` s1p1c3
                    , return s1p1c0
                    ]
         p4 <- msum [ osIs BSD `allocate` s1p4c1
                    , osIs Linux `allocate` s1p4c0
                    ]
         return $ Story {
             storyAltText   = "Alt"
           , storyPanelSets = [ p1, s1p2, s1p3, p4 ]
           , storyPadX = 0, storyPadY = 0
           , storyName = "s01"
           }
    ]
