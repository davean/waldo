{-# LANGUAGE OverloadedStrings #-}
module Waldo.Script (
    Script(..)
  , PanelSizes, PanelData(..), Panel(..)
  , ImagePart(..)
  , TextPart(..)
  , Pos(..)
  , loadImagePanels, mkScript, scriptName
  ) where

import Data.List
import Control.Monad
import qualified Data.Text as T
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.ImageSize as CI
import System.FilePath ((</>), takeFileName, splitExtension)
import System.Path.Glob
import Data.Digest.Pure.SHA
import Control.DeepSeq

import Data.Aeson ((.=))

pad :: Int
pad = 4

panelRightEdge :: Panel -> Int
panelRightEdge p = (pX $ pPos p) + (pWidth p)

scriptName :: Script -> T.Text
scriptName (s@Script {}) = T.concat $ [sName s, " : "] ++ (intersperse "+" $ map pName (sPanels s))
scriptName (ScriptTo goto) = T.concat ["Goto : ", goto]

mkScript :: T.Text -- name
         -> T.Text -- alt-text
         -> [PanelData] -- panels!
         -> Script
mkScript nm alt pds =
  let ps = snd $ mapAccumL (\xstart p ->
                             let newp = panelData2panel xstart p
                             in (panelRightEdge newp, newp)) 0 pds
  in Script {
     sAltText = alt
   , sPanels  = ps
   , sHeight  = 2*pad + (maximum $ map pHeight ps)
   , sWidth   = (1+length ps)*pad + (sum $ map pWidth ps)
   , sName    = nm
   }

hashImgNm :: FilePath -> FilePath
hashImgNm fn =
  let (nm, typ) = splitExtension fn
  in (showDigest $ sha256 (BSL8.pack ("basfd" ++ nm)))++typ

loadImagePanels :: Int -- Story
                -> Int -- Panel
                -> Int -- Choice
                -> IO PanelSizes
loadImagePanels s p c = do
  fns <- glob ("panels" </>
               ("a1_"++show s++"p"++show p++"s*_"++show c++".*"))
  ps <- forM fns $ \fn -> do
    mImgInf <- runResourceT $ CB.sourceFile fn $$ CI.sinkImageSize
    case mImgInf of
      Nothing -> fail "Couldn't read image."
      Just sz -> do
        let pname = hashImgNm $ takeFileName fn
        d <- BSL8.readFile fn
        BSL8.writeFile ("loadedPanels" </> pname) d
        return $
          PanelData {
              pdWidth  = CI.width sz
            , pdHeight = CI.height sz
            , pdImages = [ImagePart { ipPos = Pos 0 0, ipImageUrl = T.pack pname }]
            , pdText   = []
            , pdName   = T.pack ("p"++show p++"s"++show s++"_"++show c)
            }
  if null ps
    then fail ("No panels found for "++show (s, p, c))
    else return ps

panelData2panel :: Int -> PanelData -> Panel
panelData2panel xlast pd = 
  Panel {
      pPos     = Pos (xlast+pad) pad
    , pWidth  = pdWidth  pd
    , pHeight = pdHeight pd
    , pImages = pdImages pd
    , pText   = pdText   pd
    , pName   = pdName pd
    }

type PanelSizes = [PanelData]

data Script =
    ScriptTo {
        sTarget :: !T.Text
      }
  | Script {
      sWidth   :: !Int
    , sHeight  :: !Int
    , sAltText :: !T.Text
    , sPanels  :: [Panel]
    , sName :: !T.Text
    }
  deriving (Eq, Ord, Show)

instance NFData Script where
  rnf (s@ScriptTo {sTarget=t}) = t `seq` s `seq` ()
  rnf (s@Script {sWidth=w, sHeight=h, sAltText=a, sPanels=p, sName=n}) =
    w `seq` h `seq` a `deepseq` p `deepseq` n `deepseq` s `seq` ()

data Panel = Panel {
    pPos    :: !Pos
  , pWidth  :: !Int
  , pHeight :: !Int
  , pImages :: [ImagePart]
  , pText   :: [TextPart]
  , pName   :: !T.Text
  } deriving (Eq, Ord, Show)

instance NFData Panel where
  rnf (pan@Panel {pPos=p, pWidth=w, pHeight=h, pImages=i, pText=t, pName=n}) =
    p `deepseq` w `seq` h `seq` i `deepseq` t `deepseq` n `deepseq` pan `seq` ()

data PanelData = PanelData {
    pdWidth  :: !Int
  , pdHeight :: !Int
  , pdImages :: [ImagePart]
  , pdText   :: [TextPart]
  , pdName   :: !T.Text
  } deriving (Eq, Ord, Show)

data ImagePart = ImagePart {
    ipPos      :: !Pos
  , ipImageUrl :: !T.Text
  } deriving (Eq, Ord, Show)

instance NFData ImagePart where
  rnf (i@ImagePart {ipPos=p, ipImageUrl=u}) =
    p `deepseq` u `deepseq` i `seq` ()

data TextPart = TextPart {
    tpPos    :: !Pos
  , tpString :: !T.Text
  , tpSize   :: !Float
  , tpFont   :: !T.Text
  , tpAngle  :: !Float
  } deriving (Eq, Ord, Show)

instance NFData TextPart where
  rnf (tp@TextPart {tpPos=p, tpString=t, tpSize=s, tpFont=f, tpAngle=a}) = 
    p `deepseq` t `deepseq` s `seq` f `deepseq` a `seq` tp `seq` ()

data Pos = Pos { pX :: !Int, pY :: !Int } deriving (Eq, Ord, Show)

instance NFData Pos where
  rnf (p@Pos {pX=x, pY=y}) = x `seq` y `seq` p `seq` ()

instance JS.ToJSON Script where
  toJSON (ScriptTo t) = JS.object ["goto" .= t]
  toJSON (Script w h alt ps _) = JS.object [ "width" .= w
                                           , "height" .= h
                                           , "alttext" .= alt
                                           , "panels" .= ps
                                           ]

instance JS.ToJSON Panel where
  toJSON (Panel p w h is ts _) = JS.object [ "pos" .= p
                                           , "width" .= w
                                           , "height" .= h
                                           , "images" .= is
                                           , "texts" .= ts
                                           ]

instance JS.ToJSON ImagePart where
  toJSON (ImagePart p url) = JS.object [ "pos" .= p, "url" .= url ]

instance JS.ToJSON TextPart where
  toJSON (TextPart p str sz f r) = JS.object [ "pos" .= p
                                             , "str" .= str
                                             , "size" .= sz
                                             , "font" .= f
                                             , "rad" .= r
                                             ]

instance JS.ToJSON Pos where
  toJSON (Pos x y) = JS.object [ "x" .= x, "y" .= y ]
