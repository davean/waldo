{-# LANGUAGE OverloadedStrings #-}
module Waldo.BrowserCap (
    BrowserCap
  , BrowserEntry(..)
  , loadBrowserCap
  , lookupBrowser
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as MapL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Cache.LRU.IO as LRU
import Data.Maybe
import Data.Cache.LRU.IO (AtomicLRU)
import Data.List
import Control.Monad
import Control.DeepSeq
import System.Timeout
--import Debug.Trace

data BrowserCap = BrowserCap {
    bcEntries :: ![BrowserEntry]
  , bcCache   :: AtomicLRU BS8.ByteString BrowserEntry
  }

instance NFData BrowserCap where

data MatchPart =
    TPart !T.Text
  | One
  | Many
  deriving (Eq, Ord, Show)

data BrowserEntry = BrowserEntry {
    beParent :: !T.Text
  , beUserAgent :: !T.Text
  , beUserAgentMatcher :: ![MatchPart]
  , beBrowser :: !T.Text
  , beVersion :: !T.Text
  , beMajorVersion :: !T.Text
  , beMinorVersion :: !T.Text
  , bePlatform :: !T.Text 
  , beWin16 :: !Bool
  , beWin32 :: !Bool
  , beWin64 :: !Bool
  , beFrames :: !Bool
  , beIFrames :: !Bool
  , beTables :: !Bool
  , beCookies :: !Bool
  , beBackgroundSounds :: !Bool
  , beJavaScript :: !Bool
  , beVBScript :: !Bool
  , beJavaApplets :: !Bool
  , beActiveXControls :: !Bool
  , beBanned :: !Bool
  , beMobileDevice :: !Bool
  , beSyndicationReader :: !Bool
  , beCrawler :: !Bool
  , beCSSVersion :: !T.Text
  , beAolVersion :: !T.Text
  , beMasterParent :: !Bool
  , beSortOrder :: !T.Text
  , beInternalID :: !T.Text
  } deriving (Eq, Ord, Show)

instance NFData BrowserEntry where
  
data BrowserEntryShim = BrowserEntryShim {
    besParent :: T.Text
  , besUserAgent :: T.Text
  , besUserAgentMatcher :: [MatchPart]
  , besBrowser :: T.Text
  , besVersion :: T.Text
  , besMajorVersion :: T.Text
  , besMinorVersion :: T.Text
  , besPlatform :: T.Text
  , besAlpha :: T.Text
  , besBeta  :: T.Text
  , besWin16 :: T.Text
  , besWin32 :: T.Text
  , besWin64 :: T.Text
  , besFrames :: T.Text
  , besIFrames :: T.Text
  , besTables :: T.Text
  , besCookies :: T.Text
  , besBackgroundSounds :: T.Text
  , besJavaScript :: T.Text
  , besVBScript :: T.Text
  , besJavaApplets :: T.Text
  , besActiveXControls :: T.Text
  , besBanned :: T.Text
  , besMobileDevice :: T.Text
  , besSyndicationReader :: T.Text
  , besCrawler :: T.Text
  , besCSSVersion :: T.Text
  , besAolVersion :: T.Text
  , besMasterParent :: T.Text
  , besSortOrder :: T.Text
  , besInternalID :: T.Text
  } deriving (Eq, Ord, Show)

-- This would be faster if it returned a list of offsets
-- and took the origional string and dropped the already-matched length.
-- That is because it would improve match simplification.
nextOptions :: [MatchPart] -> T.Text -> [T.Text]
nextOptions ((TPart t0):_) rest = maybeToList $ T.stripPrefix t0 rest
nextOptions (Many:(TPart t0):_) rest = map snd $ T.breakOnAll t0 rest
nextOptions (One:_) rest = maybeToList $ fmap snd $ T.uncons rest
nextOptions [Many] _ = [""]
nextOptions [] rest = if T.null rest then [""] else []
nextOptions mp _ = error $ "Failed match: " ++ show mp

isMatch :: T.Text -> [MatchPart] -> Bool
isMatch t mparts =
  let finalEnds = foldl (\ends mp -> concatMap (Set.toList . Set.fromList . nextOptions mp) ends) [t] $ tails mparts
  in "" `elem` finalEnds

toMatcher :: T.Text -> [MatchPart]
toMatcher t =
    optimize $ map toMatchPart $ T.unpack t
  where
    toMatchPart '?' = One
    toMatchPart '*' = Many
    toMatchPart  c  = TPart (T.pack [c])
    optimize ((TPart t0):(TPart t1):r) = optimize ((TPart (t0 `T.append` t1)):r)
    optimize (Many:Many:r) = optimize (Many : r)
    optimize (One:Many:r) = optimize (Many : r)
    optimize (Many:One:r) = optimize (Many : r)
    optimize (x:xs) = x : optimize xs
    optimize [] = []

lookupBrowser :: BrowserCap -> BS8.ByteString -> IO (Maybe BrowserEntry)
lookupBrowser BrowserCap {bcEntries=entries, bcCache=cacheRef} ua = do
    r <- timeout (10^(6::Int)) $ do
      cache <- LRU.lookup ua cacheRef
      case cache of
        Just be -> return $ Just be
        Nothing -> do
          case bestMatching of
            Nothing -> return Nothing
            Just be -> do
              LRU.insert ua be cacheRef
              return $ Just be
    return $ join r
  where
    bestMatching = listToMaybe $ map snd $ sortBy cmpByFst $ map (\be -> (T.length $ beUserAgent be, be)) allMatching
    allMatching = mapMaybe match entries
    cmpByFst a b = compare (fst b) (fst a)
    match :: BrowserEntry -> Maybe BrowserEntry
    match be =
      if isMatch (T.concat ["[", T.decodeUtf8 ua, "]"]) $ beUserAgentMatcher be
      then Just be
      else Nothing

loadBrowserCap :: FilePath -> IO BrowserCap
loadBrowserCap fn = do
  bcf <- T.readFile fn
  let bcl = drop 3 $ T.lines bcf
  let bce = catMaybes $ map (AP.maybeResult . AP.parse parseBCLine) bcl
  bccR <- LRU.newAtomicLRU (Just 16)
  let bces = force $ convertShims bce
  (force bces) `seq` return $
    BrowserCap { bcEntries = bces
               , bcCache = bccR
               }

parseBCLine :: AP.Parser BrowserEntryShim
parseBCLine = do
    parent <- parseQuoted
    _ <- AP.string ","
    userAgent <- parseQuoted
    _ <- AP.string ","
    browser <- parseQuoted
    _ <- AP.string ","
    version <- parseQuoted
    _ <- AP.string ","
    majorVersion <- parseQuoted
    _ <- AP.string ","
    minorVersion <- parseQuoted
    _ <- AP.string ","
    platform <- parseQuoted
    _ <- AP.string ","
    alpha <- parseQuoted
    _ <- AP.string ","
    beta <- parseQuoted
    _ <- AP.string ","
    win16 <- parseQuoted
    _ <- AP.string ","
    win32 <- parseQuoted
    _ <- AP.string ","
    win64 <- parseQuoted
    _ <- AP.string ","
    frames <- parseQuoted
    _ <- AP.string ","
    iFrames <- parseQuoted
    _ <- AP.string ","
    tables <- parseQuoted
    _ <- AP.string ","
    cookies <- parseQuoted
    _ <- AP.string ","
    backgroundSounds <- parseQuoted
    _ <- AP.string ","
    javaScript <- parseQuoted
    _ <- AP.string ","
    vBScript <- parseQuoted
    _ <- AP.string ","
    javaApplets <- parseQuoted
    _ <- AP.string ","
    activeXControls <- parseQuoted
    _ <- AP.string ","
    banned <- parseQuoted
    _ <- AP.string ","
    mobileDevice <- parseQuoted
    _ <- AP.string ","
    syndicationReader <- parseQuoted
    _ <- AP.string ","
    crawler <- parseQuoted
    _ <- AP.string ","
    cSSVersion <- parseQuoted
    _ <- AP.string ","
    aolVersion <- parseQuoted
    _ <- AP.string ","
    masterParent <- parseQuoted
    _ <- AP.string ","
    sortOrder <- parseQuoted
    _ <- AP.string ","
    internalID <- parseQuoted
    return $ BrowserEntryShim {
        besParent = parent
      , besUserAgent = userAgent
      , besUserAgentMatcher = toMatcher userAgent
      , besBrowser = browser
      , besVersion = version
      , besMajorVersion = majorVersion
      , besMinorVersion = minorVersion
      , besPlatform = platform
      , besAlpha = alpha
      , besBeta  = beta
      , besWin16 = win16
      , besWin32 = win32
      , besWin64 = win64
      , besFrames = frames
      , besIFrames = iFrames
      , besTables = tables
      , besCookies = cookies
      , besBackgroundSounds = backgroundSounds
      , besJavaScript = javaScript
      , besVBScript = vBScript
      , besJavaApplets = javaApplets
      , besActiveXControls = activeXControls
      , besBanned = banned
      , besMobileDevice = mobileDevice
      , besSyndicationReader = syndicationReader
      , besCrawler = crawler
      , besCSSVersion = cSSVersion
      , besAolVersion = aolVersion
      , besMasterParent = masterParent
      , besSortOrder = sortOrder
      , besInternalID = internalID
      }
  where
    parseQuoted :: AP.Parser T.Text
    parseQuoted = do
      _ <- AP.string "\""
      str <- AP.takeWhile (/='"')
      _ <- AP.string "\""
      return str

convertShims :: [BrowserEntryShim] -> [BrowserEntry]
convertShims shims =
  let convMap = MapL.fromList $ map (\bes -> (besUserAgent bes, mergeBE bes convMap)) shims
  in MapL.elems convMap

mergeBE :: BrowserEntryShim -> MapL.HashMap T.Text BrowserEntry -> BrowserEntry
mergeBE c converted =
    let ourparent = -- trace ("=============\ngetting parent for: " ++ (T.unpack $ besUserAgent c)) $ 
          if (besParent c) == "DefaultProperties"
          then Nothing -- base case
          else 
            if (besMasterParent c) == "true"
            then MapL.lookup "[DefaultProperties]" converted
            else MapL.lookup (T.concat ["[", besParent c, "]"]) converted
        be = BrowserEntry {
             beParent = besParent c
           , beUserAgent = besUserAgent c
           , beUserAgentMatcher = besUserAgentMatcher c 
           , beBrowser = mergeText (besBrowser c) (fmap beBrowser ourparent)
           , beVersion = mergeText (besVersion c) (fmap beVersion ourparent)
           , beMajorVersion = mergeText (besMajorVersion c) (fmap beMajorVersion ourparent)
           , beMinorVersion = mergeText (besMinorVersion c) (fmap beMinorVersion ourparent)
           , bePlatform = mergeText (besPlatform c) (fmap bePlatform ourparent)
           , beWin16 = mergeBool (besWin16 c) (fmap beWin16 ourparent)
           , beWin32 = mergeBool (besWin32 c) (fmap beWin32 ourparent)
           , beWin64 = mergeBool (besWin32 c) (fmap beWin64 ourparent)
           , beFrames = mergeBool (besFrames c) (fmap beFrames ourparent)
           , beIFrames = mergeBool (besIFrames c) (fmap beIFrames ourparent)
           , beTables = mergeBool (besTables c) (fmap beTables ourparent)
           , beCookies = mergeBool (besCookies c) (fmap beCookies ourparent)
           , beBackgroundSounds = mergeBool (besBackgroundSounds c) (fmap beBackgroundSounds ourparent)
           , beJavaScript = mergeBool (besJavaScript c) (fmap beJavaScript ourparent)
           , beVBScript = mergeBool (besVBScript c) (fmap beVBScript ourparent)
           , beJavaApplets = mergeBool (besJavaApplets c) (fmap beJavaApplets ourparent)
           , beActiveXControls = mergeBool (besActiveXControls c) (fmap beActiveXControls ourparent)
           , beBanned = mergeBool (besBanned c) (fmap beBanned ourparent)
           , beMobileDevice = mergeBool (besBanned c) (fmap beMobileDevice ourparent)
           , beSyndicationReader = mergeBool (besSyndicationReader c) (fmap beSyndicationReader ourparent)
           , beCrawler = mergeBool (besCrawler c) (fmap beCrawler ourparent)
           , beCSSVersion = mergeText (besCSSVersion c) (fmap beCSSVersion ourparent)
           , beAolVersion = mergeText (besAolVersion c) (fmap beAolVersion ourparent)
           , beMasterParent = mergeBool (besMasterParent c) (Just $ error "WTF?")
           , beSortOrder = besSortOrder c
           , beInternalID = besInternalID c
           }
    in -- trace ("\n\n" ++ show ourparent ++ "\n" ++ show be ++ "\n\n") $ 
       be
  where
    mergeText :: T.Text -> Maybe T.Text -> T.Text
    mergeText ours mparents =
      if not $ ours `elem` ["default", ""]
      then ours
      else case mparents of
        Nothing -> ""
        Just parents -> parents
    mergeBool :: T.Text -> Maybe Bool -> Bool
    mergeBool ours mparents =
      case ours of
        "true" -> True
        "false" -> False
        "True" -> True
        "False" -> False
        "default" ->
          case mparents of
            Nothing -> error $ "lacking bool parent"
            Just parents -> parents
        e -> error $ "unknown bool type: " ++ (T.unpack e)
