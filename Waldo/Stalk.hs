{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Waldo.Stalk (
    OS(..), Browser(..), NetSpeed(..)
  , PersonalData(..)
  , StalkRequest, wai2stalk
  , StalkDB, loadStalkDB
  , stalk
  ) where

import Data.Word
import Data.Bits
import Data.Maybe
import Control.Monad
import Data.List (intercalate)
import Data.Geolocation.GeoIP
import Data.ByteString.Char8 (ByteString)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), (.=), (.:), (.:?))
import Network.Socket (SockAddr(SockAddrInet, SockAddrInet6))
import Data.Either (rights)
import qualified Text.Regex.TDFA as R
import qualified Text.Regex.TDFA.ByteString as RB
import qualified Data.Aeson as JS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Wai as WAI
import qualified Network.HTTP.Types as HTTP
import qualified Data.HashMap.Strict as Map
import qualified Data.CaseInsensitive as CI
import Network.URI
import Data.Geo.Coord
import Safe

import Waldo.BrowserCap

data Browser = 
    Chrome
  | Safari
  | FireFox
  | InternetExplorer
  | Opera
  | Netscape
  deriving (Eq, Ord, Show)

data OS =
    BSD
  | Linux
  | Windows
  | Mac
  deriving (Eq, Ord, Show)

data NetSpeed =
    Dialup
  | Cellular
  | CableDSL
  | Corporate
  deriving (Eq, Ord, Show)

data PersonalData =
  PersonalData {
    pdLocal    :: [ByteString] -- Order of decreasing precision.
  , pdOrg      :: Maybe ByteString -- Who owns the IP.
  , pdISP      :: Maybe ByteString -- Who provides internet to the IP.
  , pdNetSpeed :: Maybe NetSpeed
  , pdReferer  :: Maybe ByteString
  , pdRefURI   :: Maybe URI
  , pdBrowser  :: Maybe Browser
  , pdOS       :: Maybe OS
  , pdLatLon   :: Maybe Coord
  , pdScreen   :: (Int, Int)
  , pdBrowserEntry :: Maybe BrowserEntry
  , pdStalk    :: StalkRequest
  } deriving (Eq, Show)

data StalkRequest = 
  StalkRequest {
      srParams :: HTTP.Query
    , srHeaders :: HTTP.RequestHeaders
    , srFromIP :: Maybe ByteString
    , srTrustForward :: Bool
    }
  deriving (Eq, Ord, Show)

instance ToJSON StalkRequest where
  toJSON (StalkRequest {srParams=p, srHeaders=h, srFromIP=ip, srTrustForward=t}) =
    JS.object [ "params"  .= p
              , "headers" .= map (\(k, v) -> (CI.original k, v)) h
              , "ip" .= ip
              , "trust_forward" .= t
              ]

instance FromJSON StalkRequest where
  parseJSON (JS.Object o) = do
    ip <- o .:? "ip"
    p  <- o .: "params"
    t  <- o .: "trust_forward"
    h  <- o .: "headers"
    return $ StalkRequest {
        srParams=p
      , srHeaders= map (\(k, v) -> (CI.mk k ,v)) h
      , srFromIP=ip
      , srTrustForward=t
      }
  parseJSON _ = mzero

data StalkDB =
  StalkDB {
      sdbBrowserCap  :: BrowserCap
    , sdbMaxMindCity :: GeoDB
    , sdbMaxMindOrg  :: GeoDB
    , sdbMaxMindISP  :: GeoDB
    , sdbMaxMindNet  :: GeoDB
    }

loadStalkDB :: IO StalkDB
loadStalkDB = do
  bc <- loadBrowserCap "datasets/browsercap.csv"
  cdb <- openGeoDB mmap_cache "datasets/GeoIPCity.dat"
  odb <- openGeoDB mmap_cache "datasets/GeoIPOrg.dat"
  idb <- openGeoDB mmap_cache "datasets/GeoIPISP.dat"
  ndb <- openGeoDB mmap_cache "datasets/GeoIPNet.dat"
  return $ StalkDB {
      sdbBrowserCap   = bc
    , sdbMaxMindCity  = cdb
    , sdbMaxMindOrg   = odb
    , sdbMaxMindISP   = idb
    , sdbMaxMindNet = ndb
    }

wai2stalk :: WAI.Request -> StalkRequest
wai2stalk req =
  StalkRequest {
      srParams  = WAI.queryString req
    , srHeaders = WAI.requestHeaders req
    , srFromIP  = ip
    , srTrustForward = True
    }
  where
    ip =
      case WAI.remoteHost req of
        SockAddrInet _ addr4 -> 
          let (x0, x1, x2, x3) = w32to8 addr4
          in Just $ BS8.pack $ concat [show x3, ".", show x2, ".", show x1, ".", show x0]
        SockAddrInet6 _ _ _ _ -> Nothing
        _ -> Nothing

stalk :: StalkDB -> StalkRequest -> IO PersonalData
stalk sdb req = do
    bc <- lookupBrowser (sdbBrowserCap sdb) $ fromMaybe "" agnt
    let mips = if srTrustForward req
                   -- This first one had a special key attached to avoid an issue
                   -- with injection of false Forward-Fors.
              then (maybeToList $ lookup "X-Forwarded-For" (srHeaders req)) ++
                   (map snd $ filter (\h -> (fst h) `elem` ["X-Forwarded-For", "X-Forward-For"]) (srHeaders req))
              else maybeToList $ srFromIP req
    let ips = mapMaybe validIP mips
    let browser = str2browser $ fromMaybe "" $ fmap beBrowser bc
    let os = str2os $ fromMaybe "" $ fmap bePlatform bc
    geos <- forM (ips) $ \ip -> do
      gipCityM <- geoLocateByIPAddress (sdbMaxMindCity sdb) ip
      gipOrgM  <- geoStringByIPAddress (sdbMaxMindOrg sdb)  ip
      gipISPM  <- geoStringByIPAddress (sdbMaxMindISP sdb)  ip
      gipNetM  <- geoStringByIPAddress (sdbMaxMindNet sdb) ip
      return $
        if not $ or [isJust gipCityM, isJust gipOrgM, isJust gipISPM, isJust gipNetM]
        then Nothing
        else Just $ PersonalData {
          pdLocal = (fromMaybe [] $ fmap city2locals gipCityM) ++ ["Earth"]
        , pdOrg = fmap cleanOrg gipOrgM
        , pdISP = gipISPM
        , pdNetSpeed = join $ fmap str2speed gipNetM
        , pdReferer = referer
        , pdRefURI = refUri
        , pdBrowser = browser
        , pdOS = os
        , pdLatLon = parseLatLon gipCityM
        , pdScreen = scrn
        , pdBrowserEntry = bc
        , pdStalk = req
        }
    return $ fromMaybe (noGeoResult bc browser os) $ listToMaybe $ catMaybes geos
  where
    parseLatLon gipc = do
      c <- gipc
      return ((geoLatitude c) !.! (geoLongitude c))
    city2locals :: GeoIPRecord -> [ByteString]
    city2locals g = [geoCity g, geoRegion g, geoCountryCode3 g, geoCountryName g, geoContinentCode g]
    noGeoResult bc browser os = 
      PersonalData {
          pdLocal = ["Earth"]
        , pdOrg = Nothing
        , pdISP = Nothing
        , pdNetSpeed = Nothing
        , pdReferer = referer
        , pdRefURI = refUri
        , pdBrowser = browser
        , pdOS = os
        , pdLatLon = Nothing
        , pdScreen = scrn
        , pdBrowserEntry = bc
        , pdStalk = req
        }
    parms = srParams req
    hdrs = srHeaders req
    scrn =
      let x = case BS8.readInt (fromMaybe "" $ join $ lookup "w" parms) of
            Nothing -> 0
            Just (xi, _) -> xi
          y = case BS8.readInt (fromMaybe "" $ join $ lookup "h" parms) of
            Nothing -> 0
            Just (yi, _) -> yi
      in (x, y)
    agnt = lookup "User-Agent" hdrs
    referer = join  $ lookup "r" parms
    refUri = join $ fmap (parseURI . BS8.unpack) referer
    validIP :: ByteString -> Maybe ByteString
    validIP fips0 = do
      (x0, fips1) <- BS8.readInt fips0
      (x1, fips2) <- BS8.readInt $ BS.drop 1 fips1
      (x2, fips3) <- BS8.readInt $ BS.drop 1 fips2
      (x3, _) <- BS8.readInt $ BS.drop 1 fips3
      return $ BS8.pack $ intercalate "." [show x0, show x1, show x2, show x3]
    rComp =
      R.CompOption {R.multiline=False,R.rightAssoc=True
                   ,R.caseSensitive=False,R.newSyntax=True,R.lastStarGreedy=False}
    rExec =
      R.ExecOption { R.captureGroups=False }
    rCompile (p, r) =
      case RB.compile rComp rExec p of
        Left e -> Left e
        Right c -> Right (c, r)
    clean :: [(ByteString, ByteString)] -> BS.ByteString -> BS.ByteString
    clean rules this = fromMaybe this $
                       fmap snd $ headMay $
                       filter (\(p, _) -> either (const False) isJust $ RB.regexec p this) $
                       rights $ map rCompile rules
    cleanOrg :: BS.ByteString -> BS.ByteString
    cleanOrg = clean [
        ("\\^Google", "Google")
      ]
    str2speed =
      flip Map.lookup (Map.fromList [
           ("Dialup", Dialup)
         , ("Cellular", Cellular)
         , ("Cable/DSL", CableDSL)
         , ("Corporate", Corporate)
         ])
    str2browser =
      flip Map.lookup (Map.fromList [
           ("Chrome"  , Chrome), ("Chromium", Chrome)
         , ("Safari", Safari)
         , ("Firefox", FireFox), ("Iceweasel", FireFox)
         , ("IE", InternetExplorer)
         , ("Opera", Opera), ("Opera Mini", Opera)
         , ("Netscape", Netscape)
         ])
    str2os =
      flip Map.lookup (Map.fromList [
          ("MacOSX", Mac)
        , ("Linux", Linux), ("Debian", Linux)
        , ("FreeBSD", BSD), ("NetBSD", BSD), ("OpenBSD", BSD)
        , ("IRIX", BSD), ("IRIX64", BSD)
        , ("HP-UX", BSD)
        , ("SunOS", BSD), ("Solaris", BSD)
        , ("WinCE", Windows)
        , ("Win16", Windows), ("Win32", Windows), ("Win64", Windows)
        , ("Win31", Windows)
        , ("Win95", Windows), ("Win98", Windows), ("WinME", Windows)
        , ("WinNT", Windows)
        , ("Win2000", Windows), ("Win2003", Windows)
        , ("WinXP", Windows), ("WinVista", Windows)
        , ("Win7", Windows), ("Win8", Windows)
        ])

w32to8 :: Word32 -> (Word8, Word8, Word8, Word8)
w32to8 w0 =
  let (w0_h, w0_l) = w32to16 w0
      ((x0, x1), (x2, x3)) = (w16to8 w0_h, w16to8 w0_l)
  in (x0, x1, x2, x3)

w32to16 :: Word32 -> (Word16, Word16)
w32to16 w0 =
  let w_h = fromIntegral $ w0 `shiftR` 16
      w_l = fromIntegral $ w0 .&. 0xFFFF
  in (w_h, w_l)

w16to8 :: Word16 -> (Word8, Word8)
w16to8 w0 =
  let w_h = fromIntegral $ w0 `shiftR` 8
      w_l = fromIntegral $ w0 .&. 0xFF
  in (w_h, w_l)
