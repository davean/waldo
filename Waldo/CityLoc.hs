module Waldo.CityLoc where

import Control.Monad.Reader
import Data.Geo.Coord
import Data.Geo.Sphere
import Data.Geo.Haversine

import Waldo.Stalk
import Waldo.Story

data CityData = CityData {
    cityLoc :: Coord
  , cityInfluenceKm :: Double
  }

mkCity :: Coord -> Double -> CityData
mkCity l i = CityData l i

closeTo :: CityData -> StoryGuard
closeTo c =
  asks pdLatLon >>= guard . (maybe False (\latlon ->
                                           let kmDist = (haversine earthMean (cityLoc c) latlon)/1000
                                           in kmDist < (cityInfluenceKm c)))

atlanta, belfast, boston, brisbane, cambridge, chicago, christchurch, cnu, dallas, detroit, downtownNYC, greenBay, halifax, houston, jerusalem, lakeChamplain, lakeErie, lakeMead, lakeMichigan, lasVegas, london, losAngeles, melbourne, miami, montreal, nyc, ottawa, paris, philadelphia, richmond, rioDeJaneiro, riverside, sacramento, sanAntonio, sanDiego, sanFran, scotland, seattle, sendai, sydney, tampa, telAviv, tokyo, toronto, vaBeach :: CityData

atlanta      = mkCity (( 33.755000) !.! (- 84.390000)) 20
belfast      = mkCity (( 54.600000) !.! (-  5.916700)) 6
boston       = mkCity (( 42.357778) !.! (- 71.061667)) 4
brisbane     = mkCity ((-27.466700) !.! ( 153.033300)) 180
cambridge    = mkCity (( 42.373611) !.! (- 71.110556)) 40
chicago      = mkCity (( 41.881944) !.! (- 87.627778)) 40
christchurch = mkCity ((-43.500000) !.! ( 172.600000)) 13
cnu          = mkCity (( 37.063800) !.! (- 76.494200)) 10
dallas       = mkCity (( 32.782778) !.! (- 96.803889)) 73
detroit      = mkCity (( 42.331389) !.! (- 83.045833)) 50
greenBay     = mkCity (( 44.513333) !.! (- 88.015833)) 180
halifax      = mkCity (( 44.654444) !.! (- 63.599167)) 17
houston      = mkCity (( 29.762778) !.! (- 95.383056)) 20
jerusalem    = mkCity (( 31.783300) !.! (  35.216700)) 4
lakeChamplain= mkCity (( 44.533333) !.! (- 73.333333)) 58
lakeErie     = mkCity (( 42.200000) !.! (- 81.200000)) 140
lakeMead     = mkCity (( 36.250000) !.! (-114.390000)) 50
lakeMichigan = mkCity (( 44.000000) !.! (- 87.000000)) 130
lasVegas     = mkCity (( 36.175000) !.! (-115.136389)) 20
london       = mkCity (( 51.517100) !.! (   0.106200)) 30
losAngeles   = mkCity (( 34.050000) !.! (-118.250000)) 65
melbourne    = mkCity ((-37.783300) !.! ( 144.966700)) 40
miami        = mkCity (( 25.787778) !.! (- 80.224167)) 30
montreal     = mkCity (( 45.500000) !.! (- 73.666667)) 45
downtownNYC  = mkCity (( 40.664167) !.! (- 73.938611)) 20
nyc          = mkCity (( 40.664167) !.! (- 73.938611)) 20
ottawa       = mkCity (( 45.420833) !.! (- 75.690000)) 50
paris        = mkCity (( 48.874200) !.! (   2.347000)) 25
philadelphia = mkCity (( 39.953333) !.! (- 75.170000)) 20
richmond     = mkCity (( 37.540972) !.! (- 77.432889)) 40
rioDeJaneiro = mkCity ((-22.908300) !.! (- 43.243600)) 70
riverside    = mkCity (( 33.948056) !.! (-117.396111)) 15
sacramento   = mkCity (( 38.555556) !.! (-121.468889)) 45
sanAntonio   = mkCity (( 29.416667) !.! (- 98.500000)) 23
sanDiego     = mkCity (( 32.715000) !.! (-117.162500)) 60
sanFran      = mkCity (( 37.779300) !.! (-122.419200)) 10
scotland     = mkCity (( 57.100000) !.! (-  4.000000)) 250
seattle      = mkCity (( 47.609722) !.! (-122.333056)) 45
sendai       = mkCity (( 31.816700) !.! ( 130.300600)) 35
sydney       = mkCity ((-33.868300) !.! ( 151.208600)) 60
tampa        = mkCity (( 27.947222) !.! (- 82.458611)) 42
telAviv      = mkCity (( 32.083300) !.! (  34.800000)) 10
tokyo        = mkCity (( 35.683300) !.! ( 139.766700)) 50
toronto      = mkCity (( 43.716589) !.! (- 79.340686)) 30
vaBeach      = mkCity (( 36.850600) !.! (- 75.977900)) 37
