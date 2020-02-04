-- To deal with time in Haskell, see http://two-wrongs.com/haskell-time-library-tutorial

module PresentTime where

import Data.Time.Clock
import Data.Time.LocalTime

getPresentTime :: Int -> IO (Int, Int, Int)
getPresentTime diff = do
  time <- getCurrentTime
  timezone <- getCurrentTimeZone
  let (TimeOfDay h m s) = localTimeOfDay $ utcToLocalTime timezone $ addUTCTime (realToFrac diff) time
  return (h, m, fromInteger $ floor s)

getDiffTime :: Int -> (Int, Int, Int)
getDiffTime diff =
  let (TimeOfDay h m s) = timeToTimeOfDay $ realToFrac diff in
  (h, m, fromInteger $ floor s)
  
