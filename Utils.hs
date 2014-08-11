module Utils where 

import Import
import Data.Time
-------------------------------------------
--
-- Utilities for handling timestamps  
--
-------------------------------------------

-- Returns a Javascript compliant timestamp. 
-- Primarily, it is the amount of miliseconds from January 1st, 1970 until it is invoked. 
-- Note that in Javascript, Date.now() returns a timestamp expressed in miliseconds i.e. 1 * 10 ^ 3 seconds.

getTimestamp :: IO Integer
getTimestamp = do
  now <- getCurrentTime
  let ts =  truncate $ (diffUTCTime now time0) * 1000
  return ts
  
-- Same as getTimestamp but its return type is NominalDiffTime
getTimestamp' :: IO NominalDiffTime
getTimestamp' = do
  now <- getCurrentTime
  let ts =  (diffUTCTime now time0) * 1000
  return ts
    
-- Returns a UTCTime object, created from a given Integer value generated with the function Date.now() in Javascript
parseJSTimestamp :: Integer -> UTCTime 
parseJSTimestamp jstimestamp  = addUTCTime (fromInteger (jstimestamp `div` 1000)) time0 -- timestamps in Javascript are expressed in miliseconds. It must be divided by 1000




-- CONSTANT: Time object representing January 1st, 1970 at 00:00:00. 

time0 :: UTCTime
time0 = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
