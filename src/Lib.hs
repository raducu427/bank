module Lib
    ( simulation
    , timeBetweenArrivalss, arrivalTimes
    , yellow, red, blue, colorsMap, diffs
    , averageAndMaximumWaitingTime
    , averageAndMaximumQueueLengths
    ) where

import Data.Map.Strict as M (fromList, map)
import Data.Vector.Unboxed as V (foldr', map, prescanl', zipWith, zip, tail, sum, length, maximum, cons, empty)
import Data.Vector.Unboxed (Vector)

simulation :: Int
simulation = 100000

yellow, red, blue :: (Double, Double)
yellow = (2, 5)
red    = (2, 2)
blue   = (5, 1)

timeBetweenArrivalss :: Vector Double -> Vector Double
timeBetweenArrivalss = V.map (\p -> (-100) * log (1 - p))

arrivalTimes :: Vector Double -> Vector Double -- accumulating timeBetweenArrivals
arrivalTimes = V.prescanl' (+) 0

serviceTimes :: (Double, Double) -> Vector Double -> Vector Double
serviceTimes (alpha, beta) = V.map (\p -> 200 * p ** (alpha - 1) * (1 - p) ** (beta - 1))

startingServices :: Vector Double -> Vector Double -> Vector Double
startingServices arrivalTimes serviceTimes = 
  V.prescanl' (\ss (at, st) -> uncurry max (at, ss + st)) 0 $ V.zip (V.tail arrivalTimes) serviceTimes

finishingTimes :: Vector Double -> Vector Double -> Vector Double  -- startingService + serviceTime
finishingTimes = V.zipWith (+) 

isWaitings :: Vector Double -> Vector Double -> Vector Bool  -- arrivalTime == startingService?
isWaitings = V.zipWith (==)

waitingTimes' :: Vector Double -> Vector Double -> Vector Double  -- startingService - arrivalTime
waitingTimes' = V.zipWith (-)

waitingLines' :: Vector Bool -> Vector Int
waitingLines' bs = let (vs, i) = V.foldr' fun (V.empty, 0) bs in cons i vs
  where

    fun True  (vs, i) = (vs,         i + 1)
    fun False (vs, i) = (cons' i vs,     0)

    cons' 0 vs = vs
    cons' n vs = V.cons n vs

waitingLines arrivals serviceRandoms color =
  waitingLines' $ isWaitings arrivals $ startingServices arrivals $ serviceTimes color serviceRandoms
  
averageQueueLengths' :: Vector Int -> Double
averageQueueLengths' is = fromIntegral (V.sum is) / fromIntegral (V.length is)

averageAndMaximumQueueLengths arrivals serviceRandoms color =
  let queues = waitingLines arrivals serviceRandoms color  
  in  (averageQueueLengths' queues,  V.maximum queues)  

waitingTimes arrivals serviceRandoms color =
  waitingTimes' (startingServices arrivals (serviceTimes color serviceRandoms)) arrivals

averageWaitingTime' :: Vector Double -> Double
averageWaitingTime' ds = V.sum ds / fromIntegral (V.length ds)

averageAndMaximumWaitingTime arrivals serviceRandoms color =
  let waits = waitingTimes arrivals serviceRandoms color 
  in (averageWaitingTime' waits, V.maximum waits)

colorsMap = M.fromList [("red", red), ("blue", blue)]

diffs arrivals serviceRandoms = M.map (f arrivals serviceRandoms) 
  where
    f arrivals serviceRandoms color = 
      let aggregates = averageAndMaximumWaitingTime arrivals serviceRandoms color
      in snd aggregates - fst aggregates


