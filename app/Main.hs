module Main where

import Data.Map.Strict as M (findMin, singleton)
import System.Random.MWC (create, uniformVector)
import Text.Printf (printf)
import Lib

main :: IO ()
main = do
  gen1 <- create
  arrivalRandoms <- uniformVector gen1 simulation
  gen2 <- create
  serviceRandoms <- uniformVector gen2 simulation

  let arrivals = arrivalTimes (timeBetweenArrivalss arrivalRandoms)
      
      aggregatesYellow = averageAndMaximumWaitingTime arrivals serviceRandoms yellow
      averageWaitingTimeYellow = fst aggregatesYellow
      maximumWaitingTimeYellow = snd aggregatesYellow
      
      aggregatesRed = averageAndMaximumQueueLengths arrivals serviceRandoms red
      averageQueueLengthRed = fst aggregatesRed
      maximumQueueLengthRed = snd aggregatesRed

      diff = M.singleton "yellow" $ maximumWaitingTimeYellow - averageWaitingTimeYellow
      closest = fst $ M.findMin $ diff <> diffs arrivals serviceRandoms colorsMap
           
  printf "%s %.4f %s %.4f %s"
         "For yellow customers, the average and maximum waiting times are" 
         averageWaitingTimeYellow 
         "and" 
         maximumWaitingTimeYellow 
         "respectively\n"

  printf "%s %.4f %s %d %s" 
         "For red customers, the average and maximum queue lengths are" 
         averageQueueLengthRed 
         "and"
         maximumQueueLengthRed 
         "respectively\n"

  printf "%s %s %s"
         "The customer that gives the closest value between the average and maximum customer waiting times is"
         closest 
         "\n"          