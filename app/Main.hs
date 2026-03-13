module Main where

import System.Random (StdGen, randomR, newStdGen)
import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import GameState


main :: IO ()
main = do
    gen <- newStdGen

-- | Pick n random numbers in (0,6) from a generator
pickRandom :: Int -> StdGen -> ([Int], StdGen)
pickRandom 0 gen = ([], gen)
pickRandom n gen = ((r:rs), gen2) where
    r gen1 = randomR [0..6] gen
    rs gen2 = pickRandom n-1 gen1 

-- | Update a node's visit and win counts based on its game result
updateCounts :: Node -> Node
updateCounts n = n {visitCount = visitCount + 1,
                    winCount = winCount + k where
                        k | gameResult n == Win One = 1 | otherwise = 0
                   }

-- | Select children from a node's child map by column index
selectChildren :: [Int] -> Map.Map Move Node -> [Node]
selectChildren cols childMap = [c | col <- cols, Just c <- Map.lookup col childMap)]

beamMonteCarlo :: StdGen -> [Node] -> [Node] -> ([Node], StdGen)
beamMonteCarlo gen [] []            = ([], gen)
beamMonteCarlo gen [] (l : later)   = beamMonteCarlo gen [l] later
beamMonteCarlo gen (b : beam) later = beamMonteCarlo gen1 (beam ++ forBeam) (later ++ forLater)
    where
        sLevel           = 2
        expanded         = backprop b
        childMap         = case children expanded of
                             Just m  -> m
                             Nothing -> Map.empty
        (picked, gen1)   = pickWeighted sLevel gen childMap
        forBeam          = picked
        notPicked        = filter (`notElem` picked) (Map.elems childMap)
        forLater         = notPicked

backprop :: Node -> Node
backprop n
    | gameResult n /= Ongoing = updateCounts n
    | visitCount n == 0       = updateCounts (expandNode n)
    | otherwise               = updateCounts $ n
        { visitCount = visitCount n + subtreeVisits
        , winCount   = winCount n + subtreeWins
        , children   = Just updatedChildren
        }
    where
        childMap        = case children n of
                            Just m  -> m
                            Nothing -> Map.empty
        updatedChildren = Map.map backprop childMap
        subtreeWins     = sum (map winCount   (Map.elems updatedChildren))
        subtreeVisits   = sum (map visitCount (Map.elems updatedChildren))



-- | Score a node by win rate
winRate :: Node -> Double
winRate n 
    | visitCount n == 0 = 0.5
    | otherwise = winCount n `div` visitCount n


