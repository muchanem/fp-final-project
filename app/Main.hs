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
beamMonteCarlo gen [] [] = ([], gen)
beamMonteCarlo gen [] (l:later) = beamMonteCarlo gen [l] later 
beamMonteCarlo gen (b:beam) later = beamMonteCarlo gen1 (beam ++ forBeam) (later ++ forLater)
    where s_level = 2 -- can update this to change per level if one wants
        expanded = updateCounts . expandNode b  -- generate all its children
        -- Randomly pick sLevel (2) children
        (picked, gen1) = pickRandom s_level gen
        forBeam = selectChildren picked (children expanded)
        -- The remaining children go to later
        notpicked = filter [0..6] `notElem` picked 
        forLater = selectChildren notpicked (children expanded)





-- | Score a node by win rate
winRate :: Node -> Double
winRate n 
    | visitCount n == 0 = 0.5
    | otherwise = winCount n `div` visitCount n

{-
Plan: https://www.lamsade.dauphine.fr/~cazenave/papers/beam.pdf

    beam ←{position}
    while true do
        nextBeam ←∅
        for b in beam do
            p ←b
            if there is a move to play in the best playout of p then
                play (p, move of the best playout)
            end if
            add p to nextBeam
            for move in possible moves of b do
                p ←b
                play (p, move)
                if level = 1 then
                    sample (p)
                else
                    beamMonteCarlo (p, level−1)
                end if
                add p to nextBeam
            end for
        end for
        if beam = nextBeam then
            break
        end if
        keep only the best s_level positions in nextBeam
        beam <- next beam
    end while
-}

