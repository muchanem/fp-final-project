module Main where

import System.Random (StdGen, randomR, newStdGen, splitGen)
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies (using, parList, rdeepseq)
import GameState

-- | A path from root to a leaf node
type Path = [Move]

maxIterations :: Int
maxIterations = 3

main :: IO ()
main = do
    gen <- newStdGen
    let rootNode = mkNode (emptyBoard numCols numRows One) Ongoing
        (tree, _) = beamMonteCarlo gen maxIterations [rootNode] []
    putStrLn $ "Iterations: " ++ show maxIterations
    putStrLn $ "Root visits: " ++ show (visitCount tree)
    putStrLn $ "Root win rate: " ++ show (winRate tree)
    putStrLn "Best move scores:"
    case children tree of
        Nothing -> putStrLn "  (no children)"
        Just cs -> mapM_ (\(m, c) ->
            putStrLn $ "  col " ++ show m
                    ++ ": " ++ show (winRate c)
                    ++ " (" ++ show (visitCount c) ++ " visits)")
            (Map.toList cs)

-- | Random rollout: play random moves from a board until terminal.
--   Returns the game result.
rollout :: StdGen -> Board -> (GameResult, StdGen)
rollout gen b =
    let moves = legalMoves b
    in if null moves
       then (Draw, gen)
       else let (i, gen1) = randomR (0, length moves - 1) gen
                move      = moves !! i
                (b', res) = applyMove b move
            in case res of
                Ongoing -> rollout gen1 b'
                _       -> (res, gen1)

-- | Pick n random numbers in (0, numCols-1) from a generator
pickRandom :: Int -> StdGen -> ([Int], StdGen)
pickRandom 0 gen = ([], gen)
pickRandom n gen = (r : rs, gen2)
  where
    (r, gen1)  = randomR (0, numCols - 1) gen
    (rs, gen2) = pickRandom (n - 1) gen1

-- | Update a node's visit and win counts based on its game result
updateCounts :: Node -> Node
updateCounts n = n { visitCount = visitCount n + 1
                   , winCount   = winCount n + k }
  where k | gameResult n == Win One = 1
          | gameResult n == Draw    = 0.5
          | otherwise               = 0

-- | Select children from a node's child map by column index
selectChildren :: [Int] -> Map.Map Move Node -> [Node]
selectChildren cols childMap = [c | col <- cols, Just c <- [Map.lookup col childMap]]

-- | Beam Monte Carlo search with iteration limit
beamMonteCarlo :: StdGen -> Int -> [Node] -> [Node] -> (Node, StdGen)
beamMonteCarlo gen _ [] []             = (mkNode (emptyBoard numCols numRows One) Ongoing, gen)
beamMonteCarlo gen iters [] (l : later) = beamMonteCarlo gen iters [l] later
beamMonteCarlo gen 0 (b : _) _         = (b, gen)
beamMonteCarlo gen iters (b : beam) later = beamMonteCarlo gen2 (iters - 1) (expanded : beam ++ forBeam) (later ++ forLater)
    where
        sLevel           = 2
        (expanded, gen1) = backpropParallel gen b
        childMap         = case children expanded of
                             Just m  -> m
                             Nothing -> Map.empty
        (picked, gen2)   = pickRandom sLevel gen1
        forBeam          = selectChildren picked childMap
        notPicked        = Map.elems (Map.filterWithKey (\k _ -> k `notElem` picked) childMap)
        forLater         = notPicked

-- | Backpropagate: expand leaves, run rollouts, propagate stats up
backprop :: StdGen -> Node -> (Node, StdGen)
backprop gen n
    | gameResult n /= Ongoing =
        (updateCounts n, gen)
    | Nothing <- children n =
        let expanded = expandNode n
            (res, gen1) = rollout gen (board n)
            expanded' = expanded { gameResult = res }
        in (updateCounts expanded', gen1)
    | otherwise =
        let Just childMap = children n
            (gen1, updatedChildren) = Map.mapAccum (\g c -> let (c', g') = backprop g c in (g', c')) gen childMap
            subtreeWins   = sum (map winCount   (Map.elems updatedChildren))
            subtreeVisits = sum (map visitCount (Map.elems updatedChildren))
            n' = n { visitCount = visitCount n + subtreeVisits
                   , winCount   = winCount n + subtreeWins
                   , children   = Just updatedChildren
                   }
        in (n', gen1)

-- | Split a generator into n independent generators
splitGens :: Int -> StdGen -> ([StdGen], StdGen)
splitGens 0 gen = ([], gen)
splitGens n gen = (g1 : gs, gen'')
  where
    (g1, gen')  = splitGen gen
    (gs, gen'') = splitGens (n - 1) gen'

-- | Collect all unexpanded Ongoing leaves from a tree, expanding them.
--   Returns the updated tree and a list of (path, board) for rollout.
collectLeaves :: Node -> (Node, [(Path, Board)])
collectLeaves n
    | gameResult n /= Ongoing = (n, [])
    | Nothing <- children n =
        let expanded = expandNode n
        in case children expanded of
            Nothing -> (expanded, [])
            Just cm -> (expanded, [([m], board c) | (m, c) <- Map.toList cm])
    | Just childMap <- children n =
        let (leaves, childMap') = Map.mapAccumWithKey collectChild [] childMap
        in (n { children = Just childMap' }, leaves)
  where
    collectChild acc m c =
        let (c', ls) = collectLeaves c
        in (acc ++ map (\(p, b) -> (m:p, b)) ls, c')

-- | Score a game result from Player One's perspective
score :: GameResult -> Double
score (Win One) = 1.0
score Draw      = 0.5
score _         = 0.0

-- | Propagate a single rollout result along a path, updating counts at each node
propagateResult :: GameResult -> Path -> Node -> Node
propagateResult res [] n =
    n { visitCount = visitCount n + 1
      , winCount   = winCount n + score res }
propagateResult res (m:ms) n =
    case children n of
        Just childMap ->
            let child  = childMap Map.! m
                child' = propagateResult res ms child
            in n { visitCount = visitCount n + 1
                 , winCount   = winCount n + score res
                 , children   = Just (Map.insert m child' childMap) }
        Nothing -> n

-- | Parallel backprop: collect leaves, rollout in parallel, propagate results
--   single-threaded. Uses the same tree walk as backprop but with parallel rollouts.
backpropParallel :: StdGen -> Node -> (Node, StdGen)
backpropParallel gen node =
    let (expanded, leaves) = collectLeaves node
        (gens, gen')       = splitGens (length leaves) gen
        -- Parallel rollouts: each leaf gets its own independent generator
        results            = map (\(g, (_p, b)) -> fst (rollout g b))
                                 (zip gens leaves)
                             `using` parList rdeepseq
        -- Single-threaded propagation of results back up the tree
        paths              = map fst leaves
        tree'              = foldl (\t (p, r) -> propagateResult r p t) expanded (zip paths results)
    in (tree', gen')

-- | Score a node by win rate
winRate :: Node -> Double
winRate n
    | visitCount n == 0 = 0.5
    | otherwise         = winCount n / fromIntegral (visitCount n)
