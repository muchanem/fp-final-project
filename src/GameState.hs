module GameState where

import Data.Array (Array, (!), (//), listArray)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (Left, Right)
import Data.Maybe (isNothing)
import Control.DeepSeq (NFData, rnf)

-- | Connect 4 board dimensions
numRows, numCols :: Int
numRows = 6
numCols = 7

-- | A move is a column index (0 through 6)
type Move = Int

-- | Each cell is either empty or occupied by a player
type Cell = Maybe Player

-- | The two players
data Player = One | Two
  deriving (Show, Eq, Ord)

instance NFData Player where
  rnf One = ()
  rnf Two = ()

-- | Swap to the other player
otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

-- | The result of a game at any point
data GameResult = Win Player | Draw | Ongoing
  deriving (Show, Eq)

instance NFData GameResult where
  rnf (Win p) = rnf p
  rnf Draw    = ()
  rnf Ongoing = ()

-- | A Connect 4 board. The grid is indexed by (column, row).
--   Row 0 is the bottom.
data Board = Board
  { grid :: !(Array (Int, Int) Cell),
    currentPlayer :: !Player
  }
  deriving (Show, Eq)

emptyBoard :: Int -> Int -> Player -> Board
emptyBoard ncol nrow p = Board { 
  grid = listArray ((0, 0), (ncol - 1, nrow - 1)) (repeat Nothing)
  , currentPlayer = p
  }

-- | An MCTS tree node
data Node = Node
  { board :: !Board,
    gameResult :: !GameResult,
    visitCount :: !Int,
    winCount :: !Double,
    children :: !(Maybe (Map Move Node))
  }
  deriving (Show)

-- | Get all legal moves (columns that aren't full)
legalMoves :: Board -> [Move]
legalMoves b = filter (\col -> isNothing (grid b ! (col, numRows - 1))) [0 .. numCols - 1]

-- | Cardinal and diagonal directions on the board
data Direction = Up | Down | Left | Right | UpRight | UpLeft | DownRight | DownLeft
  deriving (Show, Eq)

-- | Convert a direction to (column delta, row delta)
dirDelta :: Direction -> (Int, Int)
dirDelta Up = (0, 1)
dirDelta Down = (0, -1)
dirDelta Left = (-1, 0)
dirDelta Right = (1, 0)
dirDelta UpRight = (1, 1)
dirDelta UpLeft = (-1, 1)
dirDelta DownRight = (1, -1)
dirDelta DownLeft = (-1, -1)

-- | Check if the cell adjacent to a position in the given direction
--   is occupied by the specified player. Returns False if out of bounds.
checkDirection :: Board -> Player -> (Int, Int) -> Direction -> Bool
checkDirection b player (c, r) dir =
  let (dc, dr) = dirDelta dir
      c' = c + dc
      r' = r + dr
   in c' >= 0
        && c' < numCols
        && r' >= 0
        && r' < numRows
        && grid b ! (c', r') == Just player

-- | Check the game result after a piece was placed at the given position.
--   Only checks lines through that position.
checkResultFast :: Board -> (Int, Int) -> GameResult
checkResultFast b pos =
  let player = case grid b ! pos of
        Just p -> p
        Nothing -> error "checkResultFast: empty cell"
      -- Count consecutive same-player pieces in a direction from pos
      countDir dir =
        let (dc, dr) = dirDelta dir
         in go pos dc dr
        where
          go (c, r) dc dr
            | checkDirection b player (c, r) dir = 1 + go (c + dc, r + dr) dc dr
            | otherwise = 0
      axes :: [Int]
      axes =
        [ countDir Up + countDir Down, -- vertical
          countDir Left + countDir Right, -- horizontal
          countDir UpRight + countDir DownLeft, -- diagonal /
          countDir UpLeft + countDir DownRight -- diagonal \
        ]
   in if any (>= 3) axes -- + 1 for the piece itself = 4
        then Win player
        else
          if null (legalMoves b)
            then Draw
            else Ongoing

-- | Apply a move to the board. Returns the updated board and the game result.
-- NOTE: only pass legal moves, will be undefined behavior if not
  -- RESOLVED: will just skip turn if illegal move proposed (hopefully)
applyMove :: Board -> Move -> (Board, GameResult)
applyMove b col =
  let row = head [r | r <- [0 .. numRows - 1], isNothing (grid b ! (col, r))]
      pos = (col, row)
      player = currentPlayer b
      grid' = grid b // [(pos, Just player)]
      b'
        | elem col (legalMoves b) = Board {grid = grid', currentPlayer = otherPlayer player}
        | otherwise = b {currentPlayer = otherPlayer player}
      result = checkResultFast b' pos
   in (b', result)

-- | Create a fresh unexpanded node from a board and its game result
mkNode :: Board -> GameResult -> Node
mkNode b result =
  Node
    { board = b,
      gameResult = result,
      visitCount = 0,
      winCount = 0.0,
      children = Nothing
    }

-- | Expand a node by generating child nodes for all legal moves.
--   Returns the node unchanged if already expanded or terminal.
expandNode :: Node -> Node
expandNode node
  | Just _ <- children node = node -- already expanded
  | gameResult node /= Ongoing = node -- terminal
  | otherwise =
      let moves = legalMoves (board node)
          mkChild m =
            let (b', result) = applyMove (board node) m
             in (m, mkNode b' result)
          childMap = Map.fromList (map mkChild moves)
       in node {children = Just childMap}
