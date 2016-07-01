{-# LANGUAGE OverloadedStrings #-}

module Adjacency
  ( AdjacencyGraph
  , buildGraph
  , spatialMatcher
  , qwerty
  )
where

import           Control.Monad (join)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import Types
import Entropy

data AdjacencyGraph = AdjacencyGraph
  { name :: !String
  , graph :: !(M.Map Char [Maybe Text])
  , avgDegree :: !Double
  , startPositions :: !Int
  } deriving (Show)

type Position = Int
type Turns = Int
type Direction = Int
type ShiftedCount = Int

qwerty :: AdjacencyGraph
qwerty = buildGraph "qwerty"
    [ "`~ 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) -_ =+"
    ,     "qQ wW eE rR tT yY uU iI oO pP [{ ]} \\|"
    ,      "aA sS dD fF gG hH jJ kK lL ;: '\""
    ,       "zZ xX cC vV bB nN mM ,< .> /?"
    ]
    True

spatialMatcher :: AdjacencyGraph -> Matcher
spatialMatcher g password = findMatches g 0 (T.unpack password)

buildGraph :: String -> [Text] -> Bool -> AdjacencyGraph
buildGraph nm rows slanted = AdjacencyGraph nm g avgD nStarts
  where
    avgD = M.foldl' (\t adj -> t + degree adj) 0.0 g / fromIntegral nStarts
    degree = fromIntegral . length . filter (Nothing /=)
    nStarts = M.size g
    g = M.fromList $ do
        ((x,y), t) <- M.toList positionTable
        c          <- T.unpack t
        return (c, map (`M.lookup` positionTable) $ adjacentCoords x y)

    adjacentCoords :: Int -> Int -> [(Int, Int)]
    adjacentCoords x y
      | slanted   = [(x-1, y), (x, y-1), (x+1, y-1), (x+1, y), (x, y+1), (x-1, y+1)]
      | otherwise = [(x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1), (x+1, y), (x+1, y+1), (x, y+1), (x-1, y+1)]
    positionTable = M.fromList . join $ zipWith parseRow [0..] rows
    parseRow y row = let xIndent = if slanted && y > 0 then 1 else 0
                      in map (\(x, t) -> ((x + xIndent, y), t)) $ zip [0..] (T.splitOn " " row)

adjacentMatch :: AdjacencyGraph -> Char -> Char -> Maybe (Direction, ShiftedCount)
adjacentMatch ag c1 c2 = M.lookup c1 g >>= findAdjacent 0
  where
    g = graph ag
    findAdjacent :: Int -> [Maybe Text] -> Maybe (Direction, ShiftedCount)
    findAdjacent _ []               = Nothing
    findAdjacent dir (Nothing : as) = findAdjacent (dir+1) as
    findAdjacent dir (Just a  : as) = case T.findIndex (== c2) a of
        Nothing -> findAdjacent (dir+1) as
        Just i  -> Just (dir, if i > 0 then 1 else 0)


-- TODO: Write a (Vector Char) version which uses indexing rather than pattern-matching
-- on String, and compare performance
findMatches :: AdjacencyGraph -> Position -> String -> [Match]
findMatches ag i (c1:c2:cs) = case adjacentMatch ag c1 c2 of
    Nothing         -> findMatches ag (i+1) (c2:cs)
    Just (dir, sh)  -> let (m, j, nShift, nTurns, r) = extendMatch (i+1) dir (0 :: Position) sh [c1] c2 cs
                           remainingMatches = findMatches ag (j+1) r
                           Just firstShift = snd <$> adjacentMatch ag c2 c1
                        in if j - i > 1
                               then mkMatch m j (nShift + firstShift) nTurns : remainingMatches
                               else remainingMatches

  where
    mkMatch s j nShift nTurns = let t = Token ((T.reverse . T.pack) s) i j
                                    e = entropy ag (j - i + 1) (nTurns + 1) nShift
                                 in Match t e (SpatialMatch (name ag))
    -- Try to extend the match from the current valid position 'j'
    extendMatch j dir nTurns nShift s x r@(x1:xs) = case adjacentMatch ag x x1 of
        Nothing       -> (x:s, j, nShift, nTurns, r)
        Just (newDir, sh)  -> let newTurns = nTurns + (if newDir /= dir then 1 else 0)
                               in extendMatch (j+1) newDir newTurns (nShift + sh) (x:s) x1 xs
    extendMatch j _ nTurns nShift s x [] = (x:s, j, nShift, nTurns, [])
findMatches _ _ _ = []

entropy :: AdjacencyGraph -> Int -> Turns -> ShiftedCount -> Double
entropy g l nTurns nShift = shiftEntropy nShift l + logBase 2 (foldl' (\e j -> e + possibilities g nTurns j) 0 [2..l])

-- All spatial patterns are assumed to start with a turn, so t cannot be zero.
possibilities :: AdjacencyGraph -> Turns -> Int -> Double
possibilities g t l = foldl' (\p j -> p + fromIntegral (nCk (l-1) (j-1)) * s * d ^ j) 0 [1..nPossibleTurns]
  where
    s = fromIntegral (startPositions g)
    d = avgDegree g
    nPossibleTurns = min t (l-1)

shiftEntropy :: ShiftedCount-> Int -> Double
shiftEntropy 0 _ = 0.0 -- TODO: check zxcvbn behaviour for all shifted or none
shiftEntropy n l = logBase 2 $ foldl' (\e j -> e + fromIntegral (nCk l j)) 0 [1..min n nUnshifted]
  where
    nUnshifted = l - n
