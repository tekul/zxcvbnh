module Zxcvbn where

import Control.Applicative
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.List as L
--import qualified Data.List.Zipper as Z
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Entropy
import Token

type Password = String
type Dict = M.Map String  Int

type Matcher = Password -> [Match]

-- Apply a list of matchers to a password and get back the minimum
-- entropy and corresponding list of matches
zxcvbn :: [Matcher] -> Password -> (Double, [Match])
zxcvbn _ "" = (0.0, [])
zxcvbn ms p = minEntropyMatchSequence p $ ms >>= \m -> m p

type EntropyMatches = [(Double, Maybe Match)]

-- Finds the sequence of matches for the password which combine
-- to give the minimum entropy, including any brute-force matches
-- filling in the gaps
minEntropyMatchSequence :: String -> [Match] -> (Double, [Match])
minEntropyMatchSequence p matches = (minEntropy, matchSequence)
  where
    bfc = bruteForceCardinality p
    lgBfc = log2 $ fromIntegral bfc
    theEnd = length p - 1
    minEntropies = reverse $ L.foldl' minSeq [] [0..theEnd]
    minEntropy = fst $ head minEntropies
    matchSequence = case intersperseBruteForceMatches $ extractMatchSequence minEntropies [] of
                       []       -> [makeBruteForceMatch 0 theEnd]
                       ms@(m:_) -> if start m == 0
                                   then ms
                                   else makeBruteForceMatch 0 ((start m)-1) : ms

    extractMatchSequence :: EntropyMatches -> [Match] -> [Match]
    extractMatchSequence [] ms       = ms
    extractMatchSequence (em:ems) ms = case em of
                                       (_, Nothing) -> extractMatchSequence ems ms
                                       (_, Just m@(Match (Token _ i j) _ _))  -> extractMatchSequence (drop (j-i) ems) (m:ms)

    makeBruteForceMatch i j = Match (Token substring i j) (log2 . fromIntegral $ bfc ^ (j-i+1)) BruteForceMatch
      where
        substring = take (j-i+1) $ drop i p

    intersperseBruteForceMatches [] = []
    intersperseBruteForceMatches [m]
        | (end m) == theEnd = [m]
        | otherwise         = m : [makeBruteForceMatch (1 + end m) theEnd]
    intersperseBruteForceMatches (m1:m2:ms)
        | i == j    = m1 : intersperseBruteForceMatches (m2:ms)
        | otherwise = m1 : makeBruteForceMatch i (j-1) : intersperseBruteForceMatches (m2:ms)
      where
        i = 1 + end m1
        j = start m2

    minSeq :: EntropyMatches -> Int -> EntropyMatches
    minSeq acc 0   = [minimiseAtPos 0 acc (lgBfc, Nothing) matches]
    -- TODO: Use cons rather than ++ and access in reverse
    minSeq acc pos = acc ++ [(minimiseAtPos pos acc ((fst . last) acc + lgBfc, Nothing) matches)]


    minimiseAtPos :: Int -> EntropyMatches -> (Double, Maybe Match) -> [Match] -> (Double, Maybe Match)
    minimiseAtPos _   _       bestSoFar []      = bestSoFar
    minimiseAtPos pos minUpTo bestSoFar (m@(Match (Token _ i j) e _):ms) = minimiseAtPos pos minUpTo bestEntropyMatchAtPos ms
      where
        bestEntropyMatchAtPos
            | j /= pos                 = bestSoFar -- match doesn't end at current position
            | entropyWithMatch < fst bestSoFar = (entropyWithMatch, Just m) -- new best
            | otherwise                = bestSoFar
        entropyWithMatch = e + if i == 0 then 0 else fst $ minUpTo !! (i-1)


data Match = Match Token Double MatchType --  token, entropy
           deriving (Show, Eq)

start :: Match -> Int
start (Match (Token _ i _) _ _) = i

end :: Match -> Int
end (Match (Token _ _ j) _ _) = j

data MatchType = DictMatch String -- dictname
               | L33tMatch String String Int [(Char,Char)] --  token, dictname, unl33ted, rank, subs
               | SequenceMatch String Bool
               | RepeatMatch Char
               | BruteForceMatch
               | SpatialMatch Token String
               | DigitsMatch Token
               | YearMatch Token
               | DateMatch Token Int Bool
            deriving (Show, Eq)

pattern :: MatchType -> String
pattern DictMatch {} = "dictionary"
pattern L33tMatch {} = "dictionary"
pattern SequenceMatch {} = "sequence"
pattern RepeatMatch {} = "repeat"
pattern BruteForceMatch {} = "bruteforce"
pattern SpatialMatch {} = "spatial"
pattern DigitsMatch {} = "digits"
pattern YearMatch {} = "year"
pattern DateMatch {} = "date"

entropy :: MatchType -> Double
--entropy (L33tMatch (Token s _ _) _ _ rank subs) = log2 (fromIntegral rank) + extraUpperCaseEntropy s + extraL33tEntropy s subs
entropy SequenceMatch {} = undefined
entropy RepeatMatch {} = undefined
entropy BruteForceMatch {} = undefined
entropy SpatialMatch {} = undefined
entropy DigitsMatch {} = undefined
entropy YearMatch {} = undefined
entropy DateMatch {} = undefined

parseDict :: String -> Dict
parseDict src = foldl (\m (k,v) -> M.insert k v m) M.empty elts
  where ws = lines src
        elts  = zip ws [1..length ws]


dictMatcher :: String -> Dict -> Matcher
dictMatcher name dict password = dictMatch dict (tokenize password)
  where
    dictMatch d ts = mapMaybe (lookup d) ts
    meta = DictMatch name
    lookup d t@(Token w _ _) = fmap (createMatch t w) $ M.lookup (map toLower w) d
    createMatch t w rank = Match t (log2 (fromIntegral rank) + extraUpperCaseEntropy w) meta

-- Sequence Matching
data Sequence = Seq String String deriving (Show, Eq)

lowerCaseAlphabetic = Seq "lower" "abcdefghijklmnopqrstuvwxyz"
upperCaseAlhabetic  = Seq "upper" "ABCDEFGHIJ§KLMNOPQRSTUVWXYZ"
digits              = Seq "digits" "01234567890"

sequences = [lowerCaseAlphabetic, upperCaseAlhabetic, digits]

--sequenceMatches :: String -> [(String,String)] -> [Match]
-- sequenceMatches c1:c2:cs = candidateSequence c1 c2 sequences

--seqMatch :: String -> Zipper Char -> String
--seqMatch c:cs z =
--    | endp z = Nothing
--    | c == cursor z =


candidateSequence :: Char -> Char -> [Sequence] -> Maybe (Sequence, Int)
candidateSequence _  _  []     = Nothing
candidateSequence c1 c2 (s:ss) = case sequenceMatch s c1 c2 of
  Nothing        -> candidateSequence c1 c2 ss
  Just direction -> Just (s, direction)

sequenceMatch :: Sequence -> Char -> Char -> Maybe Int
sequenceMatch (Seq _ s) c1 c2 = (-) <$> L.elemIndex c2 s <*> L.elemIndex c1 s >>=
                          \relativePos -> case abs relativePos of
                                            1 -> Just relativePos
                                            _ -> Nothing

-- Repeat Matching
--

repeatMatches :: [Token] -> [Token]

repeatMatches [] = []
repeatMatches ts = removeSubTokens $ filter validRepeat ts

validRepeat :: Token -> Bool
validRepeat (Token w begin end) = end - begin + 1 > 2 && isRepeat w
  where
    isRepeat []   = True
    isRepeat (_:[]) = True
    isRepeat (c:cs) = c == head cs && isRepeat cs

-- L33t Matching

l33tTable = M.fromList
  [
    ('a', "4@"),
    ('b', "8"),
    ('c', "({[<"),
    ('e', "3"),
    ('g', "69"),
    ('i', "1!|"),
    ('l', "1|7"),
    ('o', "0"),
    ('s', "$5"),
    ('t', "+7"),
    ('x', "%"),
    ('z', "2")
  ]

l33tSubs :: M.Map Char String
l33tSubs = M.fromList $ zip k v
  where
    l33tFor l c = elem l $ fromJust $ M.lookup c l33tTable
    k = L.nub $ concat $ M.elems l33tTable
    v = map (\c -> filter (l33tFor c) $ M.keys l33tTable) k


