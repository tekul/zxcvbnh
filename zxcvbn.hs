module Zxcvbn where

import Prelude hiding (words)
import Data.Char (toLower, isAlpha, isDigit)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (mapMaybe, fromJust)
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
               | L33tMatch String String [(Char,Char)] --  dictname, unl33ted, subs
               | SequenceMatch
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
lowerCaseAlphabetic, upperCaseAlhabetic, digits :: String
lowerCaseAlphabetic = "abcdefghijklmnopqrstuvwxyz"
upperCaseAlhabetic  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits              = "01234567890"

--sequences = [lowerCaseAlphabetic, upperCaseAlhabetic, digits]


sequenceMatcher :: [String] -> Matcher
sequenceMatcher seqs = sequenceMatches $ M.unions $ map createSequenceMap seqs

createSequenceMap :: String -> M.Map String Double
createSequenceMap s = M.fromList $ subSeqsWithEntropy ++ reverseSeqs
  where
    -- Use reversed for non-reversed seqs since we look them up backwards
    subSeqs = map reverse $ filter (not . L.null) $ concatMap L.tails $ L.inits s
    subSeqsWithEntropy = zip subSeqs (map seqEntropy subSeqs)
    reverseSeqs = map (\(sq,e) -> (reverse sq, e + 1.0)) subSeqsWithEntropy

    -- last rather than head since string is reversed
    seqEntropy sq = (baseEntropy (last sq)) + (log2 $ fromIntegral $ length sq)
    baseEntropy c
        | c == '1'  = 1.0
        | c == 'a'  = 1.0
        | isDigit c = log2 10
        | isAlpha c = log2 26
        | otherwise = 1.0 + log2 26

-- * Create a map/trie of all possible subsequences (fwd and reverse) for each sequence
-- * fold through the password accumulating a match, i,j and the list of matches
-- * when no match is found, and j-i > 2, create a match instance, set current match to "", i=j and continue

sequenceMatches :: M.Map String Double -> String -> [Match]
sequenceMatches ss password = maybe matches (\m -> m : matches) $ match stump k l
  where
    (stump, k, l, matches) = L.foldl' extendSequence ("", 0, 0, []) password
    extendSequence ("", i, j, ms) c = ([c], i, j+1, ms)
    extendSequence (sq, i, j, ms) c = case M.lookup (c:sq) ss of
                                              -- If lookup fails, we've
                                              -- gone as far as we can with
                                              -- the current sequence
                                              Nothing  -> let newMatches = maybe ms (\m -> m:ms) $ match sq i j
                                                          in ([c], j, j+1, newMatches)
                                              -- Otherwise try to extend it
                                              _        -> (c:sq, i, j+1, ms)
    match sq i j
      | j-i > 2   = fmap (\e -> Match (Token (reverse sq) i (j-1)) e SequenceMatch) $ M.lookup sq ss
      | otherwise = Nothing

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

l33tMatcher :: [Matcher] -> Matcher
l33tMatcher matchers password
    | null singleSubs = [] -- No l33t substitutions found
    | otherwise = L.nub matches -- TODO: Use more efficent nub
    -- Perform substitutions into password to get list of (possibly only
    -- one) unl33ted words. Look each of them up in the list of matchers
    -- and filter out any match tokens which don't actually contain
    -- subsitutions
  where
    (unl33ted, allSubs, multiSubs) = L.foldl' unl33t ([],[],[]) password
    singleSubs = L.nub allSubs

    wordsToLookup = substitute [reverse unl33ted] multiSubs

    matches = do
             word    <- wordsToLookup
             matcher <- matchers
             Match (Token w i j) e matchType <- matcher word
             let l33tToken = take (j-i+1) $ drop i password
             if map toLower l33tToken == w
               then [] -- No l33t chars in token since it is the same as the dictionary match
               else let isUsed (c,c') = case L.elemIndex c l33tToken of
                                          Nothing -> False
                                          Just k -> w !! k == c'
                        usedSubs = filter isUsed singleSubs
                        dictName = case matchType of
                                     DictMatch name -> name
                                     _              -> "unknown" -- TODO: Sort this type stuff out somehow

                    in return $ Match (Token l33tToken i j) (e + extraL33tEntropy l33tToken usedSubs) (L33tMatch dictName w usedSubs)



    substitute :: [String] -> [(Char, String)] -> [String]
    substitute words []     = words
    substitute words [sub]  = words >>= \w -> doMultiSub w sub
    substitute words (s:ss) = substitute (words >>= \w -> doMultiSub w s) ss

    doMultiSub word (c, ss) = map (replace word c) ss

    unl33t :: (String, [(Char,Char)], [(Char,String)]) -> Char -> (String, [(Char,Char)], [(Char,String)])
    unl33t (ul, ss, ms) c = case M.lookup c l33tSubs of
                              Nothing -> (c:ul, ss, ms)
                              Just [c'] -> (c':ul, (c,c'):ss, ms)
                              Just cs -> (c:ul, (map (\sub -> (c, sub)) cs) ++ ss, (c, cs): ms)


    replace :: String -> Char -> Char -> String
    replace word c c' = let r x
                              | x == c    = c'
                              | otherwise = x
                        in map r word

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


