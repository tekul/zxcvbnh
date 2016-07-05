{-# LANGUAGE OverloadedStrings #-}
module Zxcvbn
  ( zxcvbn
  , dictMatcher
  , l33tMatcher
  , repeatMatcher
  , sequenceMatcher
  , lowerCaseAlphabetic
  , upperCaseAlphabetic
  , digits
  , module Types
  )
where

import           Prelude hiding (words, lookup)
import           Data.Char (chr, ord, isAlpha, isDigit)
import qualified Data.Map as M
import qualified Data.List as L
import           Data.Maybe (mapMaybe, fromJust)
import           Data.Text (Text)
import qualified Data.Text as T

import Entropy
import Types


-- Apply a list of matchers to a password and get back the minimum
-- entropy and corresponding list of matches
zxcvbn :: [Matcher] -> Password -> (Double, [Match])
zxcvbn _ "" = (0.0, [])
zxcvbn ms p = minEntropyMatchSequence p $ ms >>= \m -> m p

type EntropyMatches = [(Double, Maybe Match)]

-- Finds the sequence of matches for the password which combine
-- to give the minimum entropy, including any brute-force matches
-- filling in the gaps
minEntropyMatchSequence :: Password -> [Match] -> (Double, [Match])
minEntropyMatchSequence p matches = (minEntropy, matchSequence)
  where
    -- The number of guesses for one character based on the password contents
    bfc = bruteForceCardinality p
    lgBfc = log2 $ fromIntegral bfc
    theEnd = T.length p - 1
    -- List of the optimal sequence of matches at each index of the password
    -- equivalent of up_to_k and backpointers combined in original code
    -- (the entropy up to index k and the final match ending at k)
    -- The is reversed, so the entropy of the first element is the total
    -- minimised entropy to the end of the string
    minEntropies = L.foldl' minimiseAtPosition [] [0..theEnd]
    minEntropy = fst $ head minEntropies
    matchSequence = case intersperseBruteForceMatches $ extractMatchSequence minEntropies [] of
                       []       -> [makeBruteForceMatch 0 theEnd]
                       ms@(m:_) -> if start m == 0
                                   then ms
                                   else makeBruteForceMatch 0 (start m - 1) : ms

    -- Consider each position and all matches which end at that position
    -- Find the match which has minimimum entropy at that point
    -- The minUpTo list needs to include all the entropy totals because we
    -- need to look back into previous totals based on the starting
    -- position of the current match in order to work out the current minimum.
    -- The complete list of minimised matches is accumulated in reverse,
    -- with first elt being the minimum match at the end of the password string
    -- and the total minimum entropy for the string
    minimiseAtPosition :: EntropyMatches -> Int -> EntropyMatches
    minimiseAtPosition minUpTo pos = let toBeat = if pos == 0 then lgBfc else fst (head minUpTo) + lgBfc
                                      in go (toBeat, Nothing) matches : minUpTo
      where
        go :: (Double, Maybe Match) -> [Match] -> (Double, Maybe Match)
        go bestSoFar []      = bestSoFar
        go bestSoFar (m@(Match (Token _ i j) e _):ms) =
            -- Entropy with the match is that of the match plus the sequence up to the
            -- start of the match
            let entropyWithMatch = e + if i == 0 then 0 else fst $ minUpTo !! (pos - i)
                newBest
                  | j /= pos = bestSoFar
                  | entropyWithMatch < fst bestSoFar = (entropyWithMatch, Just m)
                  | otherwise = bestSoFar
             in go newBest ms

    extractMatchSequence :: EntropyMatches -> [Match] -> [Match]
    extractMatchSequence [] ms                 = ms
    extractMatchSequence ((_, Nothing):ems) ms = extractMatchSequence ems ms
    extractMatchSequence ((_, Just m@(Match (Token _ i j) _ _)):ems) ms =
        extractMatchSequence (drop (j-i) ems) (m:ms)

    makeBruteForceMatch i j = Match (Token substring i j) (log2 . fromIntegral $ bfc ^ (j-i+1)) BruteForceMatch
      where
        substring = T.take (j-i+1) $ T.drop i p

    intersperseBruteForceMatches [] = []
    intersperseBruteForceMatches [m]
        | end m == theEnd = [m]
        | otherwise         = m : [makeBruteForceMatch (1 + end m) theEnd]
    intersperseBruteForceMatches (m1:m2:ms)
        | i == j    = m1 : intersperseBruteForceMatches (m2:ms)
        | otherwise = m1 : makeBruteForceMatch i (j-1) : intersperseBruteForceMatches (m2:ms)
      where
        i = 1 + end m1
        j = start m2


-- Creates a list of all possible tokens of a string
tokenize :: Text -> [Token]
tokenize p = L.sort $ map (\(s, l) -> Token s (ord $ T.head l) (ord $ T.last l)) $ zip ss ind
  where
    ss  = continuousSubSeqs p
    ind = continuousSubSeqs $ T.pack (map chr [0..100])
    continuousSubSeqs = filter (not . T.null) . concatMap T.tails . T.inits


-- Removes tokens from a list which are subtokens of another token in the list
removeSubTokens :: [Token] -> [Token]
removeSubTokens [] = []
removeSubTokens [t] = [t]
removeSubTokens (t:ts)
    | t `subtoken` head ts = removeSubTokens ts
    | head ts `subtoken` t = removeSubTokens $ t:tail ts
    | otherwise            = t : removeSubTokens ts
  where
    subtoken (Token _ i j) (Token _ k l) = i >= k && j <= l

start :: Match -> Int
start (Match (Token _ i _) _ _) = i

end :: Match -> Int
end (Match (Token _ _ j) _ _) = j

dictMatcher :: String -> [Text] -> Matcher
dictMatcher name words password = mapMaybe match (tokenize password)
  where
    dict = M.fromList (zip words [1..]) :: M.Map Text Int
    meta = DictMatch name
    match t@(Token w _ _) = (createMatch t w) <$> M.lookup (T.toLower w) dict
    createMatch t w rank = Match t (log2 (fromIntegral rank) + extraUpperCaseEntropy w) meta

-- Standard sequences
lowerCaseAlphabetic, upperCaseAlphabetic, digits :: String
lowerCaseAlphabetic = "abcdefghijklmnopqrstuvwxyz"
upperCaseAlphabetic = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits              = "01234567890"

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

sequenceMatches :: M.Map String Double -> Text -> [Match]
sequenceMatches ss password = maybe matches (\m -> m : matches) $ match stump k l
  where
    (stump, k, l, matches) = L.foldl' extendSequence ("", 0, 0, []) (T.unpack password)
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
      | j-i > 2   = fmap (\e -> Match (Token (T.pack $ reverse sq) i (j-1)) e SequenceMatch) $ M.lookup sq ss
      | otherwise = Nothing

-- Repeat Matching
--
repeatMatcher :: Matcher
repeatMatcher password = map createMatch $ repeatMatches $ tokenize password
  where
    createMatch t@(Token s i j) = let l = j - i + 1
                                      e = log2 $ fromIntegral $ l * bruteForceCardinality s
                                  in Match t e (RepeatMatch (T.head s))

    repeatMatches :: [Token] -> [Token]
    repeatMatches [] = []
    repeatMatches ts = removeSubTokens $ filter validRepeat ts

    validRepeat :: Token -> Bool
    validRepeat (Token w begin end) = end - begin + 1 > 2 && isRepeat w -- TODO: Use length of w since it should be the same. We effectively repeat this in isRepeat
      where
        isRepeat s
          | T.null        s = True
          | T.length s == 1 = True
          | otherwise       = let cs = T.tail s -- TODO: Use indices here instead of tail
                              in  T.head s == T.head cs && isRepeat cs

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
    (unl33ted, allSubs, multiSubs) = L.foldl' unl33t ([],[],[]) (T.unpack password)
    singleSubs = L.nub allSubs

    wordsToLookup = substitute [reverse unl33ted] multiSubs

    matches = do
             word    <- wordsToLookup
             matcher <- matchers
             Match (Token t i j) e matchType <- matcher (T.pack word)
             let l33tToken = T.take (j-i+1) $ T.drop i password
             let w = T.unpack t
             if T.toLower l33tToken == t
               then [] -- No l33t chars in token since it is the same as the dictionary match
               else let isUsed (c,c') = case T.findIndex (== c) l33tToken of
                                          Nothing -> False
                                          Just k  -> T.index t k == c'
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

l33tSubs :: M.Map Char String
l33tSubs = M.fromList $ zip k v
  where
    l33tFor l c = elem l $ fromJust $ M.lookup c l33tTable
    k = L.nub $ concat $ M.elems l33tTable
    v = map (\c -> filter (l33tFor c) $ M.keys l33tTable) k

    l33tTable = M.fromList
        [ ('a', "4@")
        , ('b', "8")
        , ('c', "({[<")
        , ('e', "3")
        , ('g', "69")
        , ('i', "1!|")
        , ('l', "1|7")
        , ('o', "0")
        , ('s', "$5")
        , ('t', "+7")
        , ('x', "%")
        , ('z', "2")
        ]
