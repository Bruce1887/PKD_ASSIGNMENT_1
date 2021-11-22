-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where


import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)
import Data.List
import TcRnMonad (initTcRnIf)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS
{- wordCount xs
Computes a tally of all the distinct words appearing in the document.

RETURNS: List of WordTallys, one for each unique word in xs, with the tally of the word.
EXAMPLES:
  wordCount [["a","b","c","a","b","a"],["d","e"]] == [("a",3),("b",2),("c",1),("d",1),("e",1)]
  wordCount [["a", "rose", "is", "a", "rose"],["but", "so", "is", "a", "rose"]] == 
    [("a",3),("rose",3),("is",2),("but",1),("so",1)]
-}
wordCount :: Document -> WordTally
wordCount xs = nub [(x, length $ filter (==x) concatList) | x<-concatList]
  where concatList = concat xs

-----
{- adjacentPairs xs
Computes all the pairs in each sentence in the document.

RETURNS: Pairs containing all pairs in each sentence in xs.
EXAMPLES: 
  adjacentPairs [["time", "for", "a", "break"], ["not", "for", "a", "while"]]
    == [("time","for"),("for","a"),("a","break"),("not","for"),("for","a"),("a","while")]
  adjacentPairs [["a"],["b"]] == []
-}
adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (x:xs) = adjacentPairsInSentence x ++ adjacentPairs xs

{- adjacentPairsInSentence sent
Computes all the pairs of words in a sentence.

RETURNS: Pairs of all words in sent.
EXAMPLES:
  adjacentPairsInSentence ["time", "for", "a", "break"] == [("time","for"),("for","a"),("a","break")]
  adjacentPairsInSentence ["a"] == []
-}
adjacentPairsInSentence :: Sentence -> Pairs

adjacentPairsInSentence [] = []
adjacentPairsInSentence [x] = []
adjacentPairsInSentence (x:y:xs) = (x,y) : adjacentPairsInSentence (y:xs)

-----

{- initialPairs doc
Computes the pair of the first two words of each sentence in a document.

RETURNS: Pairs of the first two words of all sentences in doc.
EXAMPLES:
  initialPairs [["time", "for", "a", "break"], ["not", "yet"]]
== [("time","for"),("not", "yet")]
  initialPairs [["a"]] == []
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (x:xs) = if null sentencePairs then [] else head sentencePairs : initialPairs xs
  where sentencePairs = adjacentPairsInSentence x

{- finalPairs doc
Computes the pair of the last two words of each sentence in a document.

RETURNS: Pairs of the last two words of all sentences in doc.
EXAMPLES:
  finalPairs [["time", "for", "a", "break"], ["not", "yet"], ["hello"]] == [("a", "break"),("not", "yet")]
  finalPairs [["a"]] == []
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (x:xs) = if null sentencePairs then [] else last sentencePairs : finalPairs xs
  where sentencePairs = adjacentPairsInSentence x

-----
{- pairsCount xs
Computes a tally of all pairs, such as those computed by adjacentPairs.
RETURNS: A list with pairs and their number of occurances as elements.
Examples:
  pairsCount [("big","bear"),("bear","big"),("big","dog")] == [(("bear","big"),2),(("big","dog"),1)]
-}
pairsCount :: Pairs -> PairsTally
pairsCount xs = nub [(x, length $ filter (==x) sortedList) | x<-sortedList]
  where sortedList = map sortInTuple xs

{- sortInTuple (a,b)
Orders a pair of strings in alphabetical order
Returns: An alphabetically ordered pair of strings.
EXAMPLES:
  sortInTuple ("b","a") == ("a","b")
  sortInTuple ("a","A") == ("A","a")
-}
sortInTuple :: (String, String) -> (String, String)
sortInTuple (a,b)
  | a < b = (a,b)
  | otherwise = (b,a)

-----

{- neighbours xs s
Computes all the words that appear next to a certain word, and the number of times they appear.

RETURNS: WordTally of all words that are in a pair with s in xs and the tally of the given pair.
EXAMPLES:
  neighbours [(("bear","big"),2),(("big","dog"),1)] "big" == [("bear",2),("dog",1)]
  neighbours [(("bear","big"),1)] "other" == []
  neighbours [] "other" == []
-}
neighbours :: PairsTally -> String -> WordTally
neighbours xs word = [if a==word
                    then (b,c)
                    else (a,c)
                  | ((a,b),c)<-xs, a==word || b==word]

-----

{-mostCommonNeighbour xs s
Computes the word that occurs next to a certain word the most.

RETURNS: Nothing if s is not in any of the pairs in xs, or Just the most common word to occur in a pair with s.
EXAMPLES: 
  mostCommonNeighbour [(("bear","big"),2),(("big","dog"),1)] "big" == Just "bear"
  mostCommonNeighbour [(("bear","big"),1)] "other" == Nothing
  mostCommonNeighbour [] "other" == Nothing
-}

mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour xs word
  | null $ neighbours xs word = Nothing
  | otherwise = Just $ fst (maximumBy (\(_,a) (_,b) -> compare a b) (neighbours xs word))



-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]])

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])

test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])


-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple"
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky"
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a")

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b")

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun")
                                                                  (mostCommonNeighbour input "the")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\""
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\""
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




