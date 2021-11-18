-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where


import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)
import Data.List

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

-}
wordCount :: Document -> WordTally
wordCount xs = nub [(x, length $ filter (==x) cxs) | x<-cxs]
  where cxs = concat xs

-----
{- adjacentPairs xs
Computes all the pairs in the document sentence-wise.

RETURNS: Pairs containing all pairs in each sentence in xs.
EXAMPLES: 
  adjacentPairs [["time", "for", "a", "break"], ["not", "for", "a", "while"]]
    == [("time","for"),("for","a"),("a","break"),("not","for"),("for","a"),("a","while")]
  adjacentPairs [["a"],["b"]] == []
-}
adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (x:xs) = adjacentPairsInSentence x ++ adjacentPairs xs

{- adjacentPairsInSentence xs
Computes all the pairs in a sentence.

RETURNS: Pairs of all words in xs.
EXAMPLES:
  adjacentPairsInSentence ["time", "for", "a", "break"] == [("time","for"),("for","a"),("a","break")]
  adjacentPairsInSentence ["a"] == []
-}
adjacentPairsInSentence :: Sentence -> Pairs
adjacentPairsInSentence [] = []
adjacentPairsInSentence [x] = []
adjacentPairsInSentence (x:y:xs) = (x,y) : adjacentPairsInSentence (y:xs)

-----

{- initialPairs xs
Computes the first pairs of each sentence in a document.

RETURNS: Initial pairs of all sentences in xs.
EXAMPLES:
  initialPairs [["time", "for", "a", "break"], ["not", "yet"]]
== [("time","for"),("not", "yet")]
  initialPairs [["a"]] == []
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs [x] = []
initialPairs (x:xs) = head (adjacentPairsInSentence x) : initialPairs xs

{- finalPairs xs
Computes the first pairs of each sentence in a document.

RETURNS: Initial pairs of all sentences in xs.
EXAMPLES:
  finalPairs [["time", "for", "a", "break"], ["not", "yet"]]
== [("a", "break"),("not", "yet")]
  finalPairs [["a"]] == []
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs [x] = []
finalPairs (x:xs) = last (adjacentPairsInSentence x) : finalPairs xs

-----

pairsCount :: Pairs -> PairsTally
pairsCount = undefined  -- remove "undefined" and write your function here


neighbours :: PairsTally -> String -> WordTally
neighbours = undefined  -- remove "undefined" and write your function here

mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



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




