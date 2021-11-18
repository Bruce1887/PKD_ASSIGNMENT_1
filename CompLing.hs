-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

{- wordCount arg1 arg2
Computes a tally of all the distinct words appearing in the document.
PRE:
RETURNS:
EXAMPLES:
(VARIANT:)
-}

--testlista [["a","b","c","a","b","a"]]

wordCount :: Document -> WordTally
wordCount [[]] = []
wordCount doc = (head(head doc), wordCountAux [doc] head(head doc)):wordCount (tail doc)

--testlista ["a","b","c","a","b","a"]

{-wordCountAux wordlist word
Counts number of occurances of a given word in a list
RETURN: an int corresponding to the number of times a string apperears in a list of strings.
EXAMPLES:
        wordCountAux ["a","b","c","a","b","a"] "a" == 3
        wordCountAux ["a","b","c","a","b","a"] "x" == 0
        wordCountAux ["a","b","c","a","b","a"] "c" == 1
        wordCountAux ["a","b","c","a","b","a"] "b" == 2
-}
wordCountAux :: [String] -> String -> Int
wordCountAux [] word = 0
wordCountAux wordList word
  | word == (head wordList) = 1 + wordCountAux (tail wordList) word
  | otherwise               = wordCountAux (tail wordList) word
--------------------------------------------------------------------------------
adjacentPairs :: Document -> Pairs
adjacentPairs = undefined  -- remove "undefined" and write your function here

initialPairs :: Document -> Pairs
initialPairs = undefined  -- remove "undefined" and write your function here

finalPairs :: Document -> Pairs
finalPairs = undefined  -- remove "undefined" and write your function here

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
