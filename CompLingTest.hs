type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

--testlista ["a","b","c","a","b","a"]
--[testlista] [["a","b","c","a","b","a"]]

{-wordCountAux wordlist word
Counts number of occurances of a given string in a list
Return: an int corresponding to the number of times a string apperears in a list of strings.
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
  | otherwise = wordCountAux (tail wordList) word

{-uniteLists x:xs
Combines a list of a list of strings to a list of strings
RETURNS: A list of strings
EXAMPLES:
        uniteLists [["a"],["ba","c"],["a","b"],["a"]] == ["a","ba","c","a","b","a"]
-}
uniteLists :: [[String]] -> [String]
uniteLists [] = []
uniteLists (x:xs) = x ++ uniteLists xs


wordCount :: Document -> WordTally
wordCount [[]] = []
wordCount doc = (head (uniteLists doc),wordCountAux (uniteLists doc):wordCount (tail doc)

--(head(head doc), wordCountAux [doc] (head(head doc)):wordCount (tail doc)
