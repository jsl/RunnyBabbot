module RunnyBabbot.Spoonerize ( spoonerize
                              , markedSentence
                              , spoonerizations
                              , WordInfo(..)
                              ) where

import System.Random (randomRIO)
import Data.List (sort, nubBy)
import Data.Char (isLower, toLower, toUpper)
import Text.Regex.Posix

type Sequence        = Int
type Word            = String
type IsSpoonerizable = Bool

data WordInfo = WordInfo Sequence Word IsSpoonerizable
                deriving (Show)

instance Ord WordInfo where
  (WordInfo seq1 _ _) `compare` (WordInfo seq2 _ _) = seq1 `compare` seq2

instance Eq WordInfo where
    (WordInfo seq1 word1 bool1) == (WordInfo seq2 word2 bool2) =
        seq1 == seq2 && word1 == word2 && bool1 == bool2

type AnnotatedSentence = [WordInfo]

alphabet   = ['A'..'Z'] ++ ['a'..'z']
vowels     = "AEIOUaeiou"

annotatedSentence :: String -> AnnotatedSentence
annotatedSentence sent =
    map (\(x, y, z) -> (WordInfo x y z)) wordTuples
    where sentence   = words sent
          wordTuples = zip3 [1..] sentence $ cycle [True]

isTooShort :: Word -> Bool
isTooShort word = length word <= 1

hasLeadingVowel :: Word -> Bool
hasLeadingVowel word = not (null word) && head word `elem` vowels

isMention :: Word -> Bool
isMention word = word =~ "@[[:alpha:]]+[[:punct:]]*" :: Bool

isSpoonerizableWord :: Word -> Bool
isSpoonerizableWord word = not (isTooShort word) &&
                           not (hasLeadingVowel word) &&
                           not (isAllConsonants word) &&
                           not (isMention word)

markSpoonerizableWords :: AnnotatedSentence -> AnnotatedSentence
markSpoonerizableWords =
    map (\(WordInfo x y z) -> (WordInfo x y (z && isSpoonerizableWord y)))

spoonerizableWords :: AnnotatedSentence -> AnnotatedSentence
spoonerizableWords = filter (\(WordInfo _ _ isSpoonerizable) -> isSpoonerizable)

wordBeginning :: String -> String
wordBeginning = takeWhile isConsonant

wordEnding :: String -> String
wordEnding = dropWhile isConsonant

isConsonant :: Char -> Bool
isConsonant l = l `notElem` vowels

isAllConsonants :: Word -> Bool
isAllConsonants = all isConsonant

caseFunction :: Char -> Char -> Char
caseFunction char = if isLower char then
                        toLower
                    else
                        toUpper

swapWordCase :: (Word, Word) -> (Word, Word)
swapWordCase (wordA, wordB) =
    (newLtrA : tail wordA,
     newLtrB : tail wordB)
    where firstLtrA = head wordA
          firstLtrB = head wordB
          newLtrA   = caseFunction firstLtrB firstLtrA
          newLtrB   = caseFunction firstLtrA firstLtrB

swapWordBeginnings :: (Word, Word) -> (Word, Word)
swapWordBeginnings (wordA, wordB) =
    (wordBeginning bCaseFlipped ++ wordEnding wordA,
     wordBeginning aCaseFlipped ++ wordEnding wordB)
    where (aCaseFlipped, bCaseFlipped) = swapWordCase (wordA, wordB)

spoonerizeWords :: (WordInfo, WordInfo) -> (WordInfo, WordInfo)
spoonerizeWords (WordInfo seqA wordA boolA, WordInfo seqB wordB boolB) =
    (WordInfo seqA newWordA boolA, WordInfo seqB newWordB boolB)
    where (newWordA, newWordB) = swapWordBeginnings(wordA, wordB)

wordSequenceNumbers :: AnnotatedSentence -> [Int]
wordSequenceNumbers = map (\(WordInfo sequenceNumber _ _) -> sequenceNumber)

substituteWords :: (AnnotatedSentence, WordInfo, WordInfo) -> String
substituteWords (oldsentence, toSpoonerizeA, toSpoonerizeB) =
    unwords $ map (\(WordInfo _ word _) -> word) orderedWords
    where
      sequencesToReplace = wordSequenceNumbers [spoonerizedA, spoonerizedB]
      minusSpoonerized = filter (\(WordInfo seq _ _) ->
                                 (seq `notElem` sequencesToReplace)) oldsentence

      (spoonerizedA, spoonerizedB) =
          spoonerizeWords(toSpoonerizeA, toSpoonerizeB)

      newSentence = minusSpoonerized ++ [spoonerizedA, spoonerizedB]
      orderedWords = sort newSentence

markedSentence :: String -> AnnotatedSentence
markedSentence sentence = markSpoonerizableWords $ annotatedSentence sentence

symEq :: Eq a => (a,a) -> (a,a) -> Bool
symEq (x, y) (u, v) = (x == u && y == v) || (x == v && y == u)

removeDuplTuples :: Eq a => [(a,a)] -> [(a,a)]
removeDuplTuples = nubBy symEq

allpairs :: [Int] -> [(Int, Int)]
allpairs xs = filter (\(x, y) -> not $ x == y) $
              removeDuplTuples [(i, j) | i <- xs, j <- xs]

spoonerizableWordSeqs :: AnnotatedSentence -> [(Int, Int)]
spoonerizableWordSeqs sent = allpairs $ map (\(WordInfo seq _ _) -> seq) $
                             filter (\(WordInfo seq _ isSpoonerizable) ->
                                         isSpoonerizable) sent

-- Returns all of the possible spoonerizations for the given String.
spoonerizations :: String -> [String]
spoonerizations sent = map
                       (\(i, j) ->
                            substituteWords
                            (asent,
                             (asent !! (i - 1)),
                             (asent !! (j - 1))))
                       tuples
                       where
                         asent = markedSentence sent
                         tuples = spoonerizableWordSeqs asent

-- Returns a random spoonerization for the given String.
spoonerize :: String -> IO String
spoonerize sent = do
  randomSpoonerizationNum <- randomRIO(0, length possibilities)
  return $ possibilities !! randomSpoonerizationNum
    where possibilities = spoonerizations sent
