{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Text.Read (readMaybe)

-- Section 1: Tree Serialization
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
serialize :: Tree Int -> [Int]
deserialize :: [Int] -> Tree Int

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList
itoList :: InfiniteList a -> [a]
iiterate :: (a -> a) -> a -> InfiniteList a
irepeat :: a -> InfiniteList a
iprepend :: [a] -> InfiniteList a -> InfiniteList a
itake :: Integer -> InfiniteList a -> [a]
idrop :: Integer -> InfiniteList a -> InfiniteList a
naturals :: InfiniteList Integer
imap :: (a -> b) -> InfiniteList a -> InfiniteList b
ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifind :: (a -> Bool) -> InfiniteList a -> a
iconcat :: InfiniteList [a] -> InfiniteList a
integers :: InfiniteList Integer
rationals :: InfiniteList Rational
-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational

-- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)
data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)
parseAndRun :: String -> Either RunError [Int]
