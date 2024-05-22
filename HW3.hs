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
serialize Empty = []
serialize (Tree l x r) = [x] ++ serialize l ++ serialize r
deserialize :: [Int] -> Tree Int
deserialize [] = Empty
deserialize xs = let (tree, _) = buildTree xs in tree
  where
    buildTree :: [Int] -> (Tree Int, [Int])
    buildTree [] = (Empty, [])
    buildTree (y : ys) =
      let (leftTree, rest1) = buildTree ys
          (rightTree, rest2) = buildTree rest1
      in (Tree leftTree y rightTree, rest2)


-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

smallSample :: InfiniteList a -> [a]
smallSample = take 5 . itoList

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend [] a = a
iprepend (x : xs) a = x :> iprepend xs a 

itake :: Integer -> InfiniteList a -> [a]
itake 0 _ = []
itake n (a :> as) = a : itake (n-1) as

idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop 0 a = a
idrop n (_ :> as) = idrop (n-1) as

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (a :> as) = f a :> imap f as

ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter f (a :> as) = if f a then a :> ifilter f as else ifilter f as

ifind :: (a -> Bool) -> InfiniteList a -> a
ifind f (a :> as) = if f a then a else ifind f as

iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (xs :> xss) = iprepend xs (iconcat xss)

naturals :: InfiniteList Integer
naturals = iiterate (+1) 0

negatives :: InfiniteList Integer
negatives = iiterate (+(-1)) (-1)

interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (x :> xs) ys = x :> interleave ys xs

integers :: InfiniteList Integer
integers = interleave naturals negatives
-- integers = iconcat (negatives :> (naturals :> []))

rationals :: InfiniteList Rational
rationals = undefined

-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational
rationals' = undefined

-- -- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)
data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)

type Stack = [Int]

-- Helper function to safely read an integer
safeReadInt :: String -> Maybe Int
safeReadInt s = readMaybe s

-- Function to parse and execute a single command
executeCommand :: String -> Stack -> Either RunError Stack
executeCommand command stack =
  case words command of
    ["PUSH", n] -> case safeReadInt n of
      Just num -> Right (num : stack)
      Nothing  -> Left (ParseError command)
    ["POP"] -> case stack of
      []     -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Nothing})
      (_:xs) -> Right xs
    ["SWAP"] -> case stack of
      x1:x2:xs -> Right (x2:x1:xs)
      _        -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Just (length stack)})
    ["DUP"] -> case stack of
      x:xs -> Right (x:x:xs)
      _    -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Nothing})
    ["ADD"] -> case stack of
      x1:x2:xs -> Right ((x1 + x2) : xs)
      _        -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Just (length stack)})
    ["SUB"] -> case stack of
      x1:x2:xs -> Right ((x2 - x1) : xs)
      _        -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Just (length stack)})
    ["MUL"] -> case stack of
      x1:x2:xs -> Right ((x1 * x2) : xs)
      _        -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Just (length stack)})
    ["DIV"] -> case stack of
      x1:x2:xs -> if x2 == 0
                  then Left (InstructionError DivisionByZero)
                  else Right ((x2 `div` x1) : xs)
      _        -> Left (InstructionError $ StackUnderflow {instruction = command, stackValue = Just (length stack)})
    _ -> Left (ParseError command)

-- Main function to parse and run a list of commands
parseAndRun :: String -> Either RunError [Int]
parseAndRun = undefined 
