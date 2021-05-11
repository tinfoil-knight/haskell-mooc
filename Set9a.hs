-- Welcome to the first exercise set of part 2 of the Haskell Mooc!
-- Edit this file according to the instructions, and check your
-- answers with
--
--   stack runhaskell Set9aTest.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set9a.hs

module Set9a where

import Data.List
import Data.Ord

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Implement a function workload that takes in the number of
-- exercise a student has to finish, and another number that counts
-- the number of hours each exercise takes.
--
-- If the total number of hours needed for all exercises is over 100,
-- return "Holy moly!" if it is under 10, return "Piece of cake!".
-- Otherwise return "Ok."

workload :: Int -> Int -> String
workload nExercises hoursPerExercise = todo

------------------------------------------------------------------------------
-- Ex 2: Implement the function echo that builds a string like this:
--
--   echo "hello!" ==> "hello!, ello!, llo!, lo!, o!, !, "
--   echo "ECHO" ==> "ECHO, CHO, HO, O, "
--   echo "X" ==> "X, "
--   echo "" ==> ""
--
-- Hint: use recursion

echo :: String -> String
echo = todo

------------------------------------------------------------------------------
-- Ex 3: A country issues some banknotes. The banknotes have a serial
-- number that can be used to check if the banknote is valid. For a
-- banknote to be valid, either
--  * the third and fifth digits need to be the same
--  * or the fourth and sixth digits need to be the same
--
-- Given a list of bank note serial numbers (strings), count how many
-- are valid.

countValid :: [String] -> Int
countValid = todo

------------------------------------------------------------------------------
-- Ex 4: Find the first element that repeats two or more times _in a
-- row_ in the input list. Return a Nothing value if no element repeats.
--
-- Examples:
--   repeated [1,2,3] ==> Nothing
--   repeated [1,2,2,3,3] ==> Just 2
--   repeated [1,2,1,2,3,3] ==> Just 3

repeated :: Eq a => [a] -> Maybe a
repeated = todo

------------------------------------------------------------------------------
-- Ex 5: A laboratory has been collecting measurements. Some of the
-- measurements have failed, so the lab is using the type
--   Either String Int
-- to track the measurements. A Left value represents a failed measurement,
-- while a Right value represents a succesful one.
--
-- Compute the sum of all succesful measurements. If there are
-- succesful measurements, return the sum wrapped in a Right, but if
-- there are none, return Left "no data".
--
-- Examples:
--   sumSuccess [Right 1, Left "it was a snake!", Right 3]
--     ==> Right 4
--   sumSuccess [Left "lab blew up", Left "I was sick"]
--     ==> Left "no data"
--   sumSuccess []
--     ==> Left "no data"

sumSuccess :: [Either String Int] -> Either String Int
sumSuccess = todo

------------------------------------------------------------------------------
-- Ex 6: A combination lock can either be open or closed. The lock
-- also remembers a code. A closed lock can only be opened with the
-- right code. The code of an open lock can be changed.
--
-- Implement a datatype Lock and the functions isOpen, open, lock,
-- changeCode and the constant aLock as instructed below.
--
-- Examples:
--   isOpen aLock ==> False
--   isOpen (lock aLock) ==> False
--   isOpen (open "1234" aLock) ==> True
--   isOpen (lock (open "1234" aLock)) ==> False
--   isOpen (open "1235" aLock) ==> False
--   isOpen (lock (open "1235" aLock)) ==> False
--   isOpen (open "1234" (changeCode "0000" aLock)) ==> True
--   isOpen (open "0000" (changeCode "0000" aLock)) ==> False
--   isOpen (open "0000" (lock (changeCode "0000" (open "1234" aLock)))) ==> True
--   isOpen (open "1234" (lock (changeCode "0000" (open "1234" aLock)))) ==> False

data Lock = LockUndefined
  deriving Show

-- aLock should be a locked lock with the code "1234"
aLock :: Lock
aLock = todo

-- isOpen returns True if the lock is open
isOpen :: Lock -> Bool
isOpen = todo

-- open tries to open the lock with the given code. If the code is
-- wrong, nothing happens.
open :: String -> Lock -> Lock
open = todo

-- lock closes a lock. If the lock is already closed, nothing happens.
lock :: Lock -> Lock
lock = todo

-- changeCode changes the code of an open lock. If the lock is closed,
-- nothing happens.
changeCode :: String -> Lock -> Lock
changeCode = todo

------------------------------------------------------------------------------
-- Ex 7: Here's a type Text that just wraps a String. Implement an Eq
-- instance for Text that ignores all white space (space characters
-- and line returns).
--
-- Examples
--   Text "abc"  == Text "abc"      ==> True
--   Text "a bc" == Text "ab  c\n"  ==> True
--   Text "abc"  == Text "abcd"     ==> False
--   Text "a bc" == Text "ab  d\n"  ==> False

data Text = Text String
  deriving Show


------------------------------------------------------------------------------
-- Ex 8: We can represent functions or mappings as lists of pairs.
-- For example the list [("bob",13),("mary",8)] means that "bob" maps
-- to 13 and "mary" maps to 8.
--
-- Implement _composition_ for mappings like this. You compose two
-- mappings by looking up each result of the first mapping in the
-- second mapping.
--
-- You may assume there are no repeated first elements of tuples in
-- the argument lists, that is.
--
-- The ordering of the output doesn't matter.
--
-- Hint: remember the function `lookup` from Prelude?
--
-- Examples:
--   composing two mappings of size 1:
--     compose [("a",1)] [(1,True)]
--       ==> [("a",True)]
--   nonmatching mappings get ignored:
--     compose [("a",1),("b",2)] [(3,False),(4,True)]
--       ==> []
--   a more complex example: note how "omicron" and "c" are ignored
--     compose [("a","alpha"),("b","beta"),("c","gamma")] [("alpha",1),("beta",2),("omicron",15)]
--       ==> [("a",1),("b",2)]

compose :: (Eq a, Eq b) => [(a,b)] -> [(b,c)] -> [(a,c)]
compose = todo

------------------------------------------------------------------------------
-- Ex 9: Reorder a list using an [(Int,Int)] mapping.
--
-- Given a list of mappings [(from,to)], reorder the list so that the element
-- at the first index (from) goes to the second index (to). You may assume
-- that the list is ordered with respect to the first index (e.g.
-- [(0,0),(1,1),(2,2)], [(0,1),(1,0),(2,2)], [(0,1),(1,2),(2,0)], etc.). You
-- may also assume that for a list of length n, every number from 0 to n - 1
-- (inclusive) appears exactly once as the first index (from) and once as the
-- second index (to).
--
-- (Mappings of this kind are known as permutations in math, see
-- https://en.wikipedia.org/wiki/Permutation)
--
-- Implement the function permute that performs the reordering.
--
-- Examples:
--   permute [(0,0),(1,1)] [True, False] ==> [True, False]
--   permute [(0,1),(1,0)] [True, False] ==> [False, True]
--   permute [(0,0),(1,1),(2,2),(3,3),(4,4)] "curry" ==> "curry"
--   permute [(0,4),(1,3),(2,2),(3,1),(4,0)] "curry" ==> "yrruc"
--   permute [(0,2),(1,1),(2,0),(3,3),(4,4)] "curry" ==> "rucry"
--   permute [(0,2),(1,1),(2,0)] (permute [(0,2),(1,1),(2,0)] "foo")
--     ==> "foo"
--   permute [(0,1),(1,0),(2,2)] (permute [(0,0),(1,2),(2,1)] [9,3,5])
--     ==> [5,9,3]
--   permute [(0,0),(1,2),(2,1)] (permute [(0,1),(1,0),(2,2)] [9,3,5])
--     ==> [3,5,9]
--   permute ([(0,0),(1,2),(2,1)] `compose` [(0,1),(1,0),(2,2)]) [9,3,5]
--     ==> [5,9,3]
--   permute ([(0,1),(1,0),(2,2)] `compose` [(0,0),(1,2),(2,1)]) [9,3,5]
--     ==> [3,5,9]

type Permutation = [(Int,Int)]

permute :: Permutation -> [a] -> [a]
permute = todo
