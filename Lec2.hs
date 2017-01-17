{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fwarn-tabs -fno-warn-type-defaults #-}

module Lec2 where
import Data.Char
import Test.HUnit

import Prelude hiding (($))

l1 :: [Char]
l1 = undefined

l2 :: [Int]
l2 = undefined

l3 :: [(Int,Bool)]
l3 = [ (1,True), (2, False) ]

l4 :: [[Int]]
l4 = undefined

-- l5 :: [Int]
-- l5 = [ 1 , True ]  -- doesn't type check

l6 :: [a]
l6 = []

l7 :: String
l7 = ['h','e','l','l','o',' ','5','5','2','!']

cons :: a -> [a] -> [a]
cons = (:)

c1 :: [Char]
c1 = 'a' : ['b', 'c']

c2 :: [Int]
c2 = 1 : []

-- what is the type of c3?
c3 = [] : [] 

testClone1, testClone2, testClone3 :: Test
testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]

clone :: a -> Int -> [a]

clone x n = undefined

cl1, cl2, cl3 :: IO Counts
cl1 = runTestTT testClone1
cl2 = runTestTT testClone2
cl3 = runTestTT testClone3

cl4 :: IO Counts
cl4 = runTestTT (TestList [ testClone1, testClone2, testClone3 ])

clone' :: a -> Int -> [a]
clone' = undefined

cl' :: IO Counts
cl' = runTestTT (TestList [ 
          clone' 'a' 4 ~?= ['a','a','a','a'],
          clone' 'a' 0 ~?= [],
          clone' 1.1 3 ~?= [1.1, 1.1, 1.1]
         ])

testRange :: Test
testRange = TestList [ range 3  6  ~?= [3,4,5,6],
                       range 42 42 ~?= [42],
                       range 10 5  ~?= [] ]

range :: Int -> Int -> [Int]

range i j = undefined

runRTests :: IO Counts
runRTests = runTestTT testRange

range' :: Int -> Int -> [Int]
range' i j = undefined

isHi :: String -> Bool
isHi ['H','i'] = True
isHi _ = False

isGreeting :: String -> Bool
isGreeting "Hi" = True
isGreeting "Hello" = True
isGreeting "Bonjour" = True
isGreeting "Guten Tag" = True
isGreeting _ = False

isSingleton :: [a] -> Bool
isSingleton = undefined

isLong :: [a] -> Bool
isLong = undefined 

listAddTests :: Test
listAddTests = TestList [ listAdd undefined ~?= undefined ]

listAdd :: [Int] -> Int

listAdd = undefined

runLATests :: IO Counts
runLATests = runTestTT listAddTests

listAdd' :: [Int] -> Int
listAdd' = undefined

listIncrTests :: Test
listIncrTests = 
 TestList [ listIncr [1,2,3] ~?= [2,3,4],
            listIncr [42]    ~?= [43],
            listIncr []      ~?= ([] :: [Int]) ]

-- listIncr :: 

listIncr = undefined

runLITests :: IO Counts 
runLITests = runTestTT listIncrTests

main :: IO ()
main = putStr "Hello World! \n"

act2 :: (IO (), IO ())
act2 = (putStr "Hello", putStr "Hello")

many :: IO ()
many = do putStr "Hello"
          putStr " World!"
          putStr "\n"

query :: IO ()
query = do putStr "What is your name? "
           n <- getLine
           putStrLn ("Welcome to CIS 552 " ++ n)

numTest :: IO Counts
numTest = runTestTT (3 ~?= 4) 

dotest :: IO ()
dotest = do c <- runTestTT (3 ~?= 3)
            putStrLn (show c)

plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1

funp :: (Int -> Int, Int -> Int)
funp = undefined

funs :: [Int -> Int]
funs = undefined

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

dtTests :: Test
dtTests = TestList [ doTwice plus1  4 ~?= 6,
                     doTwice minus1 5 ~?= 3 ]

plusn :: Int -> (Int -> Int)
plusn n = f
   where f x = x + n

plus10  :: Int -> Int
plus10  = undefined

minus20 :: Int -> Int
minus20 = undefined

plus :: Int -> Int -> Int
plus m n = m + n

plusfive :: Int -> Int
plusfive = undefined

pfivetest :: Test
pfivetest = plusfive 1000 ~?= 1005

doTwicePlus20 :: Int -> Int
doTwicePlus20 = doTwice (plus 20)

anonTests :: Test
anonTests = TestList [ (\x -> x + 1) 100 ~?= 101,
                       doTwice (\x -> x + 1) 100 ~?= 102 ]

plus1' :: Int -> Int
plus1' = \x -> x + 1

($) :: (a -> b) -> a -> b
f $ x = f x

anotherFive :: Int
anotherFive = 2 `plus` 3

threeThirties :: [Int]
threeThirties = 30 `clone` 3

anotherFour :: Int
anotherFour = doTwice (+2) 0

greaterThan10 :: Int -> Bool
greaterThan10 = (10 <)

ex1 :: (a -> a) -> a -> a
ex1 = doTwice doTwice

