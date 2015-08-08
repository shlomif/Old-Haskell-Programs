module MySplit where

import Char
import HUnit

mysplit :: (a -> Bool) -> [a] -> [[a]]
mysplit pred [] = [[]]
mysplit pred (a:as) = [[a]]

test1 = TestCase (assertEqual "mysplit(string) #1"
    ["Quick", "brown", "fox"] (mysplit isSpace "Quick   brown fox"))

test2 = TestCase (assertEqual "Leading Space"
    ["", "Quick", "brown", "fox"] (mysplit isSpace "   Quick   brown fox"))

test3 = TestCase (assertEqual "Trailing space"
    ["Quick", "brown", "fox", ""] (mysplit isSpace "Quick   brown fox    "))

test4 = TestCase (assertEqual "Trailing space and leading space"
    ["", "Quick", "brown", "fox", ""] (mysplit isSpace " Quick   brown fox "))

test5 = TestCase (assertEqual "Tabs"
    ["Quick", "brown", "foxen"] (mysplit isSpace "Quick \t\t brown\tfoxen"))

test6 = TestCase (assertEqual "Empty"
    [] (mysplit isSpace ""))

test7 = TestCase (assertEqual "All Space"
    ["", ""] (mysplit isSpace "        "))

tests = TestList [
    TestLabel "test1" test1
    ]

