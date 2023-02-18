-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Text.Printf

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

stringToInt :: String -> Int
stringToInt s = read s :: Int

stringToDouble :: String -> Double
stringToDouble s = read s :: Double

average :: [Double] -> Double
average l =  (foldr (+) 0.0 l)/ (fromIntegral(length l))

row_average :: Row -> Value
row_average = rounded.average.(map stringToDouble)

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"]:(map op (tail m))
                          where op (name:vals) = name:[row_average vals]

rounded v = printf "%.2f" v :: String
-- Task 2

-- Number of people who have achieved their goal:

boolToInt :: Bool -> Int
boolToInt True = 1;
boolToInt False = 0;


numberOfSteps :: Row -> Int
numberOfSteps = sum.(map stringToInt).tail

get_passed_people_num :: Table -> Int
get_passed_people_num = sum.(map (boolToInt.(>1000).numberOfSteps)).tail 


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral(get_passed_people_num m)) / (fromIntegral(length (tail m)))


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg = realToFrac.average.(map (fromIntegral.numberOfSteps)).tail


-- Task 3

tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr m = (map head m) : tr (map tail m)

processColumn :: [String] -> [String]
processColumn (hour:steps) = ('H':hour):((rounded.average.(map (fromIntegral.stringToInt))) steps):[]

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h = tr.(map processColumn).tail.tr


-- Task 4

calc_ranges :: [Int] -> [Int]
calc_ranges = foldr op (0:0:0:[])
              where op x (a1:a2:a3:[])
                      | (0 <= x) && (x < 50) = (a1 + 1):a2:a3:[] 
                      | (50 <= x) && (x < 100) = a1:(a2 + 1):a3:[]
                      | (100 <= x) && (x < 500) = a1:a2:(a3 + 1):[] 
                      | otherwise = a1:a2:a3:[]
get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"]:(((map op).(drop 3).tr) m)
                      where op (name:vals) = name:(((map show).calc_ranges.(map stringToInt)) vals)

-- Task 5

--compByColumn :: Row -> Ordering
--compByColumn (name1:vals1) (name2:vals2) = 

compTotalSteps :: Row -> Row -> Ordering
compTotalSteps (name1:steps1:_) (name2:steps2:_)
        | (compare steps1 steps2) == EQ = compare name1 name2
        | otherwise = compare (stringToInt steps1) (stringToInt steps2)


get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"]:(sortBy compTotalSteps (((map (take 2)).tail) m))


-- Task 6
get_steps_diff :: Row -> Row
get_steps_diff (name:vals) = name:(row_average (take 4 vals)):(row_average (drop 4 vals)):[(rounded.abs) ( ((stringToDouble.row_average) (take 4 vals)) - ((stringToDouble.row_average) (drop 4 vals))  )]

get_steps_diff_table :: Table -> Table
get_steps_diff_table = rmap get_steps_diff ["Name","Average first 4h","Average last 4h","Difference"]


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s:(map f (tail m))


get_sleep_total :: Row -> Row
get_sleep_total (email:vals) = email:[(rounded.sum.(map stringToDouble)) vals]
