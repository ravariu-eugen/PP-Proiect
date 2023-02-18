
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Text.Read

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

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



{-
    TASK SET 2
-}

-- Task 1

test_if_number :: String -> Bool -- verifica daca un string este un numar
test_if_number s = (readMaybe s :: Maybe Double) /= Nothing
-- functia readMaybe intoarce Nothing daca string-ul nu poate fi citit ca un double

compVal :: Value -> Value -> Ordering -- compara 2 valori
compVal v1 v2 -- daca ambele sunt numere, le compara valoarea; 
        | (test_if_number v1) == True && (test_if_number v2) == True = compare (stringToDouble v1) (stringToDouble v2)
        | otherwise = compare v1 v2 -- altfel, le compara lexicografic

-- compara 2 linii in functie de valoarea de la coloana col folosind functia fun
-- daca sunt egale, le compara dupa prima coloana
compRowByColumn :: (Value -> Value -> Ordering) -> Int -> Row -> Row -> Ordering
compRowByColumn fun col r1 r2 = case (fun (r1!!col) (r2!!col)) of EQ -> compare (head r1) (head r2)
                                                                  LT -> LT
                                                                  GT -> GT


-- sorteaza un tabel in functie de coloana
tsort :: ColumnName -> Table -> Table
tsort column table = case (elemIndex column (head table)) of Nothing -> table
                                                             (Just col) -> (head table):(sortBy (compRowByColumn compVal col) (tail table))

-- Task 2
-- reuniune verticala de tabele
vunion :: Table -> Table -> Table
vunion t1 t2 
    | (head t1) == (head t2) = t1++(tail t2)
    | otherwise = t1

-- Task 3

-- adauga linii vide pana cand tabelul are cel putin len linii
padTo :: Int -> Table -> Table 
padTo len t = t++(take n (repeat blankRow))
                where width = length (head t)
                      n = max 0 (len - (length t))
                      blankRow = take width (repeat "")
-- reuniune orizontala de tabele
hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++) (padTo finalLen t1) (padTo finalLen t2)
                where finalLen = max (length t1) (length t2)

-- Task 4
-- gaseste prima linie cu cheia data pe linia aleasa 
-- intoarce lista vida daca nu exista o astfel de linie
getRowByKey :: ColumnName -> Value -> Table -> Row 
getRowByKey column_name value t = (head.tail.(++[[]]).(filterTable (value==) column_name)) t
-- functia filterTable va intoarce un tabel cu numele coloanelor pe prima linie, dupa care urmeaza
-- liniile care indeplinesc conditia. Daca nicio linie nu indeplineste conditia, atunci tabelul rezultat 
-- va avea o singura linie. De aceea, concatenam la sfarsitul tabelului o lista vida.
-- Daca exista o linie cu cheia data pe coloana aleasa, atunci ea va fi pe a doua linie a tabelului,
-- de unde o extragem folosind (head.tail)
-- Altfel, filterTable intoarce un tabel doar cu numele coloanelor pe prima linie.
-- De aceea, prin concatenare, pe a doua linie ajunge lista vida

-- intoarce valoarea de pe o anumita coloana dintr-o linie
getValueByKey ::[ColumnName] -> ColumnName -> Row -> Value
getValueByKey header column_name r = case (elemIndex column_name header) of Nothing -> undefined
                                                                            (Just col) -> r!!col

-- inlocuieste o valoare din linie cu o alta valoare
-- daca valoarea noua este "", valoarea nu este inlocuita
replaceValue :: Int -> Value -> Row -> Row 
replaceValue _ "" r = r
replaceValue col val r
    | col >= (length r) = r
    | otherwise = (take col r)++[val]++(drop (col+1) r)

-- uneste 2 linii in functie de numele coloanelor
mergeRows :: [ColumnName] -> [ColumnName] -> Row -> Row -> Row 
mergeRows head1 head2 r1 r2 = foldl op r1 (zip head2 r2)
                                where op row (colName, value) = case (elemIndex colName head1) of Nothing -> row++[value]
                                                                                                  (Just col) -> replaceValue col value row

-- reuneste 2 tabele in functie de o coloana
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column (head1:vals1) (head2:vals2) = (mergeRows head1 head2 head1 head2):(foldr op [] vals1)
                                                where op row acc 
                                                        | eq_row == [] = acc
                                                        | otherwise = (mergeRows head1 head2 row eq_row):acc
                                                        where row_val = getValueByKey head1 key_column row
                                                              eq_row = getRowByKey key_column row_val (head2:vals2)


-- Task 5
-- aplica o functie pe fiecare pereche din produsul cartezian al liniilor a doua tabele
cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names:[(new_row_function x y) | x <- (tail t1), y <- (tail t2)]

-- Task 6
-- extrage o coloanele alese
projection :: [ColumnName] -> Table -> Table
projection columns_to_extract = tr.(filter op).tr
                                where op col = elem (head col) columns_to_extract

-- Task 7
-- verifica daca o valoare de pe o anunita coloana a unei linii verifica conditia data
checkRow :: (Value -> Bool) -> Int -> Row -> Bool
checkRow fun col r1 = fun (r1!!col)

-- filtreaza un tabel, eliminand liniile ce nu verifica o conditie pentru valoarea de pe coloana data
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = case (elemIndex key_column (head t)) of Nothing -> t
                                                                             (Just col) -> (head t):(filter (checkRow condition col) (tail t))


