
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
import Common
import Dataset

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

stringToFloat :: String -> Float
stringToFloat s = read s :: Float

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


-- Task 8 TO_DO


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t



class Eval a where
    eval :: a -> QResult

-- extrage coloana cu numele dat din tabel si o transforma in linie
getColumn :: ColumnName -> Table -> [String] 
getColumn colname table = case (elemIndex colname (head table)) of Nothing -> []
                                                                   (Just col) ->  ((tr.tail) table)!!col

-- obtine tabelul rezultat din evaluarea unui query
getTable :: QResult -> Table
getTable (Table t) = t

-- obtine tabelul dintr-un query
queryToTable :: Query -> Table
queryToTable = getTable.eval


instance Eval Query where

    eval (FromTable table) = Table table

    -- pentru urmatorii constructori, aplicam la evaluare functia asociata descrisa mai sus
    eval (AsList colname query) = List (getColumn colname (queryToTable query))
    eval (Sort colname query) = Table (tsort colname (queryToTable query))
    eval (ValueMap op query) = Table (vmap op (queryToTable query))
    eval (RowMap op colnames query) = Table (rmap op colnames (queryToTable query))
    eval (VUnion query1 query2) = Table (vunion (queryToTable query1) (queryToTable query2))
    eval (HUnion query1 query2) = Table (hunion (queryToTable query1) (queryToTable query2))
    eval (TableJoin colname query1 query2) = Table (tjoin colname (queryToTable query1) (queryToTable query2))
    eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames (queryToTable query1) (queryToTable query2))
    eval (Projection colnames query) = Table (projection colnames (queryToTable query))



    -- filtram folosind conditia obtinuta din evaluarea lui fcond
    eval (Filter fcond query) = Table (colnames:(filter (feval colnames fcond) (tail t)))
                                    where t = (queryToTable query)
                                          colnames = head t
    eval (Graph edgeOp query) = Table (make_graph edgeOp (queryToTable query))
-- 3.2 & 3.3


-- genereaza lista muchiilor de la o linie la un tabel, acumulandule in acc
-- numele nodurilor sunt puse in ordine, astfel incat numele nodului "From" 
-- este mai mic lexicografic decat numele nodului "To"
generate_edges :: EdgeOp -> Table -> Row -> Table -> Table
generate_edges edgeOp acc r [] = acc
generate_edges edgeOp acc r (ht:ts) = case (edgeOp r ht) of Nothing -> rest
                                                            (Just val) -> if ((head r) >= (head ht)) 
                                                                          then [(head ht),(head r),val]:rest
                                                                          else [(head r),(head ht),val]:rest
                                     where rest = generate_edges edgeOp acc r ts
                                           
-- genereaza lista muchiilor unui tabel, acumulandule in acc
generate_graph :: EdgeOp -> Table -> Table -> Table
generate_graph edgeOp acc [] = acc
generate_graph edgeOp acc (h1:t1) = generate_edges edgeOp (generate_graph edgeOp acc t1) h1 t1


-- transforma tabelul in graf, valoarea muchiilor fiind definita de edgeOp
make_graph :: EdgeOp -> Table -> Table
make_graph edgeOp t = nub (["From","To","Value"]:(generate_graph edgeOp [] (tail t)))
    

-- 3.2 & 3.3                                                     

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a | -- filtru pentru elemente egale
    Lt String a | -- filtru pentru elemente mai mari
    Gt String a | -- filtru pentru elemente mai mici
    In String [a] | -- filtru pentru verificare apartenenta la lista
    FNot (FilterCondition a) | -- negarea unui filtru
    FieldEq String String -- filtru pentru verificare egalitate intre coloane

-- verifica daca un element apartine unei liste
isMember::(Eq a) => a -> [a] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp
instance FEval Float where
    feval colnames (Eq col val) = \row -> (stringToFloat (getValueByKey colnames col row)) == val
    feval colnames (Lt col val) = \row -> (stringToFloat (getValueByKey colnames col row)) < val
    feval colnames (Gt col val) = \row -> (stringToFloat (getValueByKey colnames col row)) > val
    feval colnames (In col vals) = \row -> isMember (stringToFloat (getValueByKey colnames col row)) vals
    feval colnames (FNot fcond) = (not).(feval colnames fcond)
    feval colnames (FieldEq col1 col2) = \row -> (stringToFloat (getValueByKey colnames col1 row)) == (stringToFloat (getValueByKey colnames col2 row))                         
instance FEval String where
    feval colnames (Eq col val) = \row -> (getValueByKey colnames col row) == val
    feval colnames (Lt col val) = \row -> (getValueByKey colnames col row) < val
    feval colnames (Gt col val) = \row -> (getValueByKey colnames col row) > val
    feval colnames (In col vals) = \row -> isMember (getValueByKey colnames col row) vals
    feval colnames (FNot fcond) = (not).(feval colnames fcond)
    feval colnames (FieldEq col1 col2) = \row -> (getValueByKey colnames col1 row) == (getValueByKey colnames col2 row)
-- 3.4



-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value


-- 3.5
similarities_query :: Query
similarities_query = ((Sort "Value").(Filter filt1).(Filter filt2).(Graph edgeOp).FromTable) Dataset.eight_hours
                        where edgeOp::Row -> Row -> Maybe Value
                              -- calculeaza numarul de campuri egale intre 2 linii
                              -- transformam fiecare pereche de valori in 1 daca sunt egale si 0 daca nu
                              -- apoi calculam suma
                              edgeOp r1 r2 = Just (show (foldr (+) 0 (zipWith op (tail r1) (tail r2))))
                              op v1 v2 = boolToInt (v1 == v2)
                              -- filtru pentru a lua elemente mai mari sau egale ca 5 
                              filt1 = FNot (Lt "Value" 5 :: FilterCondition Float)  
                              -- filtru pentru a elimina muchii cu "From" invalid
                              filt2 = FNot (Eq "From" "" :: FilterCondition String) 

-- 3.6 (Typos)
-- distanta dintre 2 string-uri
-- definim distanta dintre 2 string-uri ca numarul de modificari
--(eliminari, adaugari, schimbari de litere) necesare pentru a schimba 
--string-ul s1 in string-ul s2
stringDistance :: String -> String -> Int
stringDistance s1 s2 = (dp!l1)!l2
                    where l1 = length s1
                          l2 = length s2
                          val :: Int -> Int -> Int
                          val x y = (dp!x)!y
                          gen :: Int -> Int -> Int
                          gen x y 
                            | x == 0 = y
                            | y == 0 = x
                            | s1!!(x-1) == s2!!(y-1) = val (x-1) (y-1)
                            | otherwise = 1 + minimum [(val x (y-1)), (val (x-1) y), (val (x-1) (y-1))]
                          dp = listArray (0,l1) [listArray (0,l2) [gen x y   |y <- [0..l2]]    | x <- [0..l1]]

-- pentru fiecare varianta posibila a cuvantului gresit, calculam distanta de
-- la s la cuvantul posibil. Intoarcem cuvantul cel mai apropiat
autoCorrect ::[String]-> String -> String
autoCorrect pos s = aux (tail pos) (stringDistance s (head pos)) (head pos)
                    where aux :: [String] -> Int -> String -> String
                          aux [] _ best = best
                          aux (x:xs) mn best = if rez < mn then (aux xs rez x)
                                                    else (aux xs mn best) 
                                                    where rez = stringDistance x s  
-- corectam cuvantul de pe coloana nrcol 
correct_row :: [String] -> Int -> Row -> Row
correct_row posibles nrcol r = replaceValue nrcol (autoCorrect posibles (r!!nrcol)) r


-- corectam toate cuvintele de pe coloana col din tabelul t1 folosind tabelul t2
correct_table :: String -> Table -> Table -> Table
correct_table col t1 t2 = case (elemIndex col (head t1)) of Nothing -> t1
                                                            (Just nrcol) -> rmap (correct_row posCol nrcol) (head t1) t1
                                                            where posCol = getColumn col t2
                                                                  
