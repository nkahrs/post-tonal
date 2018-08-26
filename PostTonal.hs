{-
I expected this to be less than it was. Anyways, I wrote this in about an hour and a half on 10/17/17 to do basic atonal theory calculations in Haskell.

for now, we'll let type ascriptions be as loose as possible, but the twelve pcs will just be integral numbers 0--11, we'll use "mod" to standardize, via inOct function
-}

module PostTonal where

import Data.List -- for intersect
import Data.Monoid -- for <>

-- Pitch-class notation borrowed from Euterpea source at https://github.com/Euterpea/Euterpea/blob/master/Euterpea/Music/Note/Music.hs
data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
                 |  Bf | Ass | B | Bs | Bss
     deriving (Show, Eq, Ord, Read, Enum, Bounded)

pcToInt pc  = case pc of
  Cff  -> -2;  Cf  -> -1;  C  -> 0;   Cs  -> 1;   Css  -> 2; 
  Dff  -> 0;   Df  -> 1;   D  -> 2;   Ds  -> 3;   Dss  -> 4; 
  Eff  -> 2;   Ef  -> 3;   E  -> 4;   Es  -> 5;   Ess  -> 6; 
  Fff  -> 3;   Ff  -> 4;   F  -> 5;   Fs  -> 6;   Fss  -> 7; 
  Gff  -> 5;   Gf  -> 6;   G  -> 7;   Gs  -> 8;   Gss  -> 9; 
  Aff  -> 7;   Af  -> 8;   A  -> 9;   As  -> 10;  Ass  -> 11;
  Bff  -> 9;   Bf  -> 10;  B  -> 11;  Bs  -> 12;  Bss  -> 13

-- edo: number of equal divisions of the octave
edo = 12

-- inOct x = mod x 12, for standardization
inOct = flip mod edo

-- interval x y returns the interval from x to y
interval x y = inOct (y - x)

-- ifunc: Lewinian IFUNC, as list of lists
-- takes two lists of pcs, returns intervals from the first to the second
-- ifunc xs ys = (map interval xs) <*> ys -- would be better, but I want the extra brackets
table f xs ys = map (\x -> map (f x) ys) xs

ifunc = table interval

showTabular xs = let
  showLine = foldr (\x y -> (show x) ++ "\t" ++ y) "\n" in
  foldr (++) "" (map showLine xs)

showIfunc xs ys = putStr $ showTabular $ ifunc xs ys

-- tics vector for identifying inversions
tics = table (\ x y -> inOct $ x + y)

-- tabular': convert a normal tabular function into one consuming PCs instead of integers
tabular' f = (\xs ys -> f (map pcToInt xs) (map pcToInt ys))

-- ifunc', tics': same but with PCs instead of integers
ifunc' = tabular' ifunc
tics' = tabular' tics

-- 10/24 change. Currently ifunc is tabular, but it really should be a count. So, how do we get a count from a table?

-- ideally less than n^2 time. Counting is inefficient with linked lists. Concatenate (linear), sort, then construct a new list with a recursive pile.

countNums curNum curCount maxNum xs = -- recursive function to count numbers in the table. curNum is current number being sought, curCount is current number of that, maxNum is max number to look for, xs is list to search in. Behavior undefined if curNum > maxNum. Requires xs be sorted (this substitutes an external n log n thing for an internal n^2)
  if curNum > maxNum then [] else
    if (xs == []) then curCount : countNums (curNum + 1) 0 maxNum [] else
      if ((head xs) == curNum) then
        countNums curNum (curCount + 1) maxNum (tail xs)
      else
        (curCount) : countNums (curNum + 1) 0 maxNum xs

countTab tab = let
  is = sort $ concat tab -- table concatenated and sorted
  in
    countNums 0 0 11 is

-- operators: T and I
data TI
  = T Integer
  | I Integer
  deriving Show

standardize (T n) = T $ inOct n
standardize (I n) = I $ inOct n

instance Eq TI where
  T m == T n = inOct m == inOct n
  I m == I n = inOct m == inOct n
  T _ == I _ = False
  I _ == T _ = False

-- inCommon: overlap of a list of lists, all of which only have 0,1,...,edo-1 as elements
inCommon :: Foldable t => t [Integer] -> [Integer]
inCommon = foldl intersect [0..(edo-1)]

-- pcsTIs: returns the T and I operators that take you from the first collection to the second
pcsTIs xs ys = mappend (map T $ inCommon $ ifunc' xs ys) (map I $ inCommon $ tics' xs ys)
-- this should be pcsTIs': in general, any ' should be the same function but accepting type PitchClass instead of Integer

-- combining T/I operators
instance Monoid TI where
  mempty = T 0
  -- in mappend, <> will be Klumpenhower's Lewinian left-functional backwards composition operator (f1 <> f2 $ x is f2 . f1 $ x)
  mappend (T m) (T n) = T $ inOct $ m + n
  mappend (I m) (T n) = I $ inOct $ m + n
  mappend (T m) (I n) = I $ inOct $ n - m
  mappend (I m) (I n) = T $ inOct $ n - m

-- stuff I should do but that doesn't take the immediate tedium away

-- turn TI operators into functions
tiF (T m) = inOct . (+ m)
tiF (I m) = inOct . (\x -> m - x)

-- prime form and normal order

-- future thing to do: containers for typechecking
-- we'll put these in containers just for type-checking
-- data Normal = Normal [Integer]
--   deriving (Eq, Show, Ord)

-- data Prime = Prime [Integer]
--   deriving (Eq, Show, Ord)

-- for prime form and normal order: need to compare packed-to-left-ness
comparePackedLeft (x0:(x1:xs)) (y0:(y1:ys)) =
  case compare (inOct $ x1-x0) (inOct $ y1-y0) of
    EQ -> comparePackedLeft (x1:xs) (y1:ys)
    LT -> LT
    GT -> GT

comparePackedLeft (x1:[]) (y1:[]) = EQ
comparePackedLeft [] [] = EQ
comparePackedLeft [] _ = LT
comparePackedLeft _ _ = GT

-- compareSpan: compare the span of o1 to that of o2
compareSpan o1 o2 = let span x = inOct $ last x - head x in compare (span o1) (span o2)

-- compareSpanPacked: both of these
compareSpanPacked xs ys = case compareSpan xs ys of
  LT -> LT
  GT -> GT
  EQ -> comparePackedLeft xs ys

-- should have tests here

-- normal order: here by inefficient brute force, calculating all rotations needlessly. I could improve, but not retaining functional style.
-- 10/18: added nub because I was getting a weird glitch with prime form of [(-7),0,0]
normalOrder unsorted =
  let sorted = nub $ sort $ map inOct unsorted
      rotations_helper [] _ acc = acc
      rotations_helper xs ys acc =
        rotations_helper (tail xs) (ys <> [head xs]) (xs <> ys : acc)
      rotations = rotations_helper sorted [] []
  in
    minimumBy compareSpanPacked rotations

-- prime form
primeForm xs =
  let normal = normalOrder xs
      zero xs = map (\x -> inOct $ x - (head xs)) xs
      zeroed = zero normal
      inverted = zero $ normalOrder $  map ((last zeroed) -) zeroed
      -- change 10/22: replaced sort with normalOrder to fix a bug
  in
    minimumBy comparePackedLeft [zeroed, inverted]


-- and, for PC input
primeForm' xs = primeForm $ map pcToInt xs

-- interval vector

-- actual tone rows


-- 12/2/17: TICS and IFUNC shouldn't be tables, they should be lists of "how many per interval".
-- so, for the situations where we just want to count them from one set to itself,

tabularSelfAgg tabfunc set = sort $ concat $ tabfunc set set

listToCount ns = map (length . (\x -> filter (== x) ns)) [0..(edo-1)]

tabSelfCount tabfunc set = listToCount $ tabularSelfAgg tabfunc set

-- other stuff useful for serial operations

intToPc i = case i of
  0 -> C; 1 -> Cs; 2 -> D; 3 -> Ef; 4 -> E; 5 -> F;
  6 -> Fs; 7 -> G; 8 -> Gs; 9 -> A; 10 -> Bf; 11 -> B;

-- row form is messy because of different nomenclatures.
-- examples of how we could find, say P9 (old notation where 0 is fixed first form, would be calculated differently if we use note names):
-- map intToPc $ map (tiF $ T 9) $ map pcToInt [Cs,D,G,Fs,Gs,C,Ef,F,E,Bf,B,A]
