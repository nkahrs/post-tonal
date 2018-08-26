-- Hauptmann scripts

import PostTonal
import Data.List
import Data.Monoid

-- first, a typedef for Hauptmann scripts themselves

data HauptmannScript = Alpha [Integer]
  deriving (Eq, Ord)

instance Show HauptmannScript where
  show (Alpha ns) = ('a':) $ concat $ map show ns

-- interval class: replaces an interval with its interval class
intervalClass i = let
  i' = inOct i
  in
    min i' (12 - i')

-- prime forms are just lists of pcs, so that's enough for now
-- the following algorithm is disgustingly inefficient
-- given prime form, return possible Hauptmann scripts (ordered only as natural, ie by which part of prime form spits it out)
primeToHpts xs = let
  recentered = map (\x -> map ((0 -) . (x -)) xs) xs
  ivsToHpt is = Alpha $ sort $ filter (/= 0) $ map intervalClass is
  in
    nub $ map ivsToHpt recentered

-- hptToPrimes: return all possible prime forms for a given Hauptmann Script

-- helper function: given two lists of the same length n (undefined behavior otherwise), want the 2^n lists whose ith element is the ith of one of the original two.
choicesFromLists xs ys =
  map reverse $ helper xs ys [[]] where
  helper (x:xs) (y:ys) acc = helper xs ys $
    (map (x:) acc) <> (map (y:) acc)
  helper _ _ acc = acc

hptToPrimes (Alpha xs) = nub $ map (primeForm . (0:)) $
  filter (\ys -> ys == nub ys) $
  choicesFromLists xs $ map (0 -) xs

-- an interesting quirk happens on things like Alpha [5,5]. Before adding a nub into normal form calculator, we got prime forms that don't exist. That was a genuine bug. But even after that, we get Alpha [5,5] yielding [0,5] as a prime form. This isn't entirely incorrect due to the symmetric situation, but we probably want to rule it out. To do so, I test whether a list has duplicates very inefficiently by checking for equality to its nubbed version

-- now that I have these grossly inefficient programs, I can investigate the profound structure of Hauptmann scripts

-- first, extendByHpt takes a list of prime forms and returns a list of all prime forms who have some Hauptmann script in common with the starting one

extendByHpt = nub . concat . (map hptToPrimes) . nub . concat . (map primeToHpts)

-- allExtensionsByHpt continues this until the process stabilizes
allExtensionsByHpt pfs =
  let extended = extendByHpt pfs in
    if (pfs == extended) then sort extended else (allExtensionsByHpt extended)

pfToAllExts pf = allExtensionsByHpt [pf]

-- also, let's do the reverse of this, starting with Hauptmann scripts
extendByPf = nub . concat . (map primeToHpts) . nub . concat . (map hptToPrimes)

allExtensionsByPf hpts =
  let extended = extendByPf hpts in
    if (hpts == extended) then sort extended else (allExtensionsByPf extended)

hptToAllExts hpt = allExtensionsByPf [hpt]

-- the next step is to classify everything by brute force.
-- let's just classify Hauptmann scripts, there are enough classifications of set-types

-- to display things nicely: how to show lists on multiple lines
listLines xs = putStr $ concat $ map ((++ "\n") . show) xs

allIntervals = [1..6] -- weird type error when trying to adapt from edo

allTriScripts = nub $ [Alpha $ sort [x,y] | x<-allIntervals, y<-allIntervals]

allTriScriptFams = nub $ map hptToAllExts allTriScripts

allTetraScripts = nub $ [Alpha $ sort [x,y,z] | x<-allIntervals, y<-allIntervals, z<-allIntervals]

allTetraScriptFams = nub $ map hptToAllExts allTetraScripts

allHexScripts = nub $ [Alpha $ sort [a,b,c,d,e] | a<-allIntervals, b<-allIntervals, c<-allIntervals, d<-allIntervals, e<-allIntervals]

allHexScriptFams = nub $ map hptToAllExts allHexScripts


