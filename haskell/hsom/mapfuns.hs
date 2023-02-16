import Euterpea

transPitches :: Int -> [Pitch] -> [Pitch]
transPitches ap pitches = map (trans ap) pitches
-- same as curried form:   transPitches ap = map (trans ap)

makeRests :: [Dur] -> [Music a]
makeRests = map rest
-- same as uncurried form:   makeRests ds = map rest ds


halveDur :: Music Pitch -> Music Pitch
halveDur (Prim (Note d p)) = Prim (Note hd p) :+: Prim (Rest hd)
                             where hd = d/2

makeStaccato :: [Music Pitch] -> [Music Pitch]
makeStaccato ps = map halveDur ps

wts :: Pitch -> [Music Pitch]
wts pitch = map f [0,2,4,6,8]
            where f ap = note qn (trans ap pitch)


(~~) :: [a] -> [a] -> [a]
[] ~~ ys = ys
(x:xs) ~~ ys = x:(xs ~~ ys)


myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl op init []     = init
myfoldl op init (x:xs) = myfoldl op (op init x) xs

myreverse :: [a] -> [a]
myreverse xs = myfoldl revelem [] xs
               where revelem acc x = x : acc 

myreverse2 :: [a] -> [a]
myreverse2 = myfoldl (flip (:)) []
-- 3 curries here: 
--    1. f xs = myfoldl g [] xs    same as    f = myfoldl g []
--    2. revelem acc x = flip (:) acc x   same as   revelem = flip (:) 

applyEach2 [] x = []
applyEach2 (f:fs) x = (f x):(applyEach2 fs x)

applyEach fs x = map apply fs
                 where apply f = f x

applyEach4 fs x = foldr apply [] fs
                  where apply f acc = (f x) : acc


applyAllF fs x = foldr apply x fs
                 where apply f acc = f acc

applyAllB fs x = foldl apply x fs
                 where apply acc f = f acc


myLength :: [a] -> Int
myLength = foldr step 0
           where step _ = (+) 1

double :: Num a => [a] -> [a]
double = map (*2)

--addEachPair :: Num a => [(a,a)] => [a]
addEachPair = map step
              where step (x,y) = x + y

addPointwise xs = foldr step (0,0) xs
                  where step (l,r) (lacc,racc) = (l+lacc,r+racc)

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse durs nfs = zipWith apply nfs durs
                where apply notefun dur = notefun dur

maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch = foldr max (-3)

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch = foldr min 9999999

chrom :: Dur -> Pitch -> Pitch -> Music Pitch
chrom dur p1 p2 = foldr step (rest 0) transRange
                  where ap1 = absPitch p1; ap2 = absPitch p2
                        transRange = if ap1 < ap2 then [0..(ap2-ap1)] else [0,-1..(ap2-ap1)]
                        step t acc = note dur (trans t p1) :+: acc

mkScale :: Dur -> Pitch -> [Int] -> Music Pitch
mkScale dur p ts = fst (foldl step (rest 0, p) (0:ts))
                   where step (acc,lp) t = (acc :+: note dur (trans t lp), (trans t lp))


shift :: [a] -> Int -> [a]
shift xs 0 = xs
shift (x:xs) n = shift (xs ++ [x]) (n-1)

genScale :: Mode -> [Int]
genScale m = case m of Ionian     -> [2,2,1,2,2,2,1]
                       Dorian     -> shift (genScale Ionian) 1
                       Phrygian   -> shift (genScale Ionian) 2
                       Lydian     -> shift (genScale Ionian) 3
                       Mixolydian -> shift (genScale Ionian) 4
                       Aeolian    -> shift (genScale Ionian) 5
                       Locrian    -> shift (genScale Ionian) 6

