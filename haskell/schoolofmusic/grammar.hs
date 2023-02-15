import System.Random
import Data.List
import Euterpea

data Grammar a = Grammar a -- start sentence
                 (Rules a) -- production rules
    deriving Show

data Rules a = Uni [Rule a]
             | Sto [(Rule a, Prob)]
    deriving (Eq, Ord, Show)

data Rule a = Rule {lhs :: a, rhs :: a}
    deriving (Eq, Ord, Show)

type Prob = Double

type ReplFun a = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])
type Rand      = Double

gen :: Ord a => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed = if checkProbs newRules
                               then generate f newRules (s,rands)
                               else error "Stochastic rule-set is malformed."
    where Sto newRules = toStoRules rules
          rands        = randomRs (0.0,1.0) (mkStdGen seed)

toStoRules :: (Ord a, Eq a) => Rules a -> Rules a
toStoRules s@(Sto rs) = s
toStoRules (Uni rs) = Sto (concatMap insertProb rs')
    where rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)

insertProb :: [a] -> [(a, Prob)]
insertProb rules = zip rules (repeat prb)
    where prb = 1.0 / fromIntegral (length rules)

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = and (map checkSum (groupBy sameLHS (sort rs)))

eps = 0.001

checkSum :: [(Rule a, Prob)] -> Bool
checkSum rules = abs (1.0 - mySum) <= eps
                 where mySum = sum (map snd rules)

sameLHS :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool
sameLHS (r1,f1) (r2,f2) = lhs r1 == lhs r2

probDist :: Eq a => [(Rule a, Prob)] -> [(Rule a, Prob)]
probDist rrs = zip rs (tail (scanl (+) 0 ps))
    where (rs,ps) = unzip rrs

generate :: Eq a => ReplFun a -> [(Rule a, Prob)] -> (a, [Rand]) -> [a]
generate f rules xs = map fst (iterate (f newRules) xs)
    where newRules = map probDist (groupBy sameLHS rules)


{------------------------
 - Algae string example -
 ------------------------}

redAlgae :: Grammar String
redAlgae = Grammar "a"
                   (Uni [Rule "a"  "b|c",  Rule "b" "b",
                         Rule "c"  "b|d",  Rule "d" "e\\d",
                         Rule "e"  "f",    Rule "f" "g",
                         Rule "g"  "h(a)", Rule "h" "h",
                         Rule "|"  "|",    Rule "(" "(",
                         Rule ")"  ")",    Rule "/" "\\",
                         Rule "\\" "/"])

replStr :: ReplFun String
replStr rules (s,rands) = (s',rands')
    where s'      = concat $ zipWith (\s -> getNewRHS rules [s]) s rands
          rands'  = drop (length s) rands



{----------------------
 - L-system for music -
 ----------------------}

data LSys a = N a
            | LSys a :+ LSys a
            | LSys a :. LSys a
            | Id
    deriving (Eq,Ord,Show)

replFun :: Eq a => ReplFun (LSys a)
replFun rules (s,rands) = case s of
    a :+ b -> (a' :+ b', rands'')
              where (a', rands')  = replFun rules (a, rands)
                    (b', rands'') = replFun rules (b, rands')
    a :. b -> (a' :. b', rands'')
              where (a', rands')  = replFun rules (a, rands)
                    (b', rands'') = replFun rules (b, rands')
    Id     -> (Id, rands)
    N x    -> (getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: Eq a => [[(Rule a,Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand = case (find (\((r,p):_) -> lhs r == ls) rrs) of
    Just rs -> loop rs
    Nothing -> error "No rule match."
    where loop ((r,p):rs) = if rand <= p then rhs r else loop rs
          loop []         = error "getNewRHS anomaly"

type IR a b = [(a, Music b -> Music b)]  -- interpretation rules 

interpret :: (Eq a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b) r m = interpret a r (interpret b r m)
interpret (a :+ b) r m = interpret a r m :+: interpret b r m
interpret Id       r m = m
interpret (N x)    r m = case (lookup x r) of
    Just f  -> f m
    Nothing -> error "No interpretation rule."

data LFun = Same | Up3rd | Up5th | Up7th | Down3rd | Down5th | Down7th
    deriving (Eq,Ord,Show)

ir :: IR LFun Pitch
ir = [(Same,    id),
      (Up3rd,   Euterpea.transpose 4),
      (Up5th,   Euterpea.transpose 7),
      (Up7th,   Euterpea.transpose 11),
      (Down3rd, Euterpea.transpose (-4)),
      (Down5th, Euterpea.transpose (-7)),
      (Down7th, Euterpea.transpose (-11))]

same, up3rd, up5th, up7th, down3rd, down5th, down7th :: LSys LFun
same    = N Same
up3rd   = N Up3rd
up5th   = N Up5th
up7th   = N Up7th
down3rd = N Down3rd
down5th = N Down5th
down7th = N Down7th


g1 = Grammar same (Uni [Rule same (same :+ up3rd :+ up5th)])

t1 n = instrument Vibraphone $
       interpret (gen replFun g1 42 !! n) ir (c 5 en)


