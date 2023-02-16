import Euterpea

twice :: (a -> a) -> (a -> a)
twice f = f . f

power :: (a -> a) -> Int -> (a -> a)
power f n = foldl (.) f $ replicate (n-1) f

fix :: (a -> a) -> a
fix f = f (fix f)

sum1 :: [Integer] -> Integer
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

sum' :: ([Integer] -> Integer) -> [Integer] -> Integer
sum' _ [] = 0
sum' rec (x:xs) = x + rec xs
sum2 = fix sum'

-- sum2 [1,2,3] =
-- fix sum' [1,2,3] =
-- sum' (fix sum') [1,2,3] =
-- 1 + fix sum' [2,3] = 
-- 1 + sum' (fix sum') [2,3] =
-- 1 + 2 + fix sum' [3] =
-- 1 + 2 + sum' (fix sum') [3] =
-- 1 + 2 + 3 + fix sum' [] =
-- 1 + 2 + 3 + sum' (fix sum') [] =
-- 1 + 2 + 3 + 0 =
-- 6

remainder1 :: Integer -> Integer -> Integer
remainder1 a b = if a < b then a
                          else remainder1 (a-b) b

remainder' :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
remainder' rec a b = if a < b then a
                              else rec (a-b) b
remainder2 = fix remainder'

-- remainder2 13 4 =
-- fix remainder' 13 4 =
-- remainder' (fix remainder') 13 4 =
-- fix remainder' 9 4 =
-- remainder' (fix remainder') 9 4 =
-- fix remainder' 5 4 =
-- remainder' (fix remainder') 5 4 =
-- fix remainder' 1 4 =
-- remainder' (fix remainder') 1 4 =
-- 1


apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch,AbsPitch)]
apPairs aps1 aps2 = [(ap1,ap2) | ap1 <- aps1, ap2 <- aps2, abs (ap1-ap2) > 2 && abs (ap1-ap2) < 8]

toQN :: AbsPitch -> Music Pitch
toQN ap = note qn (pitch ap)

harmonizePairs :: [(AbsPitch,AbsPitch)] -> Music Pitch
harmonizePairs pairs = line (map makeDiad pairs)
                       where makeDiad (ap1,ap2) = toQN ap1 :=: toQN ap2



