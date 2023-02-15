import System.Random
import System.Random.Distributions
import Euterpea

sGen = mkStdGen 42

toAbsP1 :: Float -> AbsPitch
toAbsP1 x = round (40*x + 30)

mkNote1 :: AbsPitch -> Music Pitch
mkNote1 = note sn . pitch

mkLine1 :: [AbsPitch] -> Music Pitch
mkLine1 rands = line (take 64 (map mkNote1 rands))

-- uniform distribution
m1 :: Music Pitch
m1 = mkLine1 (randomRs (30,70) sGen)

--linear distribution
m2 :: Music Pitch
m2 = mkLine1 (map toAbsP1 rs1)
     where rs1 = rands linear sGen

-- exponential distribution
m3 :: Float -> Music Pitch
m3 lam = mkLine1 (map toAbsP1 rs1)
         where rs1 = rands (exponential lam) sGen

-- Gaussian distribution
m4 :: Float -> Float -> Music Pitch
m4 sig mu = mkLine1 (map toAbsP1 rs1)
            where rs1 = rands (gaussian sig mu) sGen


-- Random walk

toAbsP2 :: Float -> AbsPitch
toAbsP2 x = round (5*x)

mkLine2 :: AbsPitch -> [AbsPitch] -> Music Pitch
mkLine2 start rands = line (take 64 (map mkNote1 (scanl (+) start rands)))

m5 :: Float -> Music Pitch
m5 sig = mkLine2 50 (map toAbsP2 rs1)
         where rs1 = rands (gaussian sig 0) sGen

m6 :: Float -> Music Pitch
m6 lam = mkLine2 50 (map (toAbsP2 . subtract (1/lam)) rs1)
         where rs1 = rands (exponential lam) sGen




-- Random duration

toDur :: Int -> Dur
toDur i = fromIntegral i * tn

mkLine3 :: Pitch -> [Dur] -> Music Pitch
mkLine3 p (d1:d2:ds) = note d1 p :+: rest d2 :+: mkLine3 p ds

lineFromPitch :: Pitch -> Music Pitch
lineFromPitch p = mkLine3 p ds
                  where ds = map toDur (randomRs (1,8) sGen)
                        sGen = mkStdGen (absPitch p) -- Use abs pitch as seed

m7 :: Music Pitch
m7 = instrument Vibraphone $
     chord (map lineFromPitch [(C,4), (E,4), (G,4), (B,4)])

