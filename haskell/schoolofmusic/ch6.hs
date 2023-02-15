import Euterpea

apsToQNLine :: [AbsPitch] -> Music Pitch
apsToQNLine aps = line $ map apToNote aps
                  where apToNote ap = note qn (pitch ap) 

apsToQNLineWRests :: [AbsPitch] -> Music Pitch
apsToQNLineWRests aps = line $ map apToNote aps
                        where apToNote ap = note qn (pitch ap) :+: rest en


proper1   = apsToQNLine [60..71]
proper2   = apsToQNLine [71,70..60]
proper3   = apsToQNLineWRests [60..71]
proper4   = apsToQNLineWRests [71,70..60]
improper1 = apsToQNLine [60,62..70]
improper2 = apsToQNLine [60..72]

pcToStandard :: PitchClass -> PitchClass
pcToStandard pc = case pc of Cff -> As; Cf -> B;  C -> C; Cs -> Cs; Css -> D
                             Dff -> C;  Df -> Cs; D -> D; Ds -> Ds; Dss -> E
                             Eff -> D;  Ef -> Ds; E -> E; Es -> F;  Ess -> Fs
                             Fff -> Ds; Ff -> E;  F -> F; Fs -> Fs; Fss -> G
                             Gff -> F;  Gf -> Fs; G -> G; Gs -> Gs; Gss -> A
                             Aff -> G;  Af -> Gs; A -> A; As -> As; Ass -> B
                             Bff -> A;  Bf -> As; B -> B; Bs -> C;  Bss -> Cs

length12 :: [a] -> Bool
length12 l = length l == 12

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) | elem x xs = unique xs
              | otherwise = x : unique xs

pitchClasses :: [Music Pitch] -> [PitchClass]
pitchClasses mps = map f mps
                   where f (Prim (Note d (pc,o))) = pc

properRow :: Music Pitch -> Bool
properRow ln = length12 l && (length12 . unique . pitchClasses) l
               where l = lineToList ln

revM :: Music a -> Music a
revM n@(Prim _) = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :+: m2) = revM m2 :+: revM m1
revM (m1 :=: m2) =
    let d1 = dur m1;   d2 = dur m2
        rm1 = revM m1; rm2 = revM m2
    in if d1 <= d2 then (rest (d2-d1) :+: rm1) :=: rm2
                     else rm1 :=: (rest (d1-d2) :+: rm2)


trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i sDur (Prim (Note tDur p)) =
    if sDur >= tDur then note tDur p
                    else note sDur p :+:
                         trill (negate i) sDur
                               (note (tDur - sDur) (trans i p))
trill i d (Modify (Tempo r) m) = tempo r (trill i (d * r) m)
trill i d (Modify c m)         = Modify c (trill i d m)
trill _ _ _                    = error "trill: input must be a single note"

trill' :: Int -> Dur -> Music Pitch -> Music Pitch
trill' i sDur m = trill (negate i) sDur (transpose i m)

trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i nTimes m = trill i (dur m / fromIntegral nTimes) m

trilln' :: Int -> Int -> Music Pitch -> Music Pitch
trilln' i nTimes m = trilln (negate i) nTimes (transpose i m)

roll :: Dur -> Music Pitch -> Music Pitch
roll dur m = trill 0 dur m

rolln :: Int -> Music Pitch -> Music Pitch
rolln nTimes m = trilln 0 nTimes m

funkGroove :: Music Pitch
funkGroove
    = let p1 = perc LowTom        qn
          p2 = perc AcousticSnare en
      in tempo 3 $ cut 8 $ forever
         ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
           p1 :+: p1 :+: qnr :+: p2 :+: enr)
           :=: roll en (perc ClosedHiHat 2))

allPerc :: Music Pitch
allPerc = line $ map makePerc percEnum
          where makePerc ps = perc ps qn
                percEnum = enumFrom (toEnum 0 :: PercussionSound)


scaleVolume :: Rational -> Music (Pitch,Volume) -> Music (Pitch,Volume)
scaleVolume s m = mMap step m
                  where step (p,v) = (p, round (s * fromIntegral v))


myretro :: Music a -> Music a
myretro = mFold Prim g h Modify
          where g m1 m2 = myretro m2 :+: myretro m1
                h m1 m2 = if d1 > d2 then myretro m1 :=: (rest (d1-d2) :+: myretro m2)
                                     else (rest (d2-d1) :+: myretro m1) :=: myretro m2
                             where d1 = dur m1; d2 = dur m2


insideOut :: Music a -> Music a
insideOut = mFold Prim (:=:) (:+:) Modify


rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n-1) (f m))
 
run      = rep (transpose 5) (offset tn) 8 (c 4 tn)
cascade  = rep (transpose 4) (offset en) 8 run
cascades = rep id            (offset sn) 2 cascade
final    = cascades :+: retro cascades

run'      = rep (offset tn) (transpose 5) 8 (c 4 tn)
cascade'  = rep (offset en) (transpose 4) 8 run'
cascades' = rep (offset sn) id            2 cascade'
final'    = cascades' :+: retro cascades'




intervals :: [Int] -> [Int]
intervals xs = zipWith (-) (tail xs) (take (length xs - 1) xs)

getIntervals :: [Int] -> [[Int]]
getIntervals [x] = [[x]]
getIntervals xs = xs : getIntervals (intervals xs)

getHeads :: [[Int]] -> [Int]
getHeads = map head

intervalClosure :: [Int] -> [Int]
intervalClosure = getHeads . getIntervals

intervalClosures :: [Int] -> [[Int]]
intervalClosures xs = xs : intervalClosures (intervalClosure xs)


-- Shepard's tone stuff

makeEn :: AbsPitch -> Music Pitch
makeEn ap = note en (pitch ap)

sLine :: Bool -> Music Pitch
sLine True  = line $ map makeEn [-12..136]
sLine False = line $ map makeEn [136,135..(-12)]

shepardTone :: Bool -> Music Pitch
shepardTone up = chord $ map restThenLine [0..4]
                 where restThenLine i = (rest (i*wn)) :+: sLine up



-- sheetsofsound
-- shepardtone
-- parashant
-- strangeloop
-- iamastrangeloop
