import Euterpea

toNotes :: Pitch -> [Dur] -> [Int] -> Music Pitch
toNotes p durs ts = line (zipWith makenote durs ts)
                    where makenote d t = note d (trans t p)

twice :: Music Pitch -> Music Pitch
twice mp = mp :+: mp

frere :: Pitch -> Music Pitch
frere p = foldr step (rest 0) [phr1,phr2,phr3,phr4]
          where step phr acc = twice phr :+: acc
                phr1 = toNotes p [qn,qn,qn,qn]       [0,2,4,0]
                phr2 = toNotes p [qn,qn,qn]          [4,5,7]       :+: rest qn
                phr3 = toNotes p [en,en,en,en,qn,qn] [7,9,7,5,4,0]
                phr4 = toNotes p [qn,qn,qn]          [0,(-5),0]    :+: rest qn

makeRest :: Dur -> Int -> Music a
makeRest d m = foldr addRest (rest 0) [1..m]
               where addRest k acc = rest d :+: acc

frereRound :: Pitch -> [InstrumentName] -> Music Pitch
frereRound p insts = chord (zipWith makePart insts [0..(length insts)-1])
                     where makePart i r = (Modify (Instrument i) (makeRest wn r :+: frere p))


