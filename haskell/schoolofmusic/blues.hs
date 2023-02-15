import Euterpea

data BluesPitchClass = Ro | MT | Fo | Fi | MS
     deriving (Show, Read, Eq)

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o); mt o d = note d (MT, o)
fo o d = note d (Fo, o); fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (bpc, o))) = case bpc of
                                     Ro -> note d (C, o)
                                     MT -> note d (Ef, o)
                                     Fo -> note d (F, o)
                                     Fi -> note d (G, o)
                                     MS -> note d (Bf, o)  
fromBlues (Prim (Rest d)) = rest d
fromBlues (m1 :+: m2) = (fromBlues m1) :+: (fromBlues m2)
fromBlues (m1 :=: m2) = (fromBlues m1) :=: (fromBlues m2)
fromBlues (Modify c m) = Modify c (fromBlues m)
