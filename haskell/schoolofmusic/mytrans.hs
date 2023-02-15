import Euterpea

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = note d (pitch (absPitch p + ap))
transM ap (Prim (Rest d))   = rest d
transM ap (m1 :+: m2)       = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2)       = transM ap m1 :=: transM ap m2
transM ap (Modify c m)      = Modify c (transM ap m)
