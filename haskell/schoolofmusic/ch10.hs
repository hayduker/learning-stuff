import Euterpea

data Cluster = Cluster SNote [Cluster]
type SNote   = (Dur, AbsPitch)

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0,0) (map mkCluster pat)
              where mkCluster note = Cluster note (map (mkCluster . addMult note) pat)

addMult :: SNote -> SNote -> SNote
addMult (d0, p0) (d1, p1) = (d0*d1, p0+p1)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap (fringe (n-1)) cls


{-

let l = [(hn,3), (hn,5), (hn,1)]

selfSim l =

Cluster (0,0) (map mkCluster l) =

Cluster (0,0) [Cluster (hn,3) (map (mkCluster . addMult (hn,3)) l),
               Cluster (hn,5) (map (mkCluster . addMult (hn,5)) l),
               Cluster (hn,1) (map (mkCluster . addMult (hn,1)) l)] =

Cluster (0,0) [Cluster (hn,3) (map (mkCluster . addMult (hn,3)) l),
Cluster (0,0) [Cluster (hn,3) [Cluster (qn,6)  (map (mkCluster . addMult (qn,6))  l),
                               Cluster (qn,8)  (map (mkCluster . addMult (qn,8))  l),
                               Cluster (qn,4)  (map (mkCluster . addMult (qn,4))  l)],
               Cluster (hn,5) [Cluster (qn,8)  (map (mkCluster . addMult (qn,8))  l),
                               Cluster (qn,10) (map (mkCluster . addMult (qn,10)) l),
                               Cluster (qn,6)  (map (mkCluster . addMult (qn,6))  l)],
               Cluster (hn,1) [Cluster (qn,4)  (map (mkCluster . addMult (qn,4))  l),
                               Cluster (qn,6)  (map (mkCluster . addMult (qn,6))  l),
                               Cluster (qn,2)  (map (mkCluster . addMult (qn,2))  l)]]


-}


simToMusic :: [SNote] -> Music Pitch
simToMusic = line . map mkNote

mkNote :: (Dur,AbsPitch) -> Music Pitch
mkNote (d,ap) = note d (pitch ap)

ss :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch
ss pat n tr te = 
    transpose tr $ tempo te $ simToMusic $ fringe n $ selfSim pat

-- Examples

m0 :: [SNote]
m0 = [(1,2),(1,0),(1,5),(1,7)]
tm0 = instrument Vibraphone (ss m0 4 50 20)
ttm0 = tm0 :=: transpose (12) (retro tm0)

m1 :: [SNote]
m1 = [(1,0), (0.5,0), (0.5,0)]
tm1 = instrument Percussion (ss m1 4 43 2)
