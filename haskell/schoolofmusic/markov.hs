import Euterpea
import Data.MarkovChain
import System.Random

-- Some training sequences
ps0, ps1, ps2 :: [Pitch]
ps0 = [(C,4), (D,4), (E,4)]
ps1 = [(C,4), (D,4), (E,4), (F,4), (G,4), (A,4), (B,4)]
ps2 = [(C,4), (E,4), (G,4), (E,4), (F,4), (A,4), (G,4), (E,4),
       (C,4), (E,4), (G,4), (E,4), (F,4), (D,4), (C,4)]

-- Functions to pacakge up training code
mc  ps  n = mkLine3         (run      n ps  0 (mkStdGen 42))
mcm pss n = mkLine3 (concat (runMulti n pss 0 (mkStdGen 42)))

-- Music-making functions
mkNote3 :: Pitch -> Music Pitch
mkNote3 = note sn
mkLine3 :: [Pitch] -> Music Pitch
mkLine3 ps = line (take 64 (map mkNote3 ps))

