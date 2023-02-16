{-# LANGUAGE Arrows #-}

import Euterpea
import Control.Arrow
import FRP.UISF


-- Fixed frequency method
s1 :: AudSF () Double
s1 = proc () -> do
  s <- oscFixed 440 -< ()
  outA -< s

-- Same as:
s1' :: AudSF () Double
s1' = proc () -> do
  oscFixed 440 -< ()


-- With wavetable:
tab1 :: Table
tab1 = tableSinesN 4096 [1]

s2 :: Clock c => SigFun c () Double
s2 = proc () -> do
  osc tab1 0 -< 440

-- Same as:
s2' :: Clock c => SigFun c () Double
s2' = constA 440 >>> osc tab1 0

s2a :: AudSF () Double
s2a = s2'


-- Wavetable is cool because the same signal function generator
-- can be used with multiple tables, of course:
tab2 :: Table
tab2 = tableSinesN 4096 [1,0.5,0.33]

s3 :: Clock c => SigFun c () Double
s3 = constA 440 >>> osc tab2 0


-- We can also use it for time-varying frequencies:
vibrato :: Clock c => Double -> Double -> SigFun c Double Double
vibrato vfrq dep = proc afrq -> do
  vib <- osc tab1 0 -< vfrq
  aud <- osc tab2 0 -< afrq + vib * dep
  outA -< aud

s5 :: AudSF () Double
s5 = constA 1000 >>> vibrato 5 20


-- Using signal functions with Music types

mel :: Music1
mel = instrument simpleInstr m
  where m = Euterpea.line [na1 (c  4 en), na1 (ef 4 en), na1 (f  4 en),
                           na2 (af 4 qn), na1 (f  4 en), na1 (af 4 en),

                           na2 (bf 4 qn), na1 (af 4 en), na1 (bf 4 en),
                           na1 (c  5 en), na1 (ef 5 en), na1 (f  5 en),
                           na3 (af 5 wn)]
        na1 (Prim (Note d p)) = Prim (Note d (p, [Params [0, 0]]))
        na2 (Prim (Note d p)) = Prim (Note d (p, [Params [5,10]]))
        na3 (Prim (Note d p)) = Prim (Note d (p, [Params [5,20]]))

simpleInstr :: InstrumentName
simpleInstr = CustomInstrument "Simple Instrument"

myInstr :: Instr (AudSF () Double)
myInstr dur ap vol [vfrq, dep] = proc () -> do
  vib <- osc tab1 0 -< vfrq
  aud <- osc tab2 0 -< apToHz ap + vib * dep
  outA -< aud

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [(simpleInstr, myInstr)]

main = writeWav "simple.wav" myInstrMap mel
