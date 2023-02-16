import Euterpea

combinePitches :: Int -> [PitchClass] -> [Dur] -> Music Pitch
combinePitches o ps ds = line $ zipWith f ps ds
                         where f p d = Prim (Note d (p, o))

combinePercs :: [PercussionSound] -> [Dur] -> Music Pitch
combinePercs ps ds = line $ zipWith f ps ds
                     where f p d = perc p d

bassPart :: Music Pitch
bassPart = instrument AcousticBass $
  combinePitches 2
    [Cs, Ds, Fs, Gs, Fs, Ds, Cs, Ds, Fs, Gs, Cs,  Cs,  Cs, Ds, Fs, Gs, Cs, Ds, Fs, Gs, B,  Bss]
    [qn, en, en, qn, en, en, en, en, en, qn, den, den, qn, en, en, qn, en, en, en, en, en, (5/8)]


drumPart :: Music Pitch
drumPart = stickTriplets :=: hat :=: snare
           where stickTriplets = tempo (3/2) $ times 24 $ perc SideStick qn
                 hat = times 4 $ combinePercs [ClosedHiHat, ClosedHiHat, ClosedHiHat, ClosedHiHat]
                                              [qn,          qn,          qn,          qn         ]
                 snare = times 4 $ hnr :+: (sfnr :+: perc AcousticSnare (15/64)) :+: qnr


vibesPart :: Music Pitch
vibesPart = instrument Vibraphone $ tempo (3/2) $ 
    enr :+: (times 23 $ cs 4 qn :=: ds 4 qn) :+: (cs 4 en :=: ds 4 en)

song = bassPart :=: drumPart :=: vibesPart
