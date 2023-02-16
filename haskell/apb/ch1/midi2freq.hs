midi2freq :: Int -> Double
midi2freq midinote = c0 * (semitoneRatio ^ midinote)
    where semitoneRatio = 2 ** (1 /12.0)
          c5 = 220.0 * (semitoneRatio ^ 3)
          c0 = c5 * (0.5 ^ 5)

freq2midi :: Double -> Int
freq2midi frequency = round midinoteFrac
    where semitoneRatio = 2 ** (1 /12.0)
          c5 = 220.0 * (semitoneRatio ^ 3)
          c0 = c5 * (0.5 ^ 5)
          midinoteFrac = log (frequency / c0) / log (semitoneRatio)

