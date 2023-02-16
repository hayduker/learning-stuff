import System.Environment(getArgs)

getRatio :: Int -> Double
getRatio notesInOctave = 2 ** (1 / fromIntegral notesInOctave)

semitoneRatio :: Double
semitoneRatio = getRatio 12

c0Freq :: Double
c0Freq = c5Freq * 0.5 ^ 5
         where c5Freq = 220 * semitoneRatio ^ 3

midi2freq :: Int -> Double
midi2freq midiNote = c0Freq * semitoneRatio ^ midiNote

freq2midi :: Double -> Int
freq2midi frequency = round midinoteFrac
    where midinoteFrac = log (frequency / c0Freq) / log (semitoneRatio)

nToneChromatic :: Int -> Int -> [Double]
nToneChromatic midiStart notesInOctave = zipWith transpose freqs notes
    where freqStart = midi2freq midiStart
          numNotes = notesInOctave + 1
          freqs = take numNotes (repeat freqStart)
          notes = [0..numNotes-1]
          ratio = getRatio notesInOctave
          transpose f n = f * ratio ^ n

main :: IO ()
main = do strings <- getArgs
          if (length strings) /= 2
          then error "Invalid number of arguments."
          else mapM_ putStrLn strings


