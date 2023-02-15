import Euterpea

putCharList :: String -> [IO ()]
putCharList = map putChar

myPutStr :: String -> IO ()
myPutStr = sequence_ . putCharList

-- Or just

myPutStr2 :: String -> IO ()
myPutStr2 = sequence_ . map putChar

-- Or recursively

myPutStr3 :: String -> IO ()
myPutStr3 []     = return ()
myPutStr3 (c:cs) = do putChar c
                      putStr cs


-- Working with MIDI files

--writeMidi "myMidiFile.mid" (c 4 qn)


readMusic :: FilePath -> IO Music1
readMusic inFile = do x <- importFile inFile
                      case x of Left str -> error str
                                Right m -> return $ fromMidi m

