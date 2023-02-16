import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainLoop inh outh
       hClose inh
       hClose outh

mainLoop :: Handle -> Handle -> IO ()
mainLoop inh outh =
    do ineof <- hIsEOF inh
       if ineof
       then return ()
       else do inpStr <- hGetLine inh
               hPutStrLn outh (map toUpper inpStr)
               mainLoop inh outh

main :: IO ()
main = do inh  <- openFile "input.txt"  ReadMode
          outh <- openFile "output.txt" WriteMode
          toUpperFile inh outh
          hClose inh
          hClose outH

toUpperFile :: Handle -> Handle -> IO ()
toUpperFile inh outh =
    do c <- hGetContents inh
       hPutStrLn outh (toUpper c)
