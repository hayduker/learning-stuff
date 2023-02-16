import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do inh  <- openFile "input.txt"  ReadMode
          outh <- openFile "output.txt" WriteMode
          inStr <- hGetContents inh
          hPutStrLn outh (map toUpper inStr)
          hClose inh
          hClose outh
