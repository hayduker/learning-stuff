import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do inh  <- openFile "input.txt"  ReadMode
          outh <- openFile "output.txt" WriteMode
          inStr <- hGetContents inh
          let outStr = capitalize inStr
          hPutStrLn outh outStr
          hClose inh
          hClose outh

capitalize :: String -> String
capitalize = map toUpper
