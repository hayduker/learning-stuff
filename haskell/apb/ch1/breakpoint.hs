type Time  = Double
type Value = Double
type Breakpoint = (Time,Value)

maxpoint :: [Breakpoint] -> Breakpoint
maxpoint = foldr1 (\b0@(t0,v0) b1@(t1,v1) -> if v0 > v1 then b0 else b1)

str2bp :: String -> Maybe Breakpoint
str2bp s = if length ws == 2 then Just (t,v) else Nothing
    where ws = words s
          t = read $ head ws
          v = read $ head $ tail ws

getBreakpoints :: String -> [Breakpoint]
getBreakpoints fileContents =
    foldr accumulate [] ls
    where ls = lines fileContents
          accumulate l bps = case (str2bp l) of
                                  Nothing -> bps
                                  Just bp -> bp : bps

main :: IO ()
main = do inputStr <- readFile "example.brk"
          let bps = getBreakpoints inputStr
          mapM_ (putStrLn . show) bps
