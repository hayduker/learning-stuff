import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Char

-- Reader Monad

runR :: IO ()
runR = do print (runReader rf1 "base!")

rf1 :: Reader String String
rf1 = do str <- ask
         return (str ++ " from f1")

rf2 :: Reader String String
rf2 = do str <- ask
         return (str ++ " from f2")

rf3 :: Reader String String
rf3 = do str <- ask
         f1s <- rf1
         f2s <- rf2
         return (str ++ " from f2" ++ f1s ++ f2s)


-- Int needs to be a Monoid to work with Writer Monad
instance Semigroup Int where
    a <> b = a + b

instance Monoid Int where
    mempty = 0

-- Writer Monad

runW :: Int -> (Int, Int)
runW input = runWriter (wf0 input)

wf0 :: Int -> Writer Int Int
wf0 input = do tell input
               wf1 input

wf1 :: Int -> Writer Int Int
wf1 input = do tell 1
               wf2 (input*1)

wf2 :: Int -> Writer Int Int
wf2 input = do tell 2
               wf3 (input*2)

wf3 :: Int -> Writer Int Int
wf3 input = do tell 3
               wf4 (input*3)

wf4 :: Int -> Writer Int Int
wf4 input = do return input


-- Using State Monad

runS :: String -> (String, String)
runS input = runState (sf0 input) "initial"

sf0 :: String -> State String String
sf0 input = do str <- get
               let new = (map toUpper str) ++ " from sf0"
               put new
               return (input ++ " added to input")


