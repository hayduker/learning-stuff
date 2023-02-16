import Data.Complex

-----------------------------------------------------------------
-- Here's how you could "invent" a logging monad inadvertently --
-----------------------------------------------------------------

-- Regular ol' functions
f, g :: Float -> Float
f x = (-1)*x
g x = 2*x

-- "Debuggable" versions of the functions
f', g' :: Float -> (Float,String)
f' x = ((-1)*x, "f was called.")
g' x = (2*x,    "g was called.")

-- It is hard to compose the two functions about since their inputs/outputs
-- don't align, so we introduce the "bind" function which makes this much easier:
bind0 :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind0 f = \(gx, gs) -> let (fx, fs) = f gx in (fx, gs++fs)

-- Then we can define a composition operator that uses "bind" implicitly
comp0 :: (Float -> (Float,String)) -> (Float -> (Float,String)) -> (Float -> (Float,String))
f `comp0` g = bind0 f . g

-- Is there an "identity" debuggable function (call it unit) such that
-- unit `comp0` f = f `comp0` unit = f
unit0 :: Float -> (Float,String)
unit0 x = (x, "")

-- Why do we need this function? Well, it allows us to "lift" any regular
-- function into a debuggable one via composition:
lift0 :: (Float -> Float) -> (Float -> (Float, String))
lift0 f = unit0 . f


-------------------------------------------------
-- Here's an example for multivalued functions --
-------------------------------------------------

--sqrt', cbrt' :: Complex Double -> [Complex Double]

bind1 :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
bind1 f = \xs -> concatMap f xs
-- Or more simply:
-- bind1 = concatMap

comp1 :: (Complex Double -> [Complex Double]) -> (Complex Double -> [Complex Double]) ->
         (Complex Double -> [Complex Double])
f `comp1` g = bind1 f . g

unit1 :: Complex Double -> [Complex Double]
unit1 x = [x]

lift1 :: (Complex Double -> Complex Double) -> (Complex Double -> [Complex Double])
lift1 f = unit1 . f

{- Here's a version that runs
sqrt', cbrt' :: Double -> [Double]
sqrt' x = [x*2, x*4]
cbrt' x = [x, x*3, x*5]

bind1 :: (Double -> [Double]) -> ([Double] -> [Double])
bind1 = concatMap

comp1 :: (Double -> [Double]) -> (Double -> [Double]) -> (Double -> [Double])
f `comp1` g = bind1 f . g

unit1 :: Double -> [Double]
unit1 x = [x]

lift1 :: (Double -> Double) -> (Double -> [Double])
lift1 f = unit1 . f
-}
