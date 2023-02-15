import Euterpea


-- Color

data Color = Red | Green | Blue
     deriving (Show,Read)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

instance Ord Color where
    Red   <= Red   = True
    Red   <= Green = True
    Red   <= Blue  = True
    Green <= Red   = False
    Green <= Green = True
    Green <= Blue  = True
    Blue  <= Red   = False
    Blue  <= Green = False
    Blue  <= Blue  = True

instance Enum Color where
    toEnum 0       = Red
    toEnum 1       = Green
    toEnum 2       = Blue
    fromEnum Red   = 0
    fromEnum Green = 1
    fromEnum Blue  = 2

instance Bounded Color where
    minBound = Red
    maxBound = Blue


-- Temporal

class Temporal a where
    durT    :: a -> Dur
    cutT    :: Dur -> a -> a
    removeT :: Dur -> a -> a

instance Temporal (Music a) where
    durT m      = dur m
    cutT d m    = cut d m
    removeT d m = remove d m

instance Temporal (Primitive a) where
    durT (Note d _) = d
    durT (Rest d)   = d
    cutT cutD n@(Note origD p) = if origD > cutD then Note cutD  p
                                                 else n
    cutT cutD r@(Rest origD)   = if origD > cutD then Rest cutD
                                                 else r
    removeT remD (Note origD p) = Note (max (origD - remD) 0) p
    removeT remD (Rest origD)   = Rest (max (origD - remD) 0)


-- Eq for functions

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
    f == g = map f as == map g as
             where as = [(minBound::a)..(maxBound::a)]



data OtherColor = Black | Blu | Gren | Cyan | Rad | Magenta | Yellow | White
     deriving (Show, Eq, Ord, Enum, Bounded)


upColor :: OtherColor -> OtherColor
upColor c = toEnum (upshift $ fromEnum c) :: OtherColor
            where upshift n = mod (n+1) (fromEnum (maxBound::OtherColor) + 1)

downColor :: OtherColor -> OtherColor
downColor c = toEnum (downshift $ fromEnum c) :: OtherColor
              where downshift n = mod (n-7) (fromEnum (maxBound::OtherColor) + 1)


