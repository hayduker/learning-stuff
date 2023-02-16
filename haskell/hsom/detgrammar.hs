import Data.List
import Euterpea

{--------------------------------------------------
 - Defintion and example of deterministic grammar -
 --------------------------------------------------}

data DetGrammar a = DetGrammar a         -- start symbol
                               [(a,[a])] -- productions
    deriving Show

redAlgae = DetGrammar 'a'
    [('a',"b|c"),  ('b',"b"), ('c',"b|d"),
     ('d',"e\\d"), ('e',"f"), ('f',"g"),
     ('g',"h(a)"), ('h',"h"), ('|',"|"),
     ('(',"("),    (')',")"), ('/',"\\"),
     ('\\',"/")]

notEnoughAlgae = DetGrammar 'a'
    [('a',"b|c"),  ('b',"b"), ('c',"b|d"),
     ('d',"e\\d"), ('f',"g"),
     ('g',"h(a)"), ('h',"h"), ('|',"|"),
     ('(',"("),    (')',")"), ('/',"\\"),
     ('\\',"/")]

tooMuchAlgae = DetGrammar 'a'
    [('a',"b|c"),  ('b',"b"), ('c',"b|d"),
     ('d',"e\\d"), ('e',"f"), ('e',"bc"), ('f',"g"),
     ('g',"h(a)"), ('h',"h"), ('|',"|"),
     ('(',"("),    (')',")"), ('/',"\\"),
     ('\\',"/")]

{------------------------------------
 - Evaluating deterministic grammar -
 ------------------------------------}

unique :: Eq a => [a] -> [a]
unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

getSymbols :: Eq a => DetGrammar a -> [a]
getSymbols (DetGrammar st ps) = unique $ foldr f [st] ps
    where f rule acc = snd rule ++ acc

getHeads :: Eq a => DetGrammar a -> [a]
getHeads (DetGrammar _ ps) = foldr f [] ps
    where f rule acc = fst rule : acc

detGenerate :: Ord a => DetGrammar a -> [[a]]
detGenerate g@(DetGrammar st ps) = if testDet g
                                   then iterate (concatMap f) [st]
                                   else error "Grammar must be deterministic."
    where f a = maybe [a] id (lookup a ps)

testDet :: Ord a => DetGrammar a -> Bool
testDet g = sort (getHeads g) == sort (getSymbols g)

t n g = sequence_ (map putStrLn (take n (detGenerate g)))

st n g = last (take n (detGenerate g))

sts n g = take n (detGenerate g)

{------------------------------------------------
 - Interpreting deterministic grammars as music -
 ------------------------------------------------}

charToMusic :: AbsPitch -> Int -> Dur -> Char -> (Music Pitch, Int)
charToMusic ap mod d c = case c of 'a'  -> (note d (pitch (ap+mod)),    mod)
                                   'b'  -> (note d (pitch (ap+mod+2)),  mod)
                                   'c'  -> (note d (pitch (ap+mod+4)),  mod)
                                   'd'  -> (note d (pitch (ap+mod+5)),  mod)
                                   'e'  -> (note d (pitch (ap+mod+7)),  mod)
                                   'f'  -> (note d (pitch (ap+mod+9)),  mod)
                                   'g'  -> (note d (pitch (ap+mod+11)), mod)
                                   'h'  -> (note d (pitch (ap+mod+12)), mod)
                                   '|'  -> (rest 0,                     mod)
                                   '/'  -> (rest d,                     mod)
                                   '\\' -> (rest d,                     mod)
                                   '('  -> (rest 0,                     mod+5)
                                   ')'  -> (rest 0,                     mod-5)

strToMusic :: AbsPitch -> Dur -> String -> Music Pitch
strToMusic ap d str = fst $ foldl step (rest 0, 0) str
    where step (currMusic, currTrans) c = (currMusic :+: fst t, snd t)
              where t = charToMusic ap currTrans d c


algaeMusicSameStart :: AbsPitch -> Dur -> Int -> Music Pitch
algaeMusicSameStart ap d n = chord (map (strToMusic ap d) (sts n redAlgae))

algaeMusicSameEnd :: AbsPitch -> Dur -> Int -> Music Pitch
algaeMusicSameEnd ap d n = retro (chord (map (retro . strToMusic ap d) (sts n redAlgae)))

