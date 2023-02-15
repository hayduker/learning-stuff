import Euterpea

triad :: Mode -> Music Pitch -> Music Pitch 
triad mode mpitch = mpitch :=: third :=: fifth
                    where third | mode == Major = transpose 4 mpitch
                                | mode == Minor = transpose 3 mpitch
                          fifth = transpose 7 mpitch

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne pitch dur = two :+: five :+: one
                       where two = triad Minor (note dur (trans 2 pitch))
                             five = triad Major (note dur (trans 7 pitch))
                             one = triad Major (note dur pitch)


