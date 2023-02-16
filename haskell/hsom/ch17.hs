{-# LANGUAGE Arrows #-}

import Euterpea
import Control.Arrow
import FRP.UISF


secondCounter :: UISF () Integer
secondCounter = proc () -> do
    rec count <- fcdelay 0 1 -< count + 1
    outA -< count


-- Simple UIs

ui0 :: UISF () ()
ui0 = hiSlider 1 (0,100) 0 >>> arr pitch >>> display

mui0 = runUI' ui0


ui1 :: UISF () ()
ui1 = setSize(150,150) $
  proc _ -> do
    ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
    title "Pitch" display -< pitch ap

mui1 = runUI' ui1


ui2 :: UISF () ()
ui2 = leftRight $
  proc _ -> do
    ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
    title "Pitch" display -< pitch ap

mui2 = runUI' ui2


ui3 :: UISF () ()
ui3 = proc _ -> do
  devid <- selectOutput -< ()
  ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
  title "Pitch" display -< pitch ap
  uap <- unique -< ap
  midiOut -< (devid, fmap (\k -> [ANote 0 k 100 0.1]) uap)

mui3 = runUI' ui3


