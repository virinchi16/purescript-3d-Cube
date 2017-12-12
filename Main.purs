module Main where

import Prelude
import Control.Monad.Eff(Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Graphics.Canvas
import Data.Maybe(Maybe(Just))
import Partial.Unsafe(unsafePartial)

--Local File Imports
import Draw.Cube (draw,cha_draw)
import Draw.Samples(draw_init)
import Draw.MouseIntegration

--Foreign Imports

foreign import tim :: Context2D -> forall a.(Context2D -> Eff (canvas :: CANVAS | a) Unit) -> Unit
foreign import addEvents :: forall a. Context2D -> String -> (Event -> Eff(canvas :: CANVAS | a)  Unit) -> Eff(canvas :: CANVAS |a ) Unit


main :: forall e. Eff (canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "one"
  ctx <- getContext2D canvas
  addEvents ctx "mouseup" onMouseRelease
  addEvents ctx "mousedown" onMouseclick
  addEvents ctx "mousemove" onMouseMove

--if wanted to see an sample cube just un comment the line 21 and comment line 22

  pure $ tim ctx cha_draw
  pure unit
