module Draw.Cube where

import Draw.MouseIntegration
import DrawExtras
import Graphics.Canvas
import Math
import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (product)
import Data.Maybe (Maybe(Just))
import FFI.Util (property, setProperty)
import Partial.Unsafe (unsafePartial)


draw :: forall a. Context2D->Vertex-> Eff (canvas :: CANVAS | a) Unit
draw ctx ang= void $ do
  let poi = 0.5
  _ <-setStrokeStyle "#FF0000" ctx
--  let a = Vertex{x: ang, y: ang, z:ang
--Vertex is 3d, ie. it has x y z, x represents x axis, y represents y axis and z was not important but for calculating the
--point position in an 3d rotation we need to make it some value
--here poi is an temporary value its just an random try to bring the cube into the screen as negative points are not visible on
--on the screen
--cent just divides the return value with assumtions and gives the screen values
--for usage refer to Draw.cube line 16
  let vertex1 = cent $ updatepointwithangle (Vertex { x:  -poi, y:  -poi, z:  -poi }) ang
  let vertex2 = cent $ updatepointwithangle (Vertex { x:  poi, y:  -poi, z:  -poi }) ang
  let vertex3 = cent $ updatepointwithangle (Vertex { x:  poi, y:  poi, z:  -poi }) ang
  let vertex4 = cent $ updatepointwithangle (Vertex { x:  -poi, y:  poi, z:  -poi })ang
  let vertex5 = cent $ updatepointwithangle (Vertex { x:  -poi, y:  -poi, z:  poi }) ang
  let vertex6 = cent $ updatepointwithangle (Vertex { x:  poi, y:  -poi, z:  poi }) ang
  let vertex7 = cent $ updatepointwithangle (Vertex { x:  poi, y:  poi, z:  poi }) ang
  let vertex8 = cent $ updatepointwithangle (Vertex { x:  -poi, y:  poi, z:  poi }) ang
-- makealine is a method that takes Context2D element ie, ctx(Variable) and 2 Vertex and draws an line from vertex1
--to Vertex2 for usage refer to Draw.DrawExtras line 26
  makealine ctx vertex1 vertex2
  makealine ctx vertex2 vertex3
  makealine ctx vertex3 vertex4
  makealine ctx vertex4 vertex1
  makealine ctx vertex5 vertex6
  makealine ctx vertex6 vertex7
  makealine ctx vertex7 vertex8
  makealine ctx vertex8 vertex5
  makealine ctx vertex1 vertex5
  makealine ctx vertex2 vertex6
  makealine ctx vertex3 vertex7
  makealine ctx vertex4 vertex8

cha_draw :: forall a. Context2D-> Eff (canvas :: CANVAS | a) Unit
cha_draw ctx= void $ do
  let (a :: Number) = property acceleration "0"
  _ <- if(a > 0.0) then do
    _ <- pure $ setProperty acceleration "0" (property acceleration "0" - 0.5)
    let (dx :: Number) = property deltaMove "x"
    let (dy :: Number) = property deltaMove "y"
    pure $ setProperty rotation "x" (property rotation "x" - dy)
    pure $ setProperty rotation "y" (property rotation "y" - dx)
    pure unit

    else do
      pure unit
  --let ang =Vertex {x: property rotation "x", y: property rotation "y",z: 0.0}
  let ang =Vertex {x: (((property rotation "x") * pi)/180.0), y: (((property rotation "y") * pi)/180.0),z: 0.0}
  _ <-setFillStyle "#FFF000" ctx
  _ <-fillRect ctx { x:(0.0), y:(0.0), w:1000.0, h:1000.0 }
  _ <- draw ctx ang
  pure unit
