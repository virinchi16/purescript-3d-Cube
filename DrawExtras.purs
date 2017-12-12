module DrawExtras where

import Prelude
import Control.Monad.Eff(Eff)
import Graphics.Canvas
import Data.Maybe(Maybe(Just))
import Partial.Unsafe(unsafePartial)
import Math

newtype Vertex = Vertex{
  x :: Number,
  y :: Number,
  z :: Number
}

cent :: Vertex -> Vertex
cent (Vertex v) = Vertex {
            x: v.x * h + w,
            y: v.y * h + w,
            z: v.z
        }
    where
        w = 250.0
        h = 250.0

makealine :: forall a.Context2D -> Vertex -> Vertex -> Eff (canvas :: CANVAS | a) Unit
makealine ctx (Vertex point1) (Vertex point2) =void $ unsafePartial do
  _ <- beginPath ctx
  _ <- setFillStyle "#00FF00" ctx
  _ <- moveTo ctx point1.x point1.y
  _ <- lineTo ctx point2.x point2.y
  _ <- stroke ctx
  _ <-closePath ctx

  --_ <- fill ctx Context2D
  -- _ <- setFillStyle "#FFFFFF" ctx
  -- _ <- fillRect ctx { x:point1.x, y:point2.y, w:250.0, h:250.0 }
--  _ <- fillPath ctx { x:point1.x, y:point2.y, w:250.0, h:250.0 }
  pure unit
updatepointwithangle :: Vertex -> Vertex -> Vertex
updatepointwithangle (Vertex p) (Vertex a) =
  let
    --X Rotation
    sinA = sin(a.x)
    cosA = cos(a.x)
    rty = p.y * cosA - p.z * sinA
    rtz = p.y * sinA +p.z * cosA
    --Vector3  { x: vec.x, y: rty, z:rtz }
    --Y Rotation
    ysinA = sin(a.y)
    ycosA = cos(a.y)
    yrtx = p.x * ycosA + rtz * ysinA
    yrtz = -p.x * ysinA + rtz * ycosA
  in
    Vertex  { x: yrtx, y: rty, z:yrtz}
