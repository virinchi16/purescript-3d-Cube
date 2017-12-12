module Draw.MouseIntegration where


import DrawExtras
import Graphics.Canvas
import Math (max)
import Prelude

import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(Just))
import Partial.Unsafe (unsafePartial)
import FFI.Util

foreign import data Event :: Type

newtype Vector = Vector{x:: Number, y::Number}
isDragged :: Array Boolean
isDragged = [false]

previous :: Vector
previous = Vector{x: 0.0, y:0.0}

deltaMove ::Vertex
deltaMove =Vertex{x: 0.0, y:0.0, z:0.0}

isDragging :: Array Boolean
isDragging = [false]

acceleration :: Array Number
acceleration =[0.0]

rotation :: Vector
rotation = Vector{x: 0.0, y:0.0}

onMouseclick :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseclick evt = void $ do
    _ <- pure $ setProperty isDragging "0" true
    pure unit
onMouseRelease :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseRelease evt = void $ do
    _<- pure $ setProperty isDragging "0" false
    pure unit

onMouseMove :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseMove evt = void $ do
    let (offSetX :: Number) = property evt "offsetX"
    let (offSetY :: Number) = property evt "offsetY"
    _ <- if(property isDragging "0" ) then do
        let (prevMouseX :: Number) = property previous "x"
        let (prevMouseY :: Number) = property previous "y"
        _ <- pure $ setProperty deltaMove "x" (offSetX - prevMouseX)
        _ <- pure $ setProperty deltaMove "y" (offSetY - prevMouseY)
        let accel = (property acceleration "0") + 2.0
        pure $ setProperty acceleration "0" accel
        pure unit
        else do
                pure unit
    _ <- pure $ setProperty previous "x" offSetX
    _ <- pure $ setProperty previous "y" offSetY
    pure unit
