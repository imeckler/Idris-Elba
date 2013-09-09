module Elba.Input

import Elba.IO
import Elba.Param
import Js.Bool

data Element = MkElem Ptr

data Key = C Char
         | Shift
         | Ctrl
         | Cmd
         | Return
         | Tab
         | Delete
         | Escape

fromKeyCode : Int -> Key

toKeyCode : Key -> Int

body : JsIO Element
body = Thunk (\() => map MkElem (eval "document.body"))

down : Int -> Signal Bool
down x = map toBool $ mkForeign (FFun "Elba.Input.isDown" [FInt] (FAny JsBool)) x

pressedSet : Signal (List Key)

clicks : Signal (Int, Int)

mousePos : Signal (Int, Int)
