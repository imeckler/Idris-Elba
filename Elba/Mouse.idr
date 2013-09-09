module Elba.Mouse

import Elba.Signal
import Js.Unsafe

%access public

private
elmMousePos : IO (Signal Ptr)
elmMousePos = map MkSignal (eval "Elba.Mouse.position")

-- consider fixing so that it uses Idris's representation of unit
-- rather than Elm's although honestly I doubt it matters.
clicks : IO (Signal ())
clicks = map MkSignal (eval "Elba.Mouse.clicks")

fromElmTup : Ptr -> IO (Int, Int)
fromElmTup ptr = do
  x <- mkForeign (FFun "._0" [FPtr] FInt) ptr
  y <- mkForeign (FFun "._1" [FPtr] FInt) ptr
  return (x, y)

fromElmTup' : Ptr -> (Int, Int)
fromElmTup' ptr = unsafePerformIO (fromElmTup ptr)

mousePos : IO (Signal (Int, Int))
mousePos = map (map fromElmTup') elmMousePos

