module Elba.Array

import Js.MaybeDef
import Elba.IO

Array : Type -> Type
Array _ = Ptr

push : a -> Array a -> JsIO ()
push {a} x arr = Thunk (\() => mkForeign (FFun ".push" [FAny (Array a), FAny a] FUnit) arr x)

newArray : JsIO (Array a)
newArray {a} = Thunk (\() => mkForeign (FFun "newArray" [FUnit] (FAny (Array a))) ())

mapIO_ : (a -> JsIO b) -> Array a -> JsIO ()
mapIO_ {a} {b} f arr =
  Thunk (\() => mkForeign (FFun "Elba.iter" [FFunction (FAny a) (FAny (IO b)), FPtr] FUnit) (unsafeToIO . f) arr)

map : (a -> b) -> Array a -> JsIO (Array b)
map {a} {b} f arr =
  Thunk (\() => mkForeign (FFun ".map" [FPtr, (FFunction (FAny a) (FAny b))] FPtr) arr f)

