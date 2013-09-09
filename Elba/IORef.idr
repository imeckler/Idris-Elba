module Elba.IORef

import Elba.IO

-- iiiidata IORef UnitToPleaseIdris


IORef : Type -> Type
IORef _ = Ptr

newIORef : a -> JsIO (IORef a)
newIORef {a} x = Thunk (\() => mkForeign (FFun "Elba.newIORef" [FAny a] (FAny (IORef a))) x)

readIORef : IORef a -> JsIO a
readIORef {a} ref = Thunk (\() => mkForeign (FFun "Elba.readIORef" [FAny (IORef a)] (FAny a)) ref)

writeIORef : IORef a -> a -> JsIO ()
writeIORef {a} ref x =
  Thunk (\() => mkForeign (FFun "Elba.writeIORef" [FAny (IORef a), FAny a] FUnit) ref x)

-- Consider implementing in js using the fun call prims to avoid the extra thunk from reading
modifyIORef : IORef a -> (a -> a) -> JsIO ()
modifyIORef {a} ref f = readIORef ref >>= writeIORef ref . f

