module Elba.Signal

import Js.Bool

%access public

public
data Signal a = MkSignal Ptr

-- consider removing the unsafePerformIOs and making Signal a ~ IO Ptr

signalMap : (a -> b) -> Signal a -> Signal b
signalMap {a} {b} f (MkSignal s) = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Signal.map" [FAny (a -> b), FPtr] FPtr) f s))

instance Functor Signal where
  map = signalMap

private
unwrap : Signal a -> Ptr
unwrap (MkSignal s) = s

ap : Signal (a -> b) -> Signal a -> Signal b
ap (MkSignal sf) (MkSignal sx) = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Signal.ap" [FPtr, FPtr] FPtr) sf sx))

private
constant : a -> Signal a
constant {a} x = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Signal.constant" [FAny a] FPtr) x))

instance Applicative Signal where
  (<$>) = ap
  pure = constant

-- Make this more efficent (i.e., with less wrapping and unwrapping en el futuro
join : Signal (Signal a) -> Signal a
join sig = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Signal.join" [FPtr] FPtr) (unwrap (map unwrap sig))))

bind : Signal a -> (a -> Signal b) -> Signal b
bind {a} (MkSignal s) f = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Signal.bind" [FAny (a -> Ptr), FPtr] FPtr) (unwrap . f) s))

-- Consider implementing this directly
instance Monad Signal where
  (>>=) = bind
  -- (>>=) x f = join (map f x)

filter : (a -> Bool) -> a -> Signal a -> Signal a
filter {a} f init (MkSignal s) = unsafePerformIO (
  map MkSignal (
    mkForeign (
      FFun "Elba.Signal.filter" [FAny (a -> JsBool), FAny a, FPtr] FPtr) (fromBool . f) init s))

whenIO : Ptr -> a -> Ptr -> IO Ptr
whenIO {a} boolSig x s = do
  jsBoolSig <- mkForeign (FFun "Elba.Signal.map" [FAny (Bool -> JsBool), FPtr] FPtr) fromBool boolSig
  mkForeign (FFun "Elba.Signal.keepWhen" [FPtr, FAny a, FPtr] FPtr) jsBoolSig x s

when : Signal Bool -> a -> Signal a -> Signal a
when (MkSignal boolSig) x (MkSignal s) = unsafePerformIO (map MkSignal (whenIO boolSig x s))

scan : (b -> a -> b) -> b -> Signal a -> Signal b
scan {a} {b} f z (MkSignal s) = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Signal.scan" [FAny (b -> a -> b), FAny b, FPtr] FPtr) f z s))

abstract
Subscription : Type
Subscription = Ptr

stop : Subscription -> IO ()
stop sub = mkForeign (FFun ".unsubscribe" [FPtr, FUnit] FUnit) sub ()

sink : (a -> IO b) -> Signal a -> IO ()
sink {a} {b} f (MkSignal s) =
  mkForeign (FFun "Elba.Signal.sink" [FAny (a -> IO b), FPtr] FUnit) f s

