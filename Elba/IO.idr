module Elba.IO

%access abstract

data JsIO a = Thunk (() -> IO a) | Pure a
{--
data JsIO a = Yo (IO a) | Thunk (() -> IO a)

instance Functor JsIO where
  map f (Yo x)    = Yo (map f x)
  map f (Thunk t) = Thunk (map f . t)

instance Applicative JsIO where
  pure x = Yo (return x)
  (Yo f)     <$> (Yo x)    = Yo    (f <$> x)
  (Yo f)     <$> (Thunk t) = Thunk (\() => f <$> (t ()))
  (Thunk ft) <$> (Yo x)    = Thunk (
    --}

instance Functor JsIO where
  map f (Thunk t) = Thunk (\() => map f (t ()))
  map f (Pure a)  = Pure (f a)

instance Applicative JsIO where
  (Thunk ft) <$> (Thunk xt) = Thunk (\() => ft () <$> xt ())
  (Thunk ft) <$> (Pure x)   = Thunk (\() => ft () <$> pure x)
  (Pure f)   <$> (Pure x)   = Pure (f x)
  (Pure f)   <$> (Thunk xt) = Thunk (\() => map f (xt ()))
  pure = Pure

unsafeToIO : JsIO a -> IO a
unsafeToIO (Thunk xt) = xt ()
unsafeToIO (Pure x)   = pure x


-- The thunk case seems a bit inefficient
instance Monad JsIO where
  (Pure x)   >>= f = f x
  (Thunk xt) >>= f = Thunk (\() => map f (xt ()) >>= unsafeToIO)

printAny : a -> JsIO ()
printAny {a} x = Thunk (\() => mkForeign (FFun "console.log" [FAny a] FUnit) x)

random : JsIO Float
random = Thunk (\() => mkForeign (FFun "Math.random" [FUnit] FFloat) ())

setGlobal : String -> a -> JsIO ()
setGlobal {a} name x = Thunk (\() => mkForeign (FFun "setGlobal" [FString, FAny a] FUnit) name x)

syntax "thunk" [x] = (\() => x)

