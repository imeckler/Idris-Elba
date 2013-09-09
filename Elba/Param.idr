module Elba.Param

import Elba.IO
import Elba.IORef
import Js.Unsafe
import Js.MaybeDef
import Js.Common

%access abstract

Signal : Type -> Type
Signal = JsIO

private
Weak : Type -> Type
Weak a = IORef (MaybeDef a)

deRefWeak : Weak a -> JsIO (Maybe a)
deRefWeak ptr = map toMaybe (readIORef ptr)

private
UpdatePool : Type
UpdatePool = List (Weak (JsIO (), JsIO ()))

data SignalGen p a = SG (IORef UpdatePool -> Signal p -> JsIO a)

private
data Phase a = Ready a | Updated a a

undefined : {a : Type} -> a
undefined = evalUnsafer "undefined"

unSG : SignalGen p a -> IORef UpdatePool -> Signal p -> JsIO a
unSG (SG f) = f

instance Functor (SignalGen p) where
  map f (SG g) = SG (\up, s => map f (g up s))

instance Applicative (SignalGen p) where
  pure x = SG (\_, _ => pure x)
  (SG f) <$> (SG x) = SG (\up, s => f up s <$> x up s)

instance Monad (SignalGen p) where
  (SG g) >>= f = SG (\up, s => g up s >>= \x => unSG (f x) up s)

-- TODO: Do mfix

External : Type -> Type
External a = JsIO (Signal a, a -> JsIO ())

external : a -> JsIO (Signal a, a -> JsIO ())
external x = map (\ref => (readIORef ref, writeIORef ref)) (newIORef x)

mapIO : (a -> b) -> JsIO a -> JsIO b
mapIO = map

superstep : IORef UpdatePool -> JsIO ()
superstep pool = loop id [] where
  -- To convince the type checker
  deref' : IORef (MaybeDef (JsIO (), JsIO ())) -> JsIO (Maybe (IORef (MaybeDef (JsIO (), JsIO ())), (JsIO (), JsIO ())))
  deref' ptr = mapIO (map (\x => (ptr, x))) (deRefWeak ptr)

  loop : (UpdatePool -> UpdatePool) -> List (JsIO ()) -> JsIO ()
  loop getPtrs final = do
    (ptrs, acts) <- mapIO (List.unzip . catMaybes) (readIORef pool >>= traverse {t = List} deref')
    case acts of
         [] => do
           sequence_ final
           writeIORef pool (getPtrs [])
         _  => do
           writeIORef pool []
           traverse_ fst acts
           -- Can't wait to see how horribly slow this is
           loop ((ptrs ++) . getPtrs) (traverse_ snd acts :: final)

-- Delete me
getGlobal : String -> a
getGlobal {a} s = unsafePerformIO (mkForeign (FFun "getGlobal" [FString] (FAny a)) s)

-- gen : IORef UpdatePool -> Signal p -> JsIO (Signal a)
start : SignalGen p (Signal a) -> JsIO (p -> JsIO a)
start {p} (SG gen) = do
  pool        <- the (JsIO (IORef UpdatePool)) (newIORef List.Nil)
  (inp, sink) <- the (External p) (external undefined)
  sample      <- gen pool inp -- (getGlobal "poolz") inp  -- pool inp
  return $ \param => do
    sink param
    res <- sample
    superstep pool
    return res

addSignal :  (a -> JsIO a)
          -> (a -> JsIO ())
          -> IORef (Phase a)
          -> IORef UpdatePool
          -> JsIO (Signal a)
addSignal {a} sample update ref pool = act where
  updateIfReady : Phase a -> JsIO ()
  updateIfReady (Ready x) = update x
  updateIfReady _         = return ()

  collectUpdate : Phase a -> JsIO ()
  collectUpdate (Updated x _) = writeIORef ref (Ready x)
  collectUpdate _             = error "Signal not updated!"

  mkSignal : Phase a -> Signal a
  mkSignal (Ready x)     = sample x
  mkSignal (Updated _ x) = return x

  act = do
    let upd = readIORef ref >>= updateIfReady
    let fin = readIORef ref >>= collectUpdate
    let sig = readIORef ref >>= mkSignal

    -- lets see if the absence of weak pointers screws us
    updateActions <- newIORef (Js.MaybeDef.pure (upd, fin))
    modifyIORef pool (updateActions ::)
    return sig


-- delay : a -> Signal a -> SignalGen p (Signal a)
{--
delay {a} {p} x0 s = sg where
  update : IORef (Phase a) -> a -> JsIO ()
  update ref x = s >>= \x' => writeIORef ref (Updated x' x)

  sg : SignalGen p (Signal a)
  sg = SG $ \pool => do
    ref <- newIORef (Ready x0)
    addSignal return (update ref) ref pool
    --}

tt : ()
tt = ()

snapshot : Signal a -> SignalGen p a
snapshot s = SG (\_, _ => s)

memoise : IORef (Phase a) -> a -> JsIO a
memoise ref x = writeIORef ref (Updated undefined x) $> pure x

generator : Signal (SignalGen p a) -> SignalGen p (Signal a)
generator s = SG gen where
  gen pool inp = do
    ref <- newIORef (Ready undefined)
    let x      = s >>= (\(SG g) => g pool inp)
    let sample = x >>= memoise ref
    addSignal (const sample) (const (map (const tt) sample)) ref pool

memo : Signal a -> SignalGen p (Signal a)
memo _ = error "Unimplemented"

until : Signal Bool -> SignalGen p (Signal Bool)
until = error "Requires mfix to be implemented first"

input : SignalGen p (Signal p)
input = SG (const return)

embed : Signal p' -> SignalGen p' a -> SignalGen p a
embed s (SG g) = SG (\pool, _ => g pool s)

execute : JsIO a -> SignalGen p a
execute act = SG (\_, _ => act)

effectful : JsIO a -> SignalGen p (Signal a)
effectful {a} {p} act = SG $ \pool, _ => do
  ref <- newIORef (Ready undefined)
  let sample = act >>= memoise ref
  addSignal (const sample) (const (map (const tt) sample)) ref pool

{--
unitl s = SG gen where
  gen pool _ = do
    ref  <- newIORef (Ready undefined)
    rsmp <- 
    --}
