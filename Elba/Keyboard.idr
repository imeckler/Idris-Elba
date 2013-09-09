module Elba.Keyboard

import Elba.Signal
import Js.Bool

Keycode : Type
Keycode = Int

-- TODO: The horrible work of implmenting the conversion between this and Int
{-- 
data Key = BackspaceKey | TabKey | ClearKey | EnterKey | PauseKey | EscapeKey |
           SpaceKey | ExclaimKey | QuotedBlKey | HashKey | DollarKey | AmpersandKey |
           QuoteKey | LeftParenKey | RightParenKey | AsteriskKey | PlusKey | CommaKey |
           MinusKey | PeriodKey | SlashKey | Num0Key | Num1Key | Num2Key |
           Num3Key | Num4Key | Num5Key | Num6Key | Num7Key | Num8Key |
           Num9Key | ColonKey | SemicolonKey | LessKey | EqualsKey | GreaterKey |
           QuestionKey | AtKey | LeftBracketKey | BackslashKey | RightBracketKey | CaretKey |
           UnderscoreKey | BackquoteKey | AKey | BKey | CKey | DKey |
           EKey | FKey | GKey | HKey | IKey | JKey | KKey |
           LKey | MKey | NKey | OKey | PKey | QKey |
           RKey | SKey | TKey | UKey | VKey | WKey |
           XKey | YKey | ZKey | DeleteKey | KeypadNum0Key | KeypadNum1Key |
           KeypadNum2Key | KeypadNum3Key | KeypadNum4Key | KeypadNum5Key | KeypadNum6Key | KeypadNum7Key |
           KeypadNum8Key | KeypadNum9Key | KeypadPeriodKey | KeypadDivideKey | KeypadMultiplyKey | KeypadMinusKey |
           KeypadPlusKey | KeypadEnterKey | KeypadEqualsKey | UpKey | DownKey | RightKey |
           LeftKey | InsertKey | HomeKey | EndKey | PageUpKey | PageDownKey |
           F1Key | F2Key | F3Key | F4Key |  F5Key | F6Key |
           F7Key | F8Key | F9Key | F10Key | F11Key | F12Key |
           F13Key | F14Key | F15Key | NumLockKey | CapsLockKey | ScrollLockKey |
           RShiftKey | LShiftKey | RCtrlKey | LCtrlKey | RAltKey | LAltKey |
           RMetaKey | LMetaKey | RSuperKey | LSuperKey | ModeKey | ComposeKey | HelpKey |
           PrintKey | SysReqKey | BreakKey | MenuKey | PowerKey | EuroKey |
           UndoKey
           --}

-- Obviously more unsafe than usual
arrToList : Ptr -> List a
arrToList {a} arr = loop 0 where
  get : Int -> a
  get i = unsafePerformIO (mkForeign (FFun "[]" [FPtr, FInt] (FAny a)) arr i)
  len : Int
  len = unsafePerformIO (mkForeign (FFun ".length" [FPtr] FInt) arr)
  loop : Int -> List a
  loop i = if i == len then [] else get i :: loop (i + 1)


keysDown : Signal (List Keycode)
keysDown = unsafePerformIO (
  map (map arrToList . MkSignal) (eval "Elba.Keyboard.keysDown"))

lastPressed : Signal Keycode
lastPressed = unsafePerformIO (
  map MkSignal (eval "Elba.Keyboard.lastPressed"))

fromJsTup' : Ptr -> IO (Int, Int)
fromJsTup' arr = do
  x <- mkForeign (FFun "[]" [FPtr, FInt] FInt) arr 0
  y <- mkForeign (FFun "[]" [FPtr, FInt] FInt) arr 1
  return (x, y)

fromJsTup : Ptr -> (Int, Int)
fromJsTup ptr = unsafePerformIO (fromJsTup' ptr)

directions : Keycode -> Keycode -> Keycode -> Keycode -> Signal (Int, Int)
directions k1 k2 k3 k4 = unsafePerformIO (
  map (map fromJsTup . MkSignal)
    (mkForeign (FFun "Elba.Keyboard.directions" [FInt, FInt, FInt, FInt] FPtr) k1 k2 k3 k4))

isDown : Int -> Signal Bool
isDown k = unsafePerformIO (
  map MkSignal (mkForeign (FFun "Elba.Keyboard.isDown" [FInt] FPtr) k))

arrows : Signal (Int, Int)
arrows = directions 38 40 37 39

wasd : Signal (Int, Int)
wasd = directions 87 83 65 68

shift : Signal Bool
shift = isDown 16

enter : Signal Bool
enter = isDown 13

space : Signal Bool
space = isDown 32

ctrl : Signal Bool
ctrl = isDown 17

