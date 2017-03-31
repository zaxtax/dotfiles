import qualified Data.Map as M
import           XMonad
import qualified XMonad.Layout.Fullscreen as F
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Prompt.Shell

mykeys (XConfig {modMask = modm}) = M.fromList $
   [ ((modm, xK_x), shellPrompt def)
   ]

main = do
  xmonad $ ewmh $ def
   { modMask            = mod4Mask
   , terminal           = "xterm -fn 10x20"
   , keys               = (\c -> mykeys c `M.union` keys def c) 
   , focusedBorderColor = "blue" -- "#58B1DE"
   , handleEventHook    = ewmhDesktopsEventHook <+> F.fullscreenEventHook <+> fullscreenEventHook
   }
