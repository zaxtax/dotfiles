import XMonad
import qualified Data.Map as M
import qualified XMonad.Layout.LayoutHints as LayoutHints
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutScreens
import XMonad.Layout.DragPane
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Layout.Tabbed hiding (fontName)
import qualified XMonad.Layout.Named
import Data.IORef
import qualified XMonad.StackSet as W

import System.IO.Unsafe (unsafePerformIO)
import Data.List (isPrefixOf)

import ViewDoc

layouts = LayoutHints.layoutHints $ 
          avoidStruts $
	  windowNavigation $
          ((Mirror tiled `named` "Horiz")
           ||| (tiled `named` "Vert")
           ||| spiral (6/7)
           ||| Grid
           ||| simpleTabbed
           ||| Full)
  where tiled = reflectHoriz $ Tall nmaster delta ratio
  	nmaster = 2
	ratio = 3/4
	delta = 3/100

named = flip XMonad.Layout.Named.named -- This way it can be used as an operator, and look normal

myLogHook :: X ()
myLogHook = do ewmhDesktopsLogHook
               dynamicLogString logPP >>= xmonadPropLog
               updatePointer (Relative 0.5 0.5)
               colorSaved

    where logPP = xmobarPP{ppCurrent=xmobarColor "green" ""
                          ,ppVisible=xmobarColor "#c0ffc0" ""
                          ,ppHidden=xmobarColor "CornflowerBlue" ""
                          ,ppHiddenNoWindows=xmobarColor "gray40" ""
                          ,ppUrgent=xmobarColor "red" "" . xmobarStrip
                          ,ppTitle=xmobarColor "green" "" . shorten 140
                          }

toggle :: IORef Bool -> X a -> X a -> X a
toggle ref ifTrue ifFalse =
    do val <- io $atomicModifyIORef ref (\x -> (not x,x))
       if val then ifTrue else ifFalse

screenMode :: IORef Bool
screenMode = unsafePerformIO $ newIORef True

mykeys (XConfig {modMask = modm}) = M.fromList $
   [ ((mod4Mask, xK_F2), spawn "gnome-screensaver-comand -l") -- %! Start screensaver
   , ((mod4Mask, xK_x), shellPrompt defaultXPConfig)
   , ((modm .|. controlMask, xK_space), toggle screenMode rescreen (layoutScreens 2 (Mirror $ Tall 0 0 (1/2))))
   , ((modm, xK_b), sendMessage ToggleStruts)
   , ((modm, xK_0), windows $ W.greedyView "0")
   , ((modm .|. shiftMask, xK_0), windows $ W.shift "0")
   -- session related bindings
   , ((modm, xK_s), toggleSaveState)
   , ((modm, xK_d), saveStateAs)
   , ((modm .|. shiftMask, xK_s), launchDocuments)
   --, ((modm, xK_f), (gridselectWindow defaultGSConfig) >>= (\w -> case w of
   --                      Just w -> windows (bringWindow w) >> focus w >> windows W.shiftMaster
   --                      Nothing -> return ()))
   , ((modm, xK_g), goToSelected defaultGSConfig)
   , ((modm,                 xK_Right), sendMessage $ Go R)
   , ((modm,                 xK_Left ), sendMessage $ Go L)
   , ((modm,                 xK_Up   ), sendMessage $ Go U)
   , ((modm,                 xK_Down ), sendMessage $ Go D)
   , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
   , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
   , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
   , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
   ]

main = xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
       { borderWidth        = 1
       , terminal           = "gnome-terminal"
       , modMask		 = mod4Mask
       , workspaces         = [ "IM", "F", "E", "D"] ++ map show [5,6,7,8,9,0]
       , keys		 = (\c -> mykeys c `M.union` keys defaultConfig c) 
       , layoutHook = layouts
       , logHook		 = myLogHook
       , focusedBorderColor = "green"
       , manageHook         = manageSpawn
       }
