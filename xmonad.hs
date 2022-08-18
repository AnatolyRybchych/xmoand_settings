import XMonad
import Data.Monoid
import System.Exit
import Text.Printf
import XMonad.Layout.Fullscreen
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Monitor
import System.Process

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_s     ), spawn "import ~/screenshot.png")
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = fullscreenFull tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook = composeOne [
  isKDETrayWindow -?> doIgnore,
  transience,
  isFullscreen -?> doFullFloat,
  resource =? "stalonetray" -?> doIgnore]

myEventHook = mempty

myLogHook = return ()

myStartupHook = do
  --disable eDP monitor if HDMI monitor connected and set refresh rate 120 hz
  spawn "xrandr | grep \" connected \" | awk '{ print$1 }' | ( grep HDMI-1-1 && xrandr --output eDP-1-1 --off ; xrandr --output HDMI-1-1 --mode 1920x1080 --rate 120)"
  --switching languages in super+space
  spawn "setxkbmap -option grp:win_space_toggle us,ua"


main = xmonad def {
-- simple stuff
  terminal           = "gnome-terminal",
  focusFollowsMouse  = True,
  clickJustFocuses   = False,
  borderWidth        = 2,
  modMask            = mod1Mask,
  workspaces         = ["1","2","3","4","5","6","7","8","9"],
  normalBorderColor  = "#000000",
  focusedBorderColor = "#0099ff",
-- key bindings
  keys               = myKeys,
  mouseBindings      = myMouseBindings,
-- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  logHook            = myLogHook,
  startupHook        = myStartupHook
}

help :: String
help = unlines [""]
