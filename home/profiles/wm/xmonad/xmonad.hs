import XMonad
import XMonad.Config
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.DynamicWorkspaces

import Actions
import Polybar

main :: IO ()
main = do
  dbus <- mkDbusClient
  xmonad $ ewmh $ def
    { terminal = "alacritty"
    , modMask = mod4Mask
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , handleEventHook = myHandleEventHook
    , workspaces = myWorkspaces
    , logHook = myPolybarLogHook dbus
    }
    `additionalKeys` myKeybindings

myWorkspaces =
  ["1-emacs", "2-web"] ++
  [show i | i <- [3..8]] ++
  ["9-github"]

myLayoutHook =
  avoidStruts (layoutHook defaultConfig)

myManageHook =
  manageDocks
  <+> composeAll
    [ resource =? ".arandr-wrapped" --> doFloat
    , resource =? ".blueman-manager-wrapped" --> doFloat
    , resource =? "pavucontrol" --> doFloat
    , resource =? "com.rafaelmardojai.Blanket" --> doFloat
    , className =? "mpv" --> doFloat
    , title =? "Inkscape 1.1" --> doFloat
    ]

myHandleEventHook =
  docksEventHook

myKeybindings =
  [ ((mod4Mask, xK_p), rofi)
  ,  ((mod4Mask, xK_s), safeSpawnProg "rofi-systemd")
  , ((mod4Mask, xK_r), renameWorkspace myXPConfig)
  -- Restart without recompiling, since Nix builds xmonad
  , ((mod4Mask, xK_q), spawn "xmonad --restart")
  , ((mod4Mask, xK_F9), switchNixOSConfig)
  , ((mod4Mask, xK_BackSpace), swapScreens)
  , ((mod4Mask .|. shiftMask, xK_q), confirmQuit)
  , ((mod4Mask, xK_Insert), runWorkspaceAction)
  , ((mod4Mask, xK_equal), spawn "pamixer -i 3")
  , ((mod4Mask, xK_minus), spawn "pamixer -d 3")
  , ((mod4Mask, xK_Delete), spawn "pamixer -t")
  ]
  ++
  [ ((modm, key), f c)
  | (modm, f) <- [(mod4Mask, viewScreen)
                 ,(mod4Mask .|. shiftMask, sendToScreen)]
  , (key, c) <- [(xK_w, 0), (xK_e, 1)]
  ]
  ++
  [ ((mod4Mask, key), viewWorkspaceWithAction i)
  | (key, i) <- (zip [xK_1..xK_9] [1..9]) ++ [(xK_0, 0)]
  ]
  ++
  [ ((mod4Mask .|. shiftMask, key), shiftToWorkspaceByPrefix $ show i)
  | (key, i) <- (zip [xK_1..xK_9] [1..9]) ++ [(xK_0, 0)]
  ]

myXPConfig = def
