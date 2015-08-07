import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Spacing
import System.IO

-- Define amount and names of workspaces
myWorkspaces = ["1:emacs","2:shells","3","4","5","6","7","8"]

myManageHook = composeAll
    [ className =? "emacs" --> doShift "1:emacs"
    , className =? "xfce4-terminal" --> doShift "2:shells"
    ]

myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = spacing 3 $ Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment by when resizing panes
    delta = 5/100

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/jonathan/.xmobarrc"
  xssproc <- spawnPipe "/usr/bin/xscreensaver -no-splash"
  trproc <- spawnPipe "/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 20"
  xmonad $ defaultConfig
       { modMask = mod4Mask
       , manageHook = manageDocks <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ myLayout
       , workspaces = myWorkspaces
       , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "blue" "" . shorten 50
                   , ppLayout = const "" -- to disable the layout info on xmobar
                   }
       }
