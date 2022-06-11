module Main where

import Control.Monad (join, when)
import Data.List (unwords)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Monoid ()
import Graphics.X11.ExtraTypes.XF86 as XF86
import System.Exit ()
import XMonad
import XMonad.Config.Dmwit (altMask)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (Direction2D (D, L, R, U), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenFull, fullscreenManageHook, fullscreenSupport)
import XMonad.Layout.Gaps (Direction2D (D, L, R, U), GapMessage (ToggleGaps), gaps)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce (spawnOnce)

myTerminal :: String
myTerminal = "st"

myFollowMouse :: Bool
myFollowMouse = True

myModMask :: KeyMask
myModMask = mod4Mask

myWorkSpaces :: [String]
myWorkSpaces = ["\xf269", "\xf120", "\xf07b", "\xf198", "\xf02d", "\xf03e", "\xf001"]

addNetSupported :: Atom -> X ()
addNetSupported x = withDisplay $ \dpy -> do
    r <- asks theRoot
    supported <- getAtom "_NET_SUPPORTED"
    atom <- getAtom "ATOM"
    liftIO $ do
        sup <- join . maybeToList <$> getWindowProperty32 dpy supported r
        when (fromIntegral x `notElem` sup) $ changeProperty32 dpy r supported atom propModeAppend [fromIntegral x]

addEwmhFullScreen :: X ()
addEwmhFullScreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNetSupported [wms, wfs]

maimcopy :: MonadIO m => m ()
maimcopy = spawn "maim -u -b 3 -m 5 | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"

maimsave :: MonadIO m => m ()
maimsave = spawn "maim -u -b 3 -m 5 ~/screenshots/$(date +%Y.%m.%d-%H.%M.%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"

launcher :: MonadIO m => m ()
launcher = spawn "rofi -show combi"

spawnAndUpdate :: MonadIO m => String -> String -> String -> String -> m ()
spawnAndUpdate path var val cmd = do
    spawn $ unwords ["eww -c", path, "update", var, val]
    spawn cmd

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig{XMonad.modMask = modm} =
    M.fromList $
        [ ((modm, xK_Return), spawn myTerminal)
        , ((modm, xK_d), launcher)
        , -- mpd control
          ((modm, xK_space), spawn "mpc -q toggle")
        , ((modm .|. shiftMask, xK_comma), spawn "mpc -q prev")
        , ((modm .|. shiftMask, xK_period), spawn "mpc -q next")
        , -- volume control
          ((modm .|. altMask, xK_F1), spawn "amixer -D default Master toggle")
        , ((modm .|. altMask, xK_F2), spawn "amixer -D default Master 5%-")
        , ((modm .|. altMask, xK_F3), spawn "amixer -D default Master 5%+")
        , -- brightness control
          ((modm .|. altMask, xK_F4), spawn "light -U 10")
        , ((modm .|. altMask, xK_F5), spawn "light -A 10")
        , -- Screenshot
          ((0, xK_Print), maimsave)
        , ((modm, xK_Print), maimcopy)
        , -- Toggle screen gaps
          ((modm, xK_g), sendMessage ToggleGaps)
        , -- bar stuff
          -- kill focused window
          ((modm .|. shiftMask, xK_q), kill)
        , -- Layout management
          ((modm, xK_grave), sendMessage NextLayout)
        , ((modm .|. shiftMask, xK_grave), setLayout $ XMonad.layoutHook conf)
        , ((modm .|. shiftMask, xK_r), refresh)
        , -- Client focus
          ((modm, xK_Tab), windows W.focusDown)
        , ((modm .|. shiftMask, xK_Tab), windows W.focusUp)
        , ((modm, xK_j), windows W.focusDown)
        , ((modm, xK_k), windows W.focusUp)
        , ((modm, xK_m), windows W.focusMaster)
        , -- Client swap
          ((modm .|. shiftMask, xK_j), windows W.swapDown)
        , ((modm .|. shiftMask, xK_k), windows W.swapUp)
        , ((modm .|. shiftMask, xK_m), windows W.swapMaster)
        , -- Master shrink/expand
          ((modm, xK_h), sendMessage Shrink)
        , ((modm, xK_l), sendMessage Expand)
        , ((modm, xK_t), withFocused $ windows . W.sink)
        , ((modm .|. shiftMask, xK_r), spawn "my-xmonad --restart") -- TODO: add rebuild method
        ]
            <> [ ((modm .|. m, k), windows $ f i)
               | (i, k) <- zip myWorkSpaces [xK_1 .. xK_7]
               , (f, m) <- zip [W.greedyView, W.shift] [0, shiftMask]
               ]

myMouseBindings :: XConfig l -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig{XMonad.modMask = modm} =
    M.fromList
        -- mod-button1, Set the window to floating mode and move by dragging
        [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
        , -- mod-button2, Raise the window to the top of the stack
          ((modm, button2), \w -> focus w >> windows W.shiftMaster)
        , -- mod-button3, Set the window to floating mode and resize by dragging
          ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
        ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
    nmaster = 1 -- The default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myManageHook =
    fullscreenManageHook <+> manageDocks
        <+> composeAll
            [ className =? "mpv" --> doFloat
            , className =? "Pavucontrol" --> doFloat
            , resource =? "desktop_window" --> doIgnore
            , resource =? "kdesktop" --> doIgnore
            , isFullscreen --> doFullFloat
            ]

myStartupHook = do
    spawn "sh ~/.xprofile"

defaults =
    def
        { -- simple stuff
          terminal = myTerminal
        , focusFollowsMouse = True
        , clickJustFocuses = False
        , borderWidth = 0
        , modMask = myModMask
        , workspaces = myWorkSpaces
        , -- key bindings
          keys = myKeys
        , mouseBindings = myMouseBindings
        , -- hooks, layouts
          manageHook = myManageHook
        , layoutHook = gaps [(L, 30), (R, 30), (U, 40), (D, 30)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders myLayout
        , handleEventHook = mempty
        , logHook = return ()
        , startupHook = myStartupHook >> addEwmhFullScreen
        }

main :: IO ()
main = xmonad . fullscreenSupport . docks . ewmhFullscreen . ewmh $ defaults
