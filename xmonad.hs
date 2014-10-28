import XMonad

import XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
-- import XMonad.Actions.UpdatePointer

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Combo
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import XMonad.Util.Run

import System.IO
import System.Posix.Unistd
import System.Exit (exitSuccess)

import Control.Applicative ((<$>))
import Data.Ratio ((%))

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Monoid as MN
import qualified XMonad.StackSet as W

---------------------
--The actual config--
---------------------
main :: IO ()
main = do
    host            <- hostname
    xmPipe          <- spawnPipe $ myXmobar host
    -- dzenStatusBar   <- spawnPipe $ myStatusDzen host
    -- conkyStatusBar  <- spawnPipe $ myStatusConky host
    safeSpawn "urxvtd" []
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
        { modMask         = myModKey host
        , terminal        = "urxvtc"
        , workspaces      = myWorkspaces
        , keys            = myKeys host
        , mouseBindings   = myMouse host
        , layoutHook      = myLayoutHook
        , manageHook      = myManageHook
        , handleEventHook = docksEventHook
        , logHook         = dynamicLogWithPP $ xmLogHook xmPipe -- >> mousePosUpdate
        , startupHook     = myStartupHook
        }

myStartupHook :: X()
myStartupHook = do
  setWMName "LG3D"
  -- safeSpawnProg "/usr/bin/urxvtd" -- No longer used as urxvtd is started by systemd for my user session

myMouse :: Host -> XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse host _conf = M.fromList
    [ ((modKey              , button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modKey              , button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modKey              , button3), (\w -> focus w >> Sqr.mouseResizeWindow w False))
    , ((modKey .|. shiftMask, button3), (\w -> focus w >> Sqr.mouseResizeWindow w True ))
    ] where
      modKey = myModKey host

myKeys :: Host -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys host conf = M.fromList $ [
          --Mostly Defaults
            ((modKey   .|. shiftMask, xK_Return                ), spawn $ XMonad.terminal conf                   )  -- launch a terminal
          , ((modKey   .|. controlMask, xK_Return              ), safeSpawn "$HOME/cfg/bin/urxvtc-last" []       )
          , ((modKey   .|. shiftMask, xK_c                     ), kill                                           )  -- Close focused window
          , ((modKey                , xK_space                 ), sendMessage NextLayout                         )  -- Rotate through the available layout algorithms
          , ((modKey   .|. shiftMask, xK_space                 ), setLayout $ XMonad.layoutHook conf             )  -- Reset the layouts on the current workspace to default
          , ((modKey                , xK_r                     ), rescreen                                       )  -- Redraws the windows
          , ((modKey                , xK_p                     ), spawn "dmenu_run"                              )  -- Toggle dmenu, application launcher
          , ((modKey                , xK_y                     ), spawn "dmenu_custom"                           )  -- Customized dmenu with special commands
          --Hiding of stuff
          , ((modKey                , xK_b                     ), sendMessage ToggleStruts                       )  -- Hide Status Bars
          --Media-Keys
          -- Should really set these at a system level
          , ((0                     , 0x1008FF11               ), spawn "ponymix decrease 5"                     )  -- Volume Down
          , ((0                     , 0x1008FF13               ), spawn "ponymix increase 5"                     )  -- Volume Up
          , ((0                     , 0x1008FF12               ), spawn "ponymix toggle"                         )  -- Mute Volume
          , ((0                     , 0x1008FF02               ), spawn "xbacklight +10%"                        )  -- Brightness Up
          , ((0                     , 0x1008FF03               ), spawn "xbacklight -10%"                        )  -- Brightness Down
          --Window/Workspace Management
          , ((modKey   .|. controlMask, xK_Left                ), prevWS                                         )  -- Move focused window to workspace to the left
          , ((modKey   .|. controlMask, xK_Right               ), nextWS                                         )  -- Move focused window to workspace to the right
            --Plus the madness at the bottom of this function
          --Window Management
          , ((modKey                , xK_Return                ), dwmpromote                                     )  -- Swaps Master and current (top of slave stack)
          , ((modKey                , xK_Tab                   ), windows W.focusDown                            )  -- Move focus to the next window
          , ((modKey   .|. shiftMask, xK_Tab                   ), windows W.focusUp                              )  -- Move focus to the previous window
          , ((modKey                , xK_j                     ), windows W.focusDown                            )  -- Move focus to the next window
          , ((modKey                , xK_k                     ), windows W.focusUp                              )  -- Move focus to the previous window
          , ((modKey                , xK_h                     ), sendMessage Shrink                             )  -- Shrink the master area
          , ((modKey                , xK_l                     ), sendMessage Expand                             )  -- Expand the master area
          , ((modKey                , xK_m                     ), windows W.focusMaster                          )  -- Move focus to the master window
          , ((modKey                , xK_t                     ), withFocused $ windows . W.sink                 )  -- Push window back into tiling
          , ((modKey                , xK_comma                 ), sendMessage (IncMasterN 1)                     )  -- Increment the number of windows in the master area
          , ((modKey                , xK_period                ), sendMessage (IncMasterN (-1))                  )  -- Deincrement the number of windows in the master area
          , ((modKey                , xK_Right                 ), sendMessage $ Move R                           )  -- Moves current window right-
          , ((modKey                , xK_Left                  ), sendMessage $ Move L                           )  -- Moves current window left -
          , ((modKey                , xK_Up                    ), sendMessage $ Move U                           )  -- Moves current window up
          , ((modKey                , xK_Down                  ), sendMessage $ Move D                           )  -- Moves current window down
          , ((modKey   .|. shiftMask, xK_Right                 ), sendMessage $ Swap R                           )  -- Make these use the same keys as the above 4 when not in dual tabbed mode
          , ((modKey   .|. shiftMask, xK_Left                  ), sendMessage $ Swap L                           )
          , ((modKey   .|. shiftMask, xK_Up                    ), sendMessage $ Swap U                           )
          , ((modKey   .|. shiftMask, xK_Down                  ), sendMessage $ Swap D                           )
          , ((modKey                , xK_x                     ), sendMessage $ Toggle MIRROR                    )  -- Mirrors Layout
          , ((modKey                , xK_f                     ), sendMessage $ Toggle NBFULL                    )  -- Temp. Full Screen current window
          --Lock Computer
          , ((modKey   .|. shiftMask, xK_z                     ), spawn "xautolock -locknow"                    )  -- Locks screen 
          --Restarting/Closing XMonad
          , ((modKey   .|. shiftMask, xK_apostrophe            ), io exitSuccess                                 )  -- Quits XMonad
          , ((modKey                , xK_apostrophe            ), spawn "xmonad --recompile; xmonad --restart"   )  -- Restarts XMonad
          ] ++ [
          -- mod-[1..9], Switch to workspace N
          -- mod-shift-[1..9], Move client to workspace N
          ((m .|. modKey  , k), windows $ f i)                              --Taken from the defaultConfig,
              | (i, k) <- zip (workspaces conf) (xK_grave:[xK_1 .. xK_9])   --Added the ` key, aka xK_grave
              , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
          ] ++ [
          -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
          -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
          ((m .|. modKey  , key), screenWorkspace sc >>= flip whenJust (windows . f)) --Taken from the defaultConfig
              | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
          ] where modKey = myModKey host

--------------
--Workspaces--
--------------
myWorkspaces :: [String]
myWorkspaces = [ "dashboard"
               , "terminal"
               , "web"
               , "messaging"
               , "media"
               , "temporary"
               ]

myWSstatic :: [String]
myWSstatic =   [ "terminal"
               , "web"
               , "messaging"
               , "media"
               ]

myWSnever :: [String]
myWSnever =    [ "dashboard"
               ]

myWSabbrev :: String -> String
myWSabbrev ws = case ws of
  "dashboard" -> "dash"
  "terminal"  -> "term"
  "messaging" -> "msg"
  "media"     -> "mm"
  "temporary" -> "temp"
  _           -> ws

-----------------------------------------
--Layouts & Workspaces working together--
-----------------------------------------
myLayoutHook    = avoidStruts
                $ smartBorders
                $ mkToggle (single NBFULL)
                $ mkToggle (single MIRROR)
                $ windowNavigation myLayoutsPerWS

myLayoutsPerWS = onWorkspace "dashboard" fullLayout
               $ onWorkspace "terminal"  layoutTerm
               $ onWorkspace "web"       layoutWeb
               $ onWorkspace "messaging" layoutMsg
               $ onWorkspace "temporary" layoutTemp
               $ layoutDefault -- All other workspaces

-------------------
---Layout Control--
-------------------
--Type Sigs are crazy long
layoutDefault   = tabbedLayout ||| tiledLayout
layoutTerm      = tiledLayout ||| comboTabed ||| tabbedLayout
layoutWeb       = tiledLayout ||| tabbedLayout
layoutMsg       = msgLayout ||| tabbedLayout
layoutTemp      = comboTabed ||| tabbedLayout ||| tiledLayout ||| msgLayout

----------------------
--Customized Layouts--
----------------------
--The Type Sigs are very complicated and long so they are absent
fullLayout      = noBorders Full
tiledLayout     = Tall 1 (1/50) (1/2)
tabbedLayout    = tabbed shrinkText defaultTheme
comboTabed      = combineTwo tiledLayout tabbedLayout tabbedLayout
msgLayout       = gridIM (1%7) (ClassName "Pidgin")  --Make Buddy List stay on right

---------------
--Manage Hook--
---------------
--Returns Query True if q is a prefix of x
(^?) :: (Eq a) => XMonad.Query [a] -> [a] -> XMonad.Query Bool
q ^? x = (L.isPrefixOf x) <$> q

myManageHook :: XMonad.Query (MN.Endo WindowSet)
myManageHook = perApplicationHook <+> manageDocks

perApplicationHook :: XMonad.Query (MN.Endo WindowSet)
perApplicationHook = composeAll
    [ className =? "Firefox"            --> doShift "web"
    , className =? "Chromium"           --> doShift "web"
    , className =? "Pidgin"             --> doShift "messaging"
    , className =? "pidgin"             --> doShift "messaging"
    , className =? "Gimp"               --> doFloat             --Lets Gimp Windows Float by default
    , className =? "MPlayer"            --> doFloat             --MPlayer windows don't get docked
    , className =? "Spotify"            --> doShift "media"
    , className =? "Wine"               --> doFloat
    , className =? "Steam"              --> doFloat
    , className =? "steam"              --> doFullFloat         --bigpicture-mode
    , className =? "LOT"                --> doShift "dashboard" >> doIgnore
    , isFullscreen                      --> doFullFloat         --Good catch all for full screen video, smartBorders is also used on the layoutHook
    ] where role = stringProperty "WM_WINDOW_ROLE"

--------------------------------
--Log Hook (aka. dzen2 output)--
--------------------------------
xmLogHook :: Handle -> PP
xmLogHook pipe = xmobarPP
  { ppOutput          = hPutStrLn pipe
  , ppCurrent         = xmobarColor "green" "" . pad . myWSabbrev
  , ppHidden          = \ws -> if ws `elem` myWSnever
                                then ""
                                else xmobarColor "lightgrey" "" $ pad $ myWSabbrev ws
  , ppHiddenNoWindows = \ws -> ""
  -- , ppHiddenNoWindows = \ws -> if ws `notElem` myWSstatic
  --                               then ""
  --                               else xmobarColor "darkgrey" "" $ pad $ myWSabbrev ws
  , ppUrgent          = xmobarColor "red" "yellow" . pad . myWSabbrev
  , ppWsSep           = xmobarColor "red" "" "→"
  , ppSep             = ""
  , ppVisible         = wrap "∴" "∵"
  , ppLayout          = xmobarColor "yellow" "" .
                        (\x -> case x of
                          "Tall"        -> "‣ Ψ"
                          "Mirror Tall" -> "‣ Ξ"
                          "Full"        -> "‣ Ο"
                          "Tabbed Simplest" -> "‣ τ"
                          "combining Tabbed Simplest and Tabbed Simplest with Tall" -> "‣ Φ"
                          _             -> "‣ " ++ x
                        )
  , ppTitle           = wrap " |" "" . dzenEscape . pad
  }

--------------------------
--Mouse Position Updater--
--------------------------
-- mousePosUpdate = let center = (0.5, 0.5)
--                      nearestCorner = (0.9, 0.9)
--                  in updatePointer center nearestCorner

--------------------
--Helper Functions--
--------------------
hostname :: IO String
hostname = nodeName <$> getSystemID

--------------------------
--Host Specific Settings--
--------------------------
type Host = String
-- Used to be when I had a Mac. since super/meta where in switched positions from my other machines.
-- Now it is not as important, but kept incase I decide to change config massively.
myModKey :: Host -> KeyMask
myModKey h
  | h == "darknut" = mod4Mask
  | h == "bari"    = mod4Mask
  | otherwise      = mod1Mask

statusFont :: Host -> String
statusFont h
  | h == "darknut" = "xft:dejavu serif-9"
  | h == "bari"    = "xft:dejavu serif-7"
  | otherwise      = "xft:dejavu serif-9"

myXmobar :: Host -> String
myXmobar h = "xmobar -f \"" ++ statusFont h ++ "\""

