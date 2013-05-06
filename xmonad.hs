import XMonad

import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FlexibleResize
import XMonad.Actions.GridSelect
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.Search
import XMonad.Actions.Submap

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import XMonad.Util.Run

import System.Directory
import System.FilePath.Posix
import System.IO
import System.Posix.Unistd
import System.Exit (exitSuccess)

import Control.Monad ((<=<), liftM)
import Control.Applicative ((<$>))
import Data.Ratio ((%))

import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad.StackSet as W

--------------------
--Helper Functions--
--------------------
hostname = nodeName <$> getSystemID

---------------------------------------
--Default Settings for various things--
---------------------------------------
myModKey h
  | h == "darknut" = mod1Mask -- Alt/Meta
  | h == "thwomp"  = mod4Mask -- Windows/Command/Super
  | otherwise      = mod1Mask
myTerminal      = "urxvtc"
myWebBrowser    = "firefox"
myXmobar h = "xmobar -f \"" ++ statusFont h ++ "\""

----------------------
--Colors, Fonts, Etc--
----------------------
statusFont h
  | h == "darknut" = "xft:dejavu serif-9"
  | h == "thwomp"  = "xft:dejavu serif-7"
  | otherwise      = "xft:dejavu serif-9"

---------------------
--Keyboard Bindings--
---------------------
myKeys host home conf = M.fromList $ [
          --Mostly Defaults
            ((modKey   .|. shiftMask, xK_Return                ), spawn $ XMonad.terminal conf                   )  -- launch a terminal
          , ((modKey   .|. controlMask, xK_Return              ), safeSpawn (combine home "atm/lastDir") []      )
          , ((modKey   .|. shiftMask, xK_c                     ), kill                                           )  -- Close focused window
          , ((modKey                , xK_space                 ), sendMessage NextLayout                         )  -- Rotate through the available layout algorithms
          , ((modKey   .|. shiftMask, xK_space                 ), setLayout $ XMonad.layoutHook conf             )  -- Reset the layouts on the current workspace to default
          , ((modKey                , xK_r                     ), rescreen                                       )  -- Redraws the windows
          , ((modKey                , xK_p                     ), spawn "dmenu_run"                              )  -- Toggle dmenu, application launcher
          --Hiding of stuff
          , ((modKey                , xK_b                     ), sendMessage ToggleStruts                       )  -- Hide Status Bars
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
          , ((modKey   .|. shiftMask, xK_h                     ), withFocused (sendMessage . expandWindowAlt)    )  -- (Mosaic Layout) Increases focused window's size
          , ((modKey                , xK_l                     ), sendMessage Expand                             )  -- Expand the master area
          , ((modKey   .|. shiftMask, xK_l                     ), withFocused (sendMessage . shrinkWindowAlt)    )  -- (Mosaic Layout) Decreases focused window's size
          , ((modKey                , xK_m                     ), windows W.focusMaster                          )  -- Move focus to the master window
          , ((modKey                , xK_t                     ), withFocused $ windows . W.sink                 )  -- Push window back into tiling
          , ((modKey                , xK_comma                 ), sendMessage (IncMasterN 1)                     )  -- Increment the number of windows in the master area
          , ((modKey                , xK_period                ), sendMessage (IncMasterN (-1))                  )  -- Deincrement the number of windows in the master area
          , ((modKey                , xK_Right                 ), sendMessage $ Move R                           )  -- Moves current window right
          , ((modKey                , xK_Left                  ), sendMessage $ Move L                           )  -- Moves current window left
          , ((modKey                , xK_Up                    ), sendMessage $ Move U                           )  -- Moves current window up
          , ((modKey                , xK_Down                  ), sendMessage $ Move D                           )  -- Moves current window down
          , ((modKey   .|. shiftMask, xK_Right                 ), sendMessage $ Swap R                           )  -- Make these use the same keys as the above 4 when not in dual tabbed mode
          , ((modKey   .|. shiftMask, xK_Left                  ), sendMessage $ Swap L                           )
          , ((modKey   .|. shiftMask, xK_Up                    ), sendMessage $ Swap U                           )
          , ((modKey   .|. shiftMask, xK_Down                  ), sendMessage $ Swap D                           )
          --Layout Management
          , ((modKey   .|. controlMask, xK_space               ), sendMessage resetAlt                           )  -- Resets layouts to default settings
          --Lock Computer
          , ((modKey   .|. shiftMask, xK_z                     ), spawn "slimlock"                               )  -- Locks screen with slimlock
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

------------------------------------------------
--Default Application Startup, Dock is in main--
------------------------------------------------
startupApps = safeSpawnProg "urxvtd" --Daemon running my terminals, reduces resource usage and improves preformance

--------------
--Workspaces--
--------------
myWorkspaces = [ "dashboard"
               , "terminal"
               , "web"
               , "messaging"
               , "media"
               , "temporary"
               ]

myWSstatic =   [ "terminal"
               , "web"
               , "messaging"
               , "media"
               ]

myWSnever =    [ "dashboard"
               ]

myWSabbrev ws = case ws of
  "dashboard" -> "dash"
  "terminal"  -> "term"
  "messaging" -> "msg"
  "media"     -> "mm"
  "temporary" -> "temp"
  _           -> ws


---------------
--Manage Hook--
---------------

--Returns Query True if q is a prefix of x
(^?) :: (Eq a) => XMonad.Query [a] -> [a] -> XMonad.Query Bool
q ^? x = (L.isPrefixOf x) <$> q

myManageHook    = composeAll
    [ className =? "Firefox"            --> doShift "web"
    , className =? "Chromium"           --> doShift "web"
    , className =? "Pidgin"             --> doShift "messaging"
    , className =? "pidgin"             --> doShift "messaging"
    , className =? "Gimp"               --> doFloat             --Lets Gimp Windows Float by default
    , className =? "MPlayer"            --> doFloat             --MPlayer windows don't get docked
    , className =? "Spotify"            --> doShift "media"
    , className =? "Wine"               --> doFloat
    , className =? "Steam"              --> doFloat >> doIgnore
    , className =? "steam"              --> doFullFloat         --bigpicture-mode
    , className =? "LOT"                --> doShift "dashboard" >> doIgnore
    , isFullscreen                      --> doFullFloat         --Good catch all for full screen video, smartBorders is also used on the layoutHook
    ] where role = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------
--Layouts & Workspaces working together--
-----------------------------------------
myLayoutHook    = onWorkspace "dashboard" fullLayout $
                  onWorkspace "terminal"  layoutTerm $
                  onWorkspace "web"       layoutWeb $
                  onWorkspace "im"        layoutIM $
                  layoutDefault

-------------------
---Layout Control--
-------------------
layoutTerm      = comboTabed ||| tabbedLayout ||| tiledLayout ||| mosaicLayout
layoutWeb       = fullLayout ||| tiledLayout
layoutIM        = fullLayout ||| mosaicLayout ||| imLayout
layoutDefault   = tabbedLayout ||| mosaicLayout ||| fullLayout

----------------------
--Customized Layouts--
----------------------
fullLayout      = noBorders Full
tiledLayout     = Tall 1 (1/50) (1/2)
mosaicLayout    = MosaicAlt M.empty
tabbedLayout    = tabbed shrinkText defaultTheme
comboTabed      = combineTwo tiledLayout tabbedLayout tabbedLayout
imLayout        = gridIM (1%7) (ClassName "Pidgin")  --Make Buddy List stay on right

--------------------------------
--Log Hook (aka. dzen2 output)--
--------------------------------
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
                          "MosaicAlt"   -> "‣ Ϡ"
                          "Tabbed Simplest" -> "‣ τ"
                          "combining Tabbed Simplest and Tabbed Simplest with Tall" -> "‣ Φ"
                          _             -> "‣ " ++ x
                        )
  , ppTitle           = wrap " |" "" . dzenEscape . pad
  }
---------------------
--The actual config--
---------------------
main = do
    host            <- hostname
    homeDir         <- getHomeDirectory
    xmPipe          <- spawnPipe $ myXmobar host
    -- dzenStatusBar   <- spawnPipe $ myStatusDzen host
    -- conkyStatusBar  <- spawnPipe $ myStatusConky host
    -- startupApps
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        {
          terminal        = myTerminal
        , modMask         = myModKey host
        , workspaces      = myWorkspaces
        , keys            = myKeys host homeDir
        , layoutHook      = avoidStruts $ smartBorders $ windowNavigation myLayoutHook     -- smartBorders removes borders if only one window or a fullscreen floating window is up
        , manageHook      = myManageHook <+> manageDocks
        , handleEventHook = docksEventHook
        , logHook         = dynamicLogWithPP $ xmLogHook xmPipe
        , startupHook     = return () >> startupApps
        }
