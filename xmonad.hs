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

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import Graphics.X11.ExtraTypes.XF86

import System.IO
import System.Exit
import System.Posix.Unistd

import Control.Monad((<=<))
import Control.Applicative((<$>))
import Data.Ratio ((%))

import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

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
myStatusDzen h  = "dzen2 -w 960 -h 20 -ta l" ++ myDzenOpts h
myStatusConky h = "conky -c ~/.conky/statusbar.rc | dzen2 -x 960 -w 960 -h 20 -ta r" ++ myDzenOpts h

----------------------
--Colors, Fonts, Etc--
----------------------
dzenBG      = "gray5"
dzenFG      = "gray75"
dzenFont h
  | h == "darknut" = "dejavu serif-10"
  | h == "thwomp"  = "dejavu serif-8"
  | otherwise      = "dejavu serif-10"
myDzenOpts h = " -fg '" ++ dzenFG ++ "' -bg '" ++ dzenBG ++ "' -fn '" ++ (dzenFont h) ++ "'"

----------------------
--Grid Select Config--
----------------------
gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 44, gs_cellwidth = 256 }

myGSColorizer = colorRangeFromClassName
                   (0x00,0x48,0x48) -- lowest inactive bg
                   (0x26,0xB1,0xB1) -- highest inactive bg
                   (0xFF,0x74,0x00) -- active bg
                   white            -- inactive fg
                   white            -- active fg
  where white = maxBound

-------------------
--Prompt Settings--
-------------------
myXPConfig :: XPConfig
myXPConfig = greenXPConfig { font = "xft:DejaVu Serif:pixelsize=12:autohint=true" }

---------------------
--Keyboard Bindings--
---------------------
myKeys host conf = M.fromList $ [
          --Mostly Defaults
            ((modKey   .|. shiftMask, xK_Return                ), spawn $ XMonad.terminal conf                   )  -- launch a terminal
          , ((modKey   .|. shiftMask, xK_c                     ), kill                                           )  -- Close focused window
          , ((modKey                , xK_space                 ), sendMessage NextLayout                         )  -- Rotate through the available layout algorithms
          , ((modKey   .|. shiftMask, xK_space                 ), setLayout $ XMonad.layoutHook conf             )  -- Reset the layouts on the current workspace to default
          , ((modKey                , xK_r                     ), rescreen                                       )  -- Redraws the windows
          , ((modKey                , xK_p                     ), spawn "yeganesh_exec"                          )  -- Toggle yeganesh (dmenu but better) yeganesh_exec is a quick script $ eval `yeganesh -x`
          --Media
          , ((0                     , xF86XK_AudioRaiseVolume  ), spawn "amixer set Master 2%+"                  )  -- Raise Volume, TODO: make it work on current audio device
          , ((0                     , xF86XK_AudioLowerVolume  ), spawn "amixer set Master 2%-"                  )  -- Lower Volume
          , ((0                     , xF86XK_AudioMute         ), spawn "amixer set Master toggle"               )  -- Mute
          , ((0                     , xF86XK_AudioPlay         ), safeSpawn "cmus-remote" ["-C", "player-pause"] )  -- CMus command, play/pause TODO: global media controls
          , ((0                     , xF86XK_AudioNext         ), safeSpawn "cmus-remote" ["-C", "player-next"]  )  -- CMus command, next song
          , ((0                     , xF86XK_AudioPrev         ), safeSpawn "cmus-remote" ["-C", "player-prev"]  )  -- CMus command, previous song
          , ((0                     , xF86XK_Eject             ), spawn "eject -T"                               )  -- Eject/Retract CD Drive
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
          , ((modKey   .|. controlMask, xK_space               ), sendMessage resetAlt)
          --Lock Computer
          , ((modKey   .|. shiftMask, xK_z                     ), spawn "slimlock"                               )  -- Locks screen with slimlock
          --Restarting/Closing XMonad
          , ((modKey   .|. shiftMask, xK_apostrophe            ), io (exitWith ExitSuccess)                      )  -- Quits XMonad
          , ((modKey                , xK_apostrophe            ), spawn "if type xmonad; then xmonad --recompile; xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")  -- Restarts XMonad
          --GridSelect Controls
          , ((modKey                , xK_g                     ), submap . M.fromList $
              [ ((0             , xK_g    ), goToSelected $ gsconfig2 myGSColorizer                              )  -- Displays grid of running apps, select one to go to it
              , ((shiftMask     , xK_g    ), bringSelected $ gsconfig2 myGSColorizer                             )  -- Displays grid of running apps, select one to pull it to current workspace
              ])
          --Prompts
          , ((modKey                , xK_s                     ), submap . M.fromList $
              [ ((0             , xK_a    ), promptSearch myXPConfig amazon                                      )  -- Prompts for amazon search
              , ((0             , xK_h    ), promptSearch myXPConfig hoogle                                      )  -- Prompts for hoogle search TODO: modify to open terminal with hoogle results
              , ((0             , xK_s    ), promptSearch myXPConfig google                                      )  -- Prompts for google search
              , ((0             , xK_t    ), appendFilePrompt myXPConfig "/home/zirro/doc/notes/todo.txt"        )  -- Prompts for quick todo note
              , ((0             , xK_w    ), promptSearch myXPConfig wikipedia                                   )  -- Prompts for wikipedia search
              , ((0             , xK_x    ), xmonadPrompt myXPConfig                                             )  -- XMonad basic command entry
              , ((controlMask   , xK_s    ), sshPrompt myXPConfig                                                )  -- Starts a new ssh session with prompt
              ])
          --Scratchpads
          , ((modKey                , xK_n                     ), submap .M.fromList  $
              [ ((0             , xK_f    ), namedScratchpadAction scratchpads "forth"                           )  -- Opens gforth
              , ((0             , xK_h    ), namedScratchpadAction scratchpads "haskell"                         )  -- Opens ghci
              , ((0             , xK_l    ), namedScratchpadAction scratchpads "htop"                            )  -- Opens htop in a temporary window
              , ((0             , xK_n    ), namedScratchpadAction scratchpads "notes"                           )  -- Opens vim on a note file
              , ((0             , xK_p    ), namedScratchpadAction scratchpads "python"                          )  -- Opens python intrepter
              , ((0             , xK_t    ), namedScratchpadAction scratchpads "todo"                            )  -- Opens my todo file
              ])
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
startupApps     = do
    safeSpawnProg "urxvtd"              --Daemon running my terminals, reduces resource usage and improves preformance
    safeSpawnProg "urxvtc"
    safeSpawnProg "xcape"  --Keyboard Daemon, intercepts Control, and sends Escape on short presses of Control, otherwise sends Control

---------------
--Scratchpads--
---------------

-- The manage hook declarations for these do not work, so they are handled below in myManageHook
scratchpads = [ NS "forth" "urxvtc -name sp_forth -e gforth"            (resource =? "sp_forth")     defaultFloating
              , NS "haskell" "urxvtc -name sp_haskell -e ghci"          (resource =? "sp_haskell")   defaultFloating
              , NS "htop" "urxvtc -name sp_htop -e htop"                (resource =? "sp_htop")      defaultFloating
              , NS "notes" "gvim --role sp_notes ~/doc/notes/notes.txt" (role =? "sp_notes")         defaultFloating
              , NS "python" "urxvtc -name sp_python -e bpython"         (resource =? "sp_python")    defaultFloating
              , NS "todo" "gvim --role sp_notes ~/doc/notes/todo.txt"   (role =? "sp_todo")          defaultFloating
              ] where role = stringProperty "WM_WINDOW_ROLE"



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
    , className =? "Iceweasel"          --> doShift "web"
    , className =? "Chromium"           --> doShift "web"
    , className =? "Pidgin"             --> insertPosition End Older <+> doShift "messaging"
    , className =? "xchat"              --> doShift "messaging"
    , className =? "Gimp"               --> doFloat             --Lets Gimp Windows Float by default
    , title     =? "pinentry"           --> doFloat
    , className =? "MPlayer"            --> doFloat             --MPlayer windows don't get docked
    , className =? "Spotify"            --> doShift "media"
    , resource  ^? "sp_"                --> doFloat
    , role      ^? "sp_"                --> doFloat
    , isFullscreen                      --> doFullFloat         --Good catch all for full screen video, smartBorders is also used on the layoutHook
    ] where role = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------
--Layouts & Workspaces working together--
-----------------------------------------
myLayoutHook    = onWorkspace "terminal"  layoutTerm $
                  onWorkspace "web"       layoutWeb $
                  onWorkspace "im"        layoutIM $
                  layoutDefault

-------------------
---Layout Control--
-------------------
layoutTerm      = comboTabed ||| tabbedLayout
layoutWeb       = fullLayout ||| tiledLayout
layoutIM        = fullLayout --gridIM (1%7) (ClassName "Pidgin")  --Make Buddy List stay on right
layoutDefault   = tabbedLayout ||| mosaicLayout ||| fullLayout

----------------------
--Customized Layouts--
----------------------
fullLayout      = noBorders Full
tiledLayout     = Tall 1 (1/50) (1/2)
mosaicLayout    = MosaicAlt M.empty
tabbedLayout    = tabbed shrinkText defaultTheme
comboTabed      = combineTwo (tiledLayout) (tabbedLayout) (tabbedLayout)

--------------------------------
--Log Hook (aka. dzen2 output)--
--------------------------------
myLogHook pipe = defaultPP
  { ppOutput          = hPutStrLn pipe
  , ppCurrent         = dzenColor "green" "" . pad . wrap "»" "«" . myWSabbrev
  , ppHidden          = \ws -> if ws `elem` myWSnever
                                then ""
                                else dzenColor "lightgrey" "" $ pad $ wrap "." "" $ myWSabbrev ws
  , ppHiddenNoWindows = \ws -> if ws `notElem` myWSstatic
                                then ""
                                else dzenColor "darkgrey" "" $ pad $ myWSabbrev ws
  , ppUrgent          = dzenColor "red" "yellow" . pad . myWSabbrev
  , ppWsSep           = "∙"
  , ppSep             = ""
  , ppVisible         = wrap "[" "]"
  , ppLayout          = dzenColor "yellow" "" .
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
    dzenStatusBar   <- spawnPipe $ myStatusDzen host
    conkyStatusBar  <- spawnPipe $ myStatusConky host
    -- startupApps
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        {
          terminal        = myTerminal
        , modMask         = myModKey host
        , workspaces      = myWorkspaces
        , keys            = myKeys host
        , layoutHook      = avoidStruts $ smartBorders $ windowNavigation $ myLayoutHook     -- smartBorders removes borders if only one window or a fullscreen floating window is up
        , manageHook      = myManageHook <+> manageDocks
        , handleEventHook = docksEventHook
        , logHook         = dynamicLogWithPP $ myLogHook dzenStatusBar
        , startupHook     = return () >> startupApps
        }
