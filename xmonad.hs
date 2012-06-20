import XMonad

import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import Graphics.X11.ExtraTypes.XF86

import System.IO
import System.Exit

import Control.Monad((<=<))
import Data.Ratio ((%))

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

---------------------------------------
--Default Settings for various things--
---------------------------------------
myModKey            = mod1Mask -- Alt/Default for xmonad --mod4Mask -- Windows/Command
myTerminal          = "urxvtc"
myWebBrowser        = "firefox"
myStatusDzen        = "dzen2 -w 960 -h 20 -ta l" ++ myDzenOpts
myStatusConky       = "conky | dzen2 -x 960 -w 960 -h 20 -ta r" ++ myDzenOpts
myXcompmgrSettings  = ["-f", "-I", ".075", "-O", ".075", "-D", "10", "-c", "-r", "20", "-o", ".8", "-C"]

----------------------
--Colors, Fonts, Etc--
----------------------
dzenBG      = "gray5"
dzenFG      = "gray75"
dzenFont    = "dejavu serif-11"
myDzenOpts  = " -fg '" ++ dzenFG ++ "' -bg '" ++ dzenBG ++ "' -fn '" ++ dzenFont ++ "'"

----------------------
--Grid Select Config--
----------------------
gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 44, gs_cellwidth = 256 }
myGSApps = ["urxvtc","firefox","spotify","xchat","eclipse"]

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
myKeys =  [
          --Media
            ((0, xF86XK_AudioRaiseVolume)         , spawn "amixer set Master 2%+" )
          , ((0, xF86XK_AudioLowerVolume)         , spawn "amixer set Master 2%-" )
          , ((0, xF86XK_AudioMute)                , spawn "amixer set Master toggle" )
          , ((0, xF86XK_AudioPlay)                , safeSpawn "cmus-remote" ["-C", "player-pause"] )
          , ((0, xF86XK_AudioNext)                , safeSpawn "cmus-remote" ["-C", "player-next"]  )
          , ((0, xF86XK_AudioPrev)                , safeSpawn "cmus-remote" ["-C", "player-prev"]  )
          , ((0, xF86XK_Eject)                    , spawn "eject -T")
          --Hiding of Dock
          , ((myModKey, xK_b)                     , sendMessage ToggleStruts)
          --Binding to pydmenu, will switch to yeganesh/dmenu later
          --, ((myModKey, xK_p)                     , spawn "pydmenu_run" )
          --Window Management
          , ((myModKey, xK_Return)                , dwmpromote )
          , ((myModKey, xK_Left)                  , prevWS )
          , ((myModKey, xK_Right)                 , nextWS )
          , ((myModKey .|. shiftMask, xK_Left)    , shiftToPrev >> prevWS )
          , ((myModKey .|. shiftMask, xK_Right)   , shiftToNext >> nextWS )
          , ((myModKey .|. controlMask, xK_Left)  , prevScreen )
          , ((myModKey .|. controlMask, xK_Right) , nextScreen )
          , ((myModKey .|. shiftMask .|. controlMask, xK_Left) , shiftPrevScreen )
          , ((myModKey .|. shiftMask .|. controlMask, xK_Right), shiftNextScreen )
          --Dynamic Workspaces
          , ((myModKey .|. shiftMask, xK_Up   )   , addWorkspacePrompt myXPConfig)
          , ((myModKey .|. shiftMask, xK_Down )   , removeWorkspace)
          --MosaicAlt Controls
          , ((myModKey .|. shiftMask, xK_h    )   , withFocused (sendMessage . expandWindowAlt))
          , ((myModKey .|. shiftMask, xK_l    )   , withFocused (sendMessage . shrinkWindowAlt))
          , ((myModKey .|. shiftMask, xK_j    )   , withFocused (sendMessage . tallWindowAlt))
          , ((myModKey .|. shiftMask, xK_k    )   , withFocused (sendMessage . wideWindowAlt))
          , ((myModKey .|. controlMask, xK_space) , sendMessage resetAlt)
          --GridSelect Controls
          , ((myModKey, xK_g), submap . M.fromList $
              [ ((0, xK_g)        , goToSelected $ gsconfig2 myGSColorizer)
              , ((shiftMask, xK_g), bringSelected $ gsconfig2 myGSColorizer)
              , ((0, xK_p)        , spawnSelected defaultGSConfig myGSApps)
              , ((0, xK_w)        , promptedGotoGrid)
              ])
          --Prompts
          , ((myModKey, xK_s), submap . M.fromList $
              [ ((0, xK_a), promptSearch myXPConfig amazon)
              , ((0, xK_h), promptSearch myXPConfig hoogle)
              , ((0, xK_s), promptSearch myXPConfig google)
              , ((0, xK_t), appendFilePrompt myXPConfig "/home/zirro/doc/notes/todo.txt")
              , ((0, xK_w), promptSearch myXPConfig wikipedia)
              , ((0, xK_x), xmonadPrompt myXPConfig)
              , ((9, xK_m), manPrompt  myXPConfig)
              , ((controlMask, xK_s), sshPrompt myXPConfig)
              ])
          --Scratchpads
          , ((myModKey, xK_n), submap .M.fromList $
              [ ((0, xK_l), namedScratchpadAction scratchpads "htop")
              , ((0, xK_d), namedScratchpadAction scratchpads "stardict")
              , ((0, xK_h), namedScratchpadAction scratchpads "haskell")
              , ((0, xK_n), namedScratchpadAction scratchpads "notes")
              , ((0, xK_p), namedScratchpadAction scratchpads "python")
              ])
          --TopicSpaces
          , ((myModKey, xK_t), submap . M.fromList $
              [ ((0, xK_n)        , spawnShell)
              , ((0, xK_t)        , currentTopicAction myTopicConfig)
              , ((0, xK_g)        , workspacePrompt myXPConfig goto)
              , ((shiftMask, xK_g), workspacePrompt myXPConfig $ windows . W.shift)
              ])
          --Lock Computer
          , ((myModKey .|. shiftMask, xK_z)       , spawn "xscreensaver-command -lock")
          --Restarting/Closing XMonad
          , ((myModKey .|. shiftMask, xK_apostrophe), io (exitWith ExitSuccess))
          , ((myModKey, xK_apostrophe), spawn "if type xmonad; then xmonad --recompile; xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
          --Start Enlightenment on top of current session
          , ((myModKey .|. controlMask, xK_apostrophe), restart "e17toxmd" False)
          ]
--          ++
--          [ ((myModKey, k), switchNthLastFocused myTopicConfig i)
--          | (i, k) <- zip [1..] workspaceKeys
--          ]
--          ++
--          zip (zip (repeat (myModKey)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
--          ++
--          zip (zip (repeat (myModKey .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

------------------------------------------------
--Default Application Startup, Dock is in main--
------------------------------------------------
startupApps     = do
    safeSpawnProg "urxvtd"              --Daemon running my terminals, reduces resource usage and improves preformance
    safeSpawnProg "urxvtc"              --Get a starting terminal running
    safeSpawn "xcompmgr" myXcompmgrSettings
    safeSpawnProg "xcape"  --Keyboard Daemon, intercepts Control, and sends Escape on short presses of Control, otherwise sends Control
    safeSpawnProg "xscreensaver"
--    safeSpawn "dropbox_cli" ["start"]
    safeSpawnProg "pidgin"
    safeSpawnProg "firefox"
--    safeSpawnProg "spotify"
--    safeSpawnProg "xchat"
--    safeSpawn "trayer" [ "--edge", "top"    --This is my System Tray
--                       , "--align", "right"
--                       , "--widthtype", "percent"
--                       , "--width", "5"
--                       , "--height", "21"
--                       , "--expand", "false"
--                       , "--transparent", "true"
--                       , "--alpha", "0"
--                       , "--tint", "0x000000"
--                       ]

---------------
--Scratchpads--
---------------
scratchpads = [ NS "htop" "urxvtc -e htop" (title =? "sp_htop") defaultFloating
              , NS "stardict" "stardict" (className =? "sp_stardict") defaultFloating
              , NS "notes" "gvim --role notes ~/doc/notes/notes.txt" (role =? "sp_notes") defaultFloating
              , NS "python" "urxvtc -e bpython3" (title =? "sp_python") defaultFloating
              , NS "haskell" "urxvtc -e ghci" (title =? "sp_haskell") defaultFloating
              ] where role = stringProperty "WM_WINDOW_ROLE"

----------------------
--Structures to help--
----------------------
--type myAbbrev       = String
--type myDir          = FilePath
--type myAction       =
--type myTopic        = (Topic, Bool, myAbbrev, myDir, myAction)


----------------
--Topic Spaces--
----------------
myTopics :: [Topic]
myTopics =  [ "dashboard"
            , "web"
            , "notes"
            , "im"
            , "irc"
            , "mail"
            , "music"
            , "video"
            , "python"
            , "haskell"
            , "android"
            , "java"
            , "development"
            , "pdf"
            , "school"
            , "documents"
            , "wine"
            , "minecraft"
            ]

--List of topics that should always be visible
myStaticTopics :: [Topic]
myStaticTopics =  [ "dashboard"
                  , "web"
                  , "notes"
                  , "im"
                  , "irc"
                  , "mail"
                  , "music"
                  ]

myTopicsToAbbrev ab = case ab of
  "dashboard"   -> "dash"
  "music"       -> "mus"
  "video"       -> "vid"
  "python"      -> "py"
  "haskell"     -> "hs"
  "android"     -> "droid"
  "development" -> "dev"
  "school"      -> "csm"
  "documents"   -> "doc"
  "minecraft"   -> "mc"
  _             -> ab

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $
    [ ("dashboard"  , "~/")
    , ("notes"      , "~/doc/notes")
    , ("web"        , "~/dl")
    , ("im"         , "~/")
    , ("irc"        , "~/")
    , ("mail"       , "~/")
    , ("music"      , "~/mm/mus")
    , ("video"      , "~/mm/mov")
    , ("python"     , "~/sc/py")
    , ("haskell"    , "~/sc/hs")
    , ("android"    , "~/")
    , ("java"       , "~/Eclipse")
    , ("development", "~/sc")
    , ("pdf"        , "~/")
    , ("school"     , "~/csm/s12")
    , ("documents"  , "~/")
    , ("wine"       , "~/.wine")
    , ("minecraft"  , "~/.minecraft")
    ]
  , defaultTopicAction = const $ spawnShell >*> 3
  , defaultTopic = "dashboard"
  , topicActions = M.fromList $
    [ ("dashboard", spawnShell)
    , ("notes"    , spawnInShell "vim ~/doc/notes/notes.txt")
    , ("web"      , spawn myWebBrowser)
    , ("im"       , spawn "pidgin")
    , ("irc"      , spawn "xchat")
    , ("mail"     , spawnInShell "mutt")
    , ("music"    , spawn "spotify" >> spawnInShell "cmus")
    , ("video"    , spawnShell)
    , ("python"   , spawnShell >*> 2 >> spawnInShell "bpython")
    , ("haskell"  , spawnShell >*> 2 >> spawnInShell "ghci")
    , ("android"  , spawnShell >*> 2)
    , ("java"     , spawnShell >*> 2)
    , ("pdf"      , spawnShell)
    , ("school"   , spawnShell >*> 2)
    , ("documents", spawnShell)
    , ("wine"     , spawnShell)
    ]
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvtc -cd " ++ dir

spawnInShell :: String -> X ()
spawnInShell app = spawn $ "urxvtc -e " ++ app

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

wsGrid = gridselect defaultGSConfig <=< asks $ map (\x -> (x,x)) . workspaces . config
promptedGotoGrid = wsGrid >>= flip whenJust (switchTopic myTopicConfig)

---------------
--Manage Hook--
---------------
myManageHook    = composeAll
    [ className =? "Firefox"                     --> doShift "web"
    , className =? "Iceweasel"                   --> doShift "web"
    , className =? "Chromium"                    --> doShift "web"
    , className =? "Pidgin"                      --> insertPosition End Older <+> doShift "im"
    , className =? "xchat"                       --> doShift "irc"
    , className =? "Gimp"                        --> doFloat             --Lets Gimp Windows Float by default
    , className =? "feh"                         --> doCenterFloat
    , title     =? "pinentry"                    --> doFloat
    , className =? "MPlayer"                     --> doFloat             --MPlayer windows don't get docked
    , className =? "Spotify"                     --> doShift "music"
    , className =? "net-minecraft-LauncherFrame" --> doShift "minecraft" <+> doFloat
    , isFullscreen                               --> doFullFloat         --Good catch all for full screen video, smartBorders is also used on the layoutHook
    ]

-----------------------------------------
--Layouts & Workspaces working together--
-----------------------------------------
myLayoutHook    = onWorkspace "web" layoutWeb $
                  onWorkspace "im"  layoutIM $
                  layoutDefault

------------------
---Layout Control--
------------------
layoutWeb       = fullLayout ||| mosaicLayout ||| tiledLayout
layoutIM        = gridIM (1%7) (ClassName "Pidgin")  --Make Buddy List stay on right
layoutDefault   = mosaicLayout ||| tiledLayout ||| Mirror tiledLayout ||| fullLayout

----------------------
--Customized Layouts--
----------------------
fullLayout      = noBorders Full
tiledLayout     = Tall 1 (1/50) (1/2)
mosaicLayout    = MosaicAlt M.empty

--------------------------------
--Log Hook (aka. dzen2 output)--
--------------------------------
myLogHook pipe = defaultPP
  { ppOutput          = hPutStrLn pipe
  , ppCurrent         = dzenColor "green" "" . pad . wrap "»" "«" . myTopicsToAbbrev
  , ppHidden          = dzenColor "lightgrey" "" . pad . wrap "." "" . myTopicsToAbbrev
  , ppHiddenNoWindows = \ws -> if ws `notElem` myStaticTopics then "" else dzenColor "darkgrey" "" $ pad $ myTopicsToAbbrev ws
  , ppUrgent          = dzenColor "red" "yellow" . pad . myTopicsToAbbrev
  , ppWsSep           = "∙"
  , ppSep             = ""
  , ppVisible         = wrap "[" "]"
  , ppLayout          = dzenColor "yellow" "" .
                        (\x -> case x of
                          "Tall"        -> "‣ Ψ"
                          "Mirror Tall" -> "‣ Ξ"
                          "Full"        -> "‣ Ο"
                          "MosaicAlt"   -> "‣ Ϡ"
                          _             -> "‣ " ++ x
                        )
  , ppTitle           = wrap " |" "" . dzenEscape . pad
  }


---------------------
--The actual config--
---------------------
main = do
    dzenStatusBar   <- spawnPipe myStatusDzen
    conkyStatusBar  <- spawnPipe myStatusConky
    -- startupApps
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { startupHook     = startupApps
        , workspaces      = myTopics
        , modMask         = myModKey
        , terminal        = myTerminal
        , manageHook      = myManageHook <+> manageDocks
        , layoutHook      = avoidStruts $ smartBorders $ myLayoutHook     -- smartBorders removes borders if only one window or a fullscreen floating window is up
        , handleEventHook = docksEventHook
        , logHook         = dynamicLogWithPP $ myLogHook dzenStatusBar
        }
        `additionalKeys` myKeys
