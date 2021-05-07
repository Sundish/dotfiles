-- neofetch --kitty ~/Images/cover_art/Drama.jpg --size 21%

import XMonad
import Data.Monoid
import Data.Char
import Data.List
import System.Exit

import XMonad.ManageHook

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell(shellPrompt)
import XMonad.Prompt.Input
-- import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
-- import XMonad.Prompt.AppendFile

import XMonad.Hooks.ManageDocks(avoidStruts, docks)
import XMonad.Hooks.ManageHelpers(composeOne, (-?>), currentWs)
import XMonad.Hooks.InsertPosition(insertPosition, Position ( End ), Focus ( Older, Newer ))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext(floatNextHook, runLogHook, toggleFloatAllNew)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast

-- import XMonad.Util.SpawnOnce()
import XMonad.Util.Run(runInTerm, hPutStrLn, spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Cursor(setDefaultCursor)
import XMonad.Util.NamedScratchpad(namedScratchpadManageHook, namedScratchpadAction, customFloating, NamedScratchpad(..))
import XMonad.Util.Stack
import Colors
-- import Test

import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts(toggleLayouts, ToggleLayout(..))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders(noBorders, smartBorders)
import XMonad.Layout.Renamed(renamed, Rename( Replace ))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.StackTile
import XMonad.Layout.MultiDishes
import XMonad.Layout.Reflect(reflectVert)
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
-- import XMonad.Layout.MagicFocus
-- import XMonad.Layout.Dishes
-- import XMonad.Layout.DragPane
-- import XMonad.Layout.OneBig
-- import XMonad.Layout.Combo
-- import XMonad.Layout.Simplest
-- import XMonad.Layout.SubLayouts
-- import XMonad.Layout.WindowNavigation
-- import XMonad.Layout.LayoutBuilder
-- import XMonad.Layout.LayoutCombinators hiding ( (|||) )

import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaceOrder( withNthWorkspace )
import XMonad.Actions.CycleWS( toggleWS' )
-- import XMonad.Actions.TagWindows( addTag )
import XMonad.Actions.WithAll( killAll )
import XMonad.Actions.PerWorkspaceKeys( bindOn )
import XMonad.Actions.ShowText
import XMonad.Actions.TagWindows
import XMonad.Actions.FocusNth
import XMonad.Actions.Submap
import qualified XMonad.Actions.FlexibleResize as X

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Foldable   as F
import qualified Data.Maybe      as A

scratchpads = [
  NS "htop" "st -T fhtop htop" (title =? "fhtop")
  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
  NS "terminal" "st -c 'floating'" (className =? "floating")
  (customFloating $ W.RationalRect l t w h),
  NS "calculator" "emacsclient --create-frame --frame-parameters='(quote (name . \"minicalc-frame\"))' --eval '(full-calc)' -s instance1"
  (title =? "minicalc-frame") (customFloating $ W.RationalRect l t w h),
  NS "DT" "dwarftherapist" (resource =? "dwarftherapist")
  (customFloating $ W.RationalRect l t w h)
  ] where h = 0.9
          w = 0.9
          t = 0.95 -h
          l = 0.95 -w

-- Borrowed Functions
-- windowCount :: X (Maybe Int) -- taken from: https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
-- windowCount = gets $ Just . length . W.integrate' . W.stack . W.workspace . W.current . windowset
-- funcTest d = withNthWorkspace W.shift d >>

zipM  m ks as f = zipWith (\k d -> (m ++ k, f d)) ks as

-- My Functions
-- windowCountAll :: X (W.Workspace i l a)
-- wc :: X (Maybe [Int])

-- weirdshit = gets $ spawn $ "st -d `pwdx " ++ (show $ A.fromMaybe 0 $ runQuery pid $ getFocusZ fromIndex . W.integrate' . W.stack . W.workspace . W.current . windowset) ++ "`" -- <- WOW!

-- This is to duplicated the current focus terminal, for some reason it only works once
duplicateTerminalF :: X () -- TODO run operation more than one time
duplicateTerminalF = withFocused (\w -> runQuery pid w >>= f . A.fromMaybe 0) -- runQuery pid p
	where
		f x | x == 0 = spawn myTerminal
		    | otherwise = spawn $ "st -d `readlink -e /proc/" ++ (show $ 1 + x) ++ "/cwd/`"

checkPass :: String -> X()
checkPass "exitnow" = io (exitWith ExitSuccess)

checkPrompt :: X()
checkPrompt = inputPrompt sunConfigDef "Ts'o'okol" ?+ checkPass

-- wc :: X [(WorkspaceId, Maybe Int)]
-- wc = gets $ (\w -> [((W.tag a), (Just . length . W.integrate' $ W.stack a)) | a <- w]) . W.hidden . windowset -- <- WOW!
wsKeys = map show $ [1..7]

dotIt :: [a] -> [Char]
dotIt = map (\_ -> '~')

aCoupleWords :: Int -> String -> String
aCoupleWords i a | length wl < i = a
                 | otherwise = unwords $ take i wl
  where
    wl = delete "-" $ words a

dunstSend :: X ()
dunstSend = spawn ("dunstify -a \"Sys\" -t 2500 --urgency=low \"info\" " ++ il ++ ib ++ "\"")
  where
    il = "\"backlight: $(light -G | awk '{print $1 + 0.5}')"
    ib = "\nbattery: $(cat /sys/class/power_supply/BAT1/capacity)%"

-- Default Terminal
myTerminal      = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- WORKSPACES HERE
--
genWs = "GEN"
webWs = "WEB"
wrkWs = "WRK"
styWs = "STY"
comWs = "COM"
sysWs = "SYS"
mscWs = "MSC"

myWorkspaces    = [ genWs
                  , webWs
                  , wrkWs
                  , styWs
                  , comWs
                  , sysWs
                  , mscWs]



-- PROJECTS HERE
projects :: [Project]
projects = [ Project { projectName      = mscWs
                     , projectDirectory = "~/music/"
                     , projectStartHook = Just $ do
                                                    runInTerm "" "alsamixer"
                                                    runInTerm "" "ncmpcpp"
                                                    runInTerm "" "ncmpcpp -s visualizer"
                     }]

-- ///////////////////
-- THEME CONFIGURATION
-- ///////////////////

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#0b0f11"
myFocusedBorderColor = "#14DB49"

sunConfigDef = def { font        = "xft:Source Code Pro:pixelsize=12:antialias=true:hinting=true"
                   , bgColor     = background
                   , fgColor     = foreground
                   , bgHLight    = color0
                   , fgHLight    = color1
                   , position    = (CenteredAt (1/3) (13/20))
                   , maxComplRows = Just 8
                   , historySize = 256}

myTabTheme =   def { activeColor         = "#746C48" -- "#556064" color1
                   , inactiveColor       = "#181512" -- "#1D1F21" color0
                   , urgentColor         = "#605655" -- "#FDF6E3" color2
                   , activeBorderColor   = "#9A875F" -- "#454948" color4
                   , inactiveBorderColor = "#817267" -- "#454948" color3
                   , urgentBorderColor   = "#556D70" -- "#268BD2" color5
                   , activeTextColor     = "#BEA492" -- "#80FFF9" color7
                   , inactiveTextColor   = "#BEA492" -- "#1ABC9C" color6
                   , urgentTextColor     = "#556D70" -- "#1ABC9C" color8
                   , fontName            = "xft:Source Code Pro:size=10:antialias=true"}

myTextTheme =  def { st_font = "xft:Source Code Pro:size=10:antialias=false:italic"
                   , st_bg   = "#12121c"
                   , st_fg   = "#FFCBAB"
                   , st_ypos = 745}

-- Status bars and logging

myLogHook h = dynamicLogWithPP $ def
			{ ppCurrent         = xmobarColor "#C9B957" "" . wrap "<box type=Bottom width=2 color=#8F423C>" "</box>" -- color 3
			, ppHiddenNoWindows = xmobarColor "#4B5C5E" "" . dotIt
			, ppHidden          = xmobarColor "#709289" ""
			, ppLayout          = xmobarColor "#EEAA88" ""
			, ppSep             = " : "
			, ppWsSep           = " "
			-- , ppOrder           = id
			, ppTitle           = id -- xmobarColor "#8C644C" "" . wrap "<fn=3>" "</fn>" . shorten 20
			, ppOutput          = hPutStrLn h
                        -- , ppExtras = [willFloatAllNewPP (\_ -> "-f")]
                        , ppOrder  = \(ws:l:_:_) -> [ws,l]
                        }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modm,               xK_c     ), flashText myTextTheme 1 "kíimij!" >> kill)

    -- close all windows in workspace
    , ((modm .|. shiftMask, xK_c     ), flashText myTextTheme 1 "kíimo'ob!" >> killAll)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- --  Reset the layouts on the current workspace to default
    -- , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    -- , ((modm,               xK_Tab   ), toggleWS' ["NSP"])

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), checkPrompt)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- , ((modm .|. controlMask .|. shiftMask, xK_s), namedScratchpadAction scratchpads "stardict")
    -- , ((modm .|. controlMask .|. shiftMask, xK_n), namedScratchpadAction scratchpads "notes")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    -- ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    -- [((m .|. modm, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myKeysB = [ ("M-<R>",   spawn "xbacklight -inc 5")
          , ("M-S-<R>", spawn "xbacklight -inc 10")
          , ("M-<L>",   spawn "xbacklight -dec 5")
          , ("M-S-<L>", spawn "xbacklight -dec 10")

          -- Volume keybindings
          , ("M-<U>",   spawn "pulsemixer --change-volume +5")
          , ("M-S-<U>", spawn "pulsemixer --change-volume +10")
          , ("M-S-<D>", spawn "pulsemixer --change-volume -10")
          , ("M-<D>",   spawn "pulsemixer --change-volume -5")

          -- Dunst Keybindings
          -- , ("M-i",             dunstSend)
          -- , ("M-<KP_Multiply>", spawn "dunstctl close-all")
          -- , ("M-<KP_Subtract>", spawn "dunstctl close")
          , ("M-g",           spawn "/home/sundish/Documents/scripts/translate_menu.sh")
          -- , ("M-g e",           spawn "/home/sundish/scripts/translate.sh 0")
          -- , ("M-g s",           spawn "/home/sundish/scripts/translate.sh 1")

          -- Program Keybindings
          , ("M-b",   spawn "qutebrowser")
          , ("M-C-<Return>", duplicateTerminalF)

          -- Dmenu keybindings
          , ("M-o",   spawn "dmenu_run -p 'run::cmd' ")
          , ("M-p d", spawn "ls ~/Documents/datasheets/ | dmenu -i -p 'data::sheet' | xargs -i zathura '~/Documents/datasheets/{}'")
          , ("M-p p", spawn "ls ~/Documents/pdfmans/ | dmenu -i -p 'pdf::store' | xargs -i zathura \"~/Documents/pdfmans/{}\"")
          , ("M-p s", spawn "passmenu")
          -- , ("M-p s", spawn "st -c 'passprompt' -g=65x18+470+260 -e ~/Documents/scripts/pasmenu")
-- st -c 'passprompt' -g=65x18+470+260 -e passmenu
          -- Screenshot Keybindings
          , ("M-s", spawn "flameshot gui") -- Grab the selected area, saves it cache and the clipboard.
          -- , ("M-s", spawn "flameshot full -c") -- Grab the selected area, saves it cache and the clipboard.

          -- Sticky window bindings
          , ("M-i", flashText myTextTheme 1 "beeta'anli'e!" >> withFocused (addTag "ghost"))
          , ("M-S-i", flashText myTextTheme 1 "k'askuntaj!" >> withFocused (delTag "ghost"))
	  , ("M-C-i", withFocused $ (\x -> windows $ W.float x $ W.RationalRect (7/10) (7/10) (3/10) (3/10)))
	  -- , ("M-S-b", withFocused $ (\x -> windows $ W.float x $ W.RationalRect (7/10) (1/40) (3/10) (3/10)))
	  -- , ("M-S-i", withFocused $ (\x -> windows $ W.float x $ W.RationalRect (1/40) (1/40) (3/10) (3/10)))
	  -- , ("M-S-a", withFocused $ (\x -> windows $ W.float x $ W.RationalRect (1/40) (7/10) (3/10) (3/10)))
          -- Layout keybindings
          , ("M-C-<Space>", sendMessage ToggleLayout)
	  -- (11/20) (11/20) (49/100) (14/29)
          , ("M-<F1>", namedScratchpadAction scratchpads "terminal")
          , ("M-<F2>", namedScratchpadAction scratchpads "htop")
          , ("M-<F4>", namedScratchpadAction scratchpads "calculator")
          -- , ("M-w", spawn "wal -o \"xmdcmp\" --saturate 0.9 -q -e -i ~/Wallpapers/wal/")
          , ("M-r", bindOn [ ("" , toggleFloatAllNew >> runLogHook)
                           , ("MSC", spawn "pidof /home/sundish/bin/msxiv sxiv | xargs kill -1; msxiv")])
          ]
          ++ zipM "M-"  wsKeys [0..]  (\a -> (withTagged "ghost" (\x -> withNthWorkspace (\b -> W.shiftWin b x) a))
	  				   >> withNthWorkspace W.greedyView a
					   >> withTagged "ghost" (\_ -> toggleFocus)) -- TODO find a way to not use two 'withTagged'
          ++ zipM "M-C-" wsKeys [0..] (withNthWorkspace W.shift)
          ++ zipM "M-S-" wsKeys [0..] (\a -> (withNthWorkspace W.shift a
	  				   >> withNthWorkspace W.greedyView a))
          -- , ("M-<F3>", switchProject (projects !! 0)
          --   )
          -- , ("M-m t", appendFilePrompt' sunConfigAdd ("* TODO " ++) "/home/sundish/org/agenda.org" (\_ -> "Agenda"))
-- (\a -> withTagged "ghost" (\x -> (withNthWorkspace (\b -> W.shiftWin b x) a)))
-- (\a -> withNthWorkspace W.greedyView a >> (withTagged "ghost" (\x -> shiftHere x))
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> X.mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

myLayout = onWorkspace genWs start $ smartBorders 
           ( toggleLayouts Full
           $ avoidStruts
           $ onWorkspace mscWs mtile
	   $ onWorkspace webWs webl
           $ (threeCM ||| tab ||| tiled))
  where
     -- default tiling algorithm partitions the screen into two panes
     mtile     = renamed [Replace "PAAX!"]
               $ spacingRaw True myBorders2 True myBorders2 True
               $ reflectVert
               $ noBorders
               $ ResizableTall 2 (3/100) (17/20) [(9/10), (9/10), (29/20)] -- 11/20

     start     = renamed [Replace "~~<"]
     	       $ avoidStruts
               $ spacingRaw False (Border 50 50 50 50) True (Border 5 5 5 5) True
               $ noBorders
               $ MultiDishes 1 3 (6/10)

     webl      = renamed [Replace "ICH"]
               $ spacingRaw True myBorders True myBorders True
               $ MultiDishes 1 3 (3/10)

     tiled     = renamed [Replace "KA'AN"]
               $ mySpacing
               $ Tall 1 (3/100) (1/2)

     threeCM   = renamed [Replace "PETEN"]
               $ mySpacing
               $ ThreeColMid 1 (3/100) (1/2)

     tab       = renamed [Replace "CHEEM"]
               $ noBorders
               $ tabbed shrinkText myTabTheme

     mySpacing  = spacingRaw False myBorders True myBorders True
     myBorders2 = (Border 5 5 5 5)
     myBorders  = (Border 3 3 3 3)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeOne
    [ className =? "MPlayer"          -?> doFloat
    , className =? "Gimp"             -?> doFloat
    , className =? "jetbrains-studio" -?> doFloat
    , className =? "Blender"          -?> doFloat
    , className =? "Steam"            -?> doFloat
    , className =? "gnuplot_qt"       -?> doFloat
    , className =? "passprompt"       -?> doFloat
    -- , className =? "mpv"              -?> (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , resource  =? "cover_art"        -?> (insertPosition End Older)
    , currentWs =? "WEB"              -?> (insertPosition End Newer)
    , resource  =? "desktop_window"   -?> doIgnore
    , resource  =? "kdesktop"         -?> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = refocusLastWhen (isFloat) <+> handleTimerEvent -- mempty
-- myEventHook = serverModeEventHookCmd
--               <+> serverModeEventHook

------------------------------------------------------------------------

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
--
-- By default, do nothing.
myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
  -- spawnOnce "nitrogen --restore"
  -- spawnOnce "nitrogen --set-zoom-fill /home/sundish/Wallpapers/wal/art_river.jpg"
  -- spawnOnce "[ ! -s ~/.config/mpd/pid ] && mpd &"
  -- spawnOnce "wal --saturate 0.9 -q -e -i /home/sundish/Wallpapers/wal/ &"
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
        xmproc <- spawnPipe "xmobar /home/sundish/.xmonad/xmobarc.hs"
	xmonad $ docks $ dynamicProjects projects (defaults xmproc)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults p = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = refocusLastLayoutHook $ myLayout,
        manageHook         = floatNextHook <+> myManageHook <+> namedScratchpadManageHook scratchpads,
        handleEventHook    = myEventHook,
        logHook            = refocusLastLogHook <+> myLogHook p,
        startupHook        = myStartupHook
    } `additionalKeysP` myKeysB

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
