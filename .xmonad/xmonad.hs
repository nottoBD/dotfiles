    
import XMonad
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Process (callCommand)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
--for window pinning
import XMonad.Actions.CopyWindow (kill1)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import Control.Monad (guard, when)
--
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..), findWorkspace, nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.Search as S
import XMonad.Actions.SpawnOn

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.Tree
import Data.List (find, isInfixOf, isPrefixOf)
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed


-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.NamedActions as NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import XMonad.Util.Cursor
-- Battery periodic check (TEST)
import Control.Concurrent (forkIO, threadDelay)
--import Control.Monad (forever, when, void)
-- Right handed master pane test
import XMonad.Layout.Reflect


-- Doom One color scheme
colorScheme = "doom-one"
colorBack = "#282c34"
colorFore = "#bbc2cf"
color01 = "#1c1f24"
color02 = "#ff6c6b"
color03 = "#98be65"
color04 = "#da8548"
color05 = "#51afef"
color06 = "#c678dd"
color07 = "#5699af"
color08 = "#202328"
color09 = "#5b6268"
color10 = "#da8548"
color11 = "#4db5bd"
color12 = "#ecbe7b"
color13 = "#3071db"
color14 = "#a9a1e1"
color15 = "#46d9ff"
color16 = "#dfdfdf"


myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "zen-browser"

myEditor :: String
myEditor = myTerminal ++ " -e vim "

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor   = colorBack

myFocusColor :: String    
myFocusColor  = color15

mySoundPlayer :: String
mySoundPlayer = "ffplay -nodisp -autoexit " 

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


myStartupHook = do
    -- Kill any previous instances
    safeSpawn "killall" ["conky"]
    safeSpawn "killall" ["blueman-applet"]
    safeSpawn "killall" ["trayer"]
    safeSpawn "killall" ["picom"]
    safeSpawn "killall" ["volctl"]

    safeSpawn "killall" ["xsettingsd"]
    liftIO $ threadDelay (2 * 1000000)

    safeSpawn "trayer" ["--edge", "top", "--align", "right", "--widthtype", "request", "--padding", "4", "--SetDockType", "true", "--SetPartialStrut", "false", "--expand", "true", "--transparent", "true", "--alpha", "0", "--tint", "0x282c34", "--height", "22", "--monitor", "primary"]
    safeSpawn "dunst" []
    safeSpawn "conky" ["-c", "/home/devid/.config/conky/doom-one-01.conkyrc"]
    safeSpawn "picom" []
    safeSpawn "volctl" []
    safeSpawn "blueman-applet" []
    spawnOnce "xsettingsd"
    spawnOnce "x-settings"
    spawnOnce "$HOME/.local/bin/phinger-time-daemon"
    
    --spawnOnOnce (myWorkspaces !! 0) "emacs"                    -- Doom Emacs → workspace 1
    spawnOnOnce (myWorkspaces !! 1) "zen-browser"  -- Zen → workspace 3
    spawnOnOnce (myWorkspaces !! 2) (myTerminal ++ " -e fish") -- Alacritty → workspace 4
    spawnOnOnce (myWorkspaces !! 8) "brave"                 -- brave → workspace 8
    --spawnOnOnce (myWorkspaces !! 6) "cryptomator"                 -- Cryptomator → workspace 7
    
    spawn "$HOME/.local/bin/x-settings && xset s off -dpms"
    spawnOnce "numlockx"
    spawnOnce "xset -q | grep -q 'Caps Lock:[[:space:]]*on' && xdotool key Caps_Lock"



myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_h)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_l)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_k)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_j)     , move (0,-1)  >> myNavigation)
         ,((0,xK_y)     , move (-1,-1) >> myNavigation)
         ,((0,xK_i)     , move (1,-1)  >> myNavigation)
         ,((0,xK_n)     , move (-1,1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                (0x28,0x2c,0x34) -- lowest inactive bg
                (0x28,0x2c,0x34) -- highest inactive bg
                (0xc7,0x92,0xea) -- active bg
                (0xc0,0xa7,0x9a) -- inactive fg
                (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_navigate    = myNavigation
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 180
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
    selectedActionM <- gridselect conf actions
    case selectedActionM of
        Just selectedAction -> selectedAction
        Nothing -> return ()

gsCategories =
  [ ("Games",      "xdotool key super+alt+1")
  , ("Education",  "xdotool key super+alt+2")
  , ("Internet",   "xdotool key super+,alt+3")
  , ("Multimedia", "xdotool key super+alt+4")
  , ("Office",     "xdotool key super+alt+5")
  , ("Settings",   "xdotool key super+alt+6")
  , ("System",     "xdotool key super+alt+7")
  , ("Utilities",  "xdotool key super+alt+8")
  ]

gsGames =
  [ ("0 A.D.", "0ad")
  , ("Battle For Wesnoth", "wesnoth")
  , ("OpenArena", "openarena")
  , ("Sauerbraten", "sauerbraten")
  , ("Steam", "steam")
  , ("Unvanquished", "unvanquished")
  , ("Xonotic", "xonotic-glx")
  ]

gsEducation =
  [ ("GCompris", "gcompris-qt")
  , ("Kstars", "kstars")
  , ("Minuet", "minuet")
  , ("Scratch", "scratch")
  ]

gsInternet =
  [ ("Zen", "zen")
  , ("Discord", "discord")
  , ("Element", "element-desktop")
  , ("Firefox", "firefox")
  , ("LBRY App", "lbry")
  , ("Mailspring", "mailspring")
  , ("Nextcloud", "nextcloud")
  , ("Brave", "brave")
  , ("Transmission", "transmission-gtk")
  , ("Zoom", "zoom")
  ]

gsMultimedia =
  [ ("Audacity", "audacity")
  , ("Blender", "blender")
  , ("Deadbeef", "deadbeef")
  , ("Kdenlive", "kdenlive")
  , ("OBS Studio", "obs")
  , ("VLC", "vlc")
  ]

gsOffice =
  [ ("Document Viewer", "evince")
  , ("LibreOffice", "libreoffice")
  , ("LO Base", "lobase")
  , ("LO Calc", "localc")
  , ("LO Draw", "lodraw")
  , ("LO Impress", "loimpress")
  , ("LO Math", "lomath")
  , ("LO Writer", "lowriter")
  ]

gsSettings =
  [ ("ARandR", "arandr")
  , ("ArchLinux Tweak Tool", "archlinux-tweak-tool")
  , ("Customize Look and Feel", "lxappearance")
  , ("Firewall Configuration", "sudo gufw")
  ]

gsSystem =
  [ ("Alacritty", "alacritty")
  , ("Bash", (myTerminal ++ " -e bash"))
  , ("Htop", (myTerminal ++ " -e htop"))
  , ("Fish", (myTerminal ++ " -e fish"))
  , ("PCManFM", "pcmanfm")
  , ("VirtualBox", "virtualbox")
  , ("Virt-Manager", "virt-manager")
  , ("Zsh", (myTerminal ++ " -e zsh"))
  ]

gsUtilities =
  [ ("Emacs", "emacs")
  , ("Emacsclient", "emacsclient -c -a '' --eval '(progn (+doom-dashboard/open (selected-frame)))'")
  , ("Nitrogen", "nitrogen")
  , ("Vim", (myTerminal ++ " -e vim"))
  ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -t mocp -e mocp"
    findMocp   = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ limitWindows 5
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 2
           $ reflectHoriz                           -- Right handed master pane
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ avoidStruts 
           $ Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 9
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }


-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=60"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }

-- The layout hook
myLayoutHook = avoidStruts
               $ mouseResize
               $ windowArrange
               $ T.toggleLayouts (noBorders Full)
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                                           ||| noBorders monocle
                                           ||| floats
                                           ||| noBorders tabs
                                           ||| grid


myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices


myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
  -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
  -- I'm doing it this way because otherwise I would have to write out the full
  -- name of my workspaces and the names would be very long if using clickable workspaces.
  [ className =? "confirm"            --> doFloat
  , className =? "file_progress"      --> doFloat
  , className =? "flameshot"          --> doFloat
  , className =? "dialog"             --> doFloat
  , className =? "download"           --> doFloat
  , className =? "error"              --> doFloat
  , className =? "Gimp"               --> doFloat
  , className =? "notification"       --> doFloat
  , className =? "pinentry-gtk-2"     --> doFloat
  , className =? "VirtualBox Machine" --> doFloat
  , className =? "splash"             --> doFloat
  , className =? "toolbar"            --> doFloat
  , className =? "volctl"             --> doIgnore
  , className =? "python3"            --> doFloat
  , className =? "cs2"                --> (doShift (myWorkspaces !! 0) <+> doFloat)
  , fmap ("steam_app_" `isPrefixOf`) className   --> (doF (W.greedyView (myWorkspaces !! 0) . W.shift (myWorkspaces !! 0)) <+> doFloat)
  , className =? "pavucontrol"        --> doCenterFloat
  , (className =? "Yad" <&&> fmap ("Cheatsheet" `isInfixOf`) title) --> doRectFloat (W.RationalRect 0.798 0.25 0.2 0.45)
  , className =? "Yad"                --> doCenterFloat 
  , resource =? "crx_nngceckbapebfimnlniiiahkandclblb"   --> doFloat
--  , className =? "zen-browser" <&&> liftM (== Just (500, 495)) minSize --> doCenterFloat
  --, className =? "zen-browser" <&&> title =? "Bitwarden — Zen Browser" --> doCenterFloat

  , title =? "Oracle VM VirtualBox Manager"   --> doFloat
  , title =? "Order Chain - Market Snapshots" --> doFloat
  , title =? "emacs-run-launcher" --> doFloat
  --, className =? "brave"   --> doShift ( myWorkspaces !! 1 )
  , className =? "mpv"             --> doShift ( myWorkspaces !! 7 )
  , className =? "Emacs"             --> doShift ( myWorkspaces !! 0 )
  , className =? "Audacious"             --> doShift ( myWorkspaces !! 6 )
  , className =? "steam"             --> doShift ( myWorkspaces !! 7 )
  , className =? "Steam"             --> doShift ( myWorkspaces !! 7 )
  , className =? "FreeTube" --> doShift  ( myWorkspaces !! 7 )
  , className =? "vmware" --> doShift  ( myWorkspaces !! 4 )
  , className =? "corectrl" --> doShift  ( myWorkspaces !! 8 )
  , className =? "Piper" --> doShift  ( myWorkspaces !! 8 )
  , className =? "tuxedo-control-center" --> doShift  ( myWorkspaces !! 8 )
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , className =? "org.cryptomator.launcher.Cryptomator$MainApp" --> doShift  ( myWorkspaces !! 6 )

  , isFullscreen -->  doFullFloat
  ] <+> namedScratchpadManageHook myScratchPads

soundDir = "/opt/sounds/"

startupSound  = soundDir ++ "startup-01.mp3"
shutdownSound = soundDir ++ "shutdown-02.mp3"
dmenuSound    = soundDir ++ "menu-01.mp3"

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'


showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  --hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()

--------------------------------------------------------------------------------
-- Pinned windows
--
-- One pin at a time. The pinned X window is referenced from the stack of every
-- non-empty workspace (the same trick CopyWindow uses: one window, many stacks).
--
-- Stock xmonad gives such a window neither of the following, so they live in
-- persistent extensible state:
--
--  * pinHome  : the workspace it was pinned from. Unpinning collapses the
--               window back there, wherever you are when you unpin.
--
--  * pinFloat : its float state per workspace (Nothing = tiled).
--               W.floating is ONE global `Map Window RationalRect`, so a
--               window on several stacks is floating (at a single rect) or
--               tiled on all of them at once. We keep one entry per workspace
--               and swap the right one in whenever the current workspace
--               changes. Everything else (position in the stack, focus, the
--               active layout, NBFULL / MIRROR toggles) is already stored per
--               workspace by xmonad, so it needs no help.
--
-- "Full on workspace 2 only": on 2, tile it (M-t) and toggle NBFULL
-- (M-<Space>), or float it full-screen. Neither leaks to other workspaces.
--------------------------------------------------------------------------------

data Pin = Pin
    { pinWin   :: !Window
    , pinHome  :: !WorkspaceId
    , pinFloat :: !(M.Map WorkspaceId (Maybe W.RationalRect))
    } deriving (Show, Read)

data PinState = PinState
    { psPin  :: !(Maybe Pin)
    , psLast :: !(Maybe WorkspaceId)  -- current workspace at the previous pinLogHook
    } deriving (Show, Read)

-- Persistent, so the pin survives M-S-r / M-C-r. The copies always survived a
-- restart (the WindowSet is serialised); the old non-persistent state did not,
-- which left a pinned window that M-C-p ignored and M-S-p re-pinned.
instance ExtensionClass PinState where
    initialValue  = PinState Nothing Nothing
    extensionType = PersistentExtension

getPin :: X (Maybe Pin)
getPin = XS.gets psPin >>= \mp -> case mp of
    Just p -> do
        alive <- gets (W.member (pinWin p) . windowset)
        if alive then pure mp else Nothing <$ putPin Nothing
    Nothing -> pure Nothing

putPin :: Maybe Pin -> X ()
putPin mp = XS.modify $ \s -> s { psPin = mp }

floatOf :: Window -> WindowSet -> Maybe W.RationalRect
floatOf w = M.lookup w . W.floating

setFloat :: Window -> Maybe W.RationalRect -> WindowSet -> WindowSet
setFloat w = maybe (W.sink w) (W.float w)

-- Workspaces currently holding >= 1 window (scratchpad excluded).
pinTargets :: WindowSet -> [WorkspaceId]
pinTargets ss =
    [ W.tag ws | ws <- W.workspaces ss
               , W.tag ws /= scratchpadWorkspaceTag
               , isJust (W.stack ws) ]

-- Pure: put w on `tag` WITHOUT moving that workspace's focus. (CopyWindow's
-- copyWindow makes the copy the focused window, so pinning used to leave
-- keyboard focus on the pin on every workspace.)
--   floating -> first in the stack: floats are restacked in stack order,
--               so it stays above the other floats there;
--   tiled    -> just below the focused window: no master-slot grab, and not
--               at the end of the stack where tall/grid's limitWindows would
--               hide it on a busy workspace.
insertQuiet :: Bool -> Window -> WorkspaceId -> WindowSet -> WindowSet
insertQuiet floating w tag = W.mapWorkspace ins
  where
    ins ws | W.tag ws /= tag = ws
           | otherwise       = ws { W.stack = Just (maybe (W.Stack w [] []) place (W.stack ws)) }
    place s@(W.Stack f up down)
      | w `elem` W.integrate s = s
      | floating               = W.Stack f (up ++ [w]) down
      | otherwise              = W.Stack f up (w : down)

-- Pure: remove w from every workspace except `keep` (keep's stack untouched,
-- so the window keeps its slot there).
dropCopiesExcept :: WorkspaceId -> Window -> WindowSet -> WindowSet
dropCopiesExcept keep w = W.mapWorkspace $ \ws ->
    if W.tag ws == keep then ws else ws { W.stack = W.stack ws >>= W.filter (/= w) }

-- A pin for w, homed on the current workspace. Every target starts out with
-- w's float state at pin time.
mkPin :: Window -> WindowSet -> Pin
mkPin w ss = Pin w (W.currentTag ss) (M.fromList [ (t, floatOf w ss) | t <- pinTargets ss ])

pinPure :: Pin -> WindowSet -> WindowSet
pinPure p ss = foldr (insertQuiet (M.member w (W.floating ss)) w) ss (M.keys (pinFloat p))
  where w = pinWin p

-- Pure: collapse the pin onto `dest` (falls back to home, then to the current
-- workspace) and give it the float state it had on that workspace.
unpinPure :: WorkspaceId -> Pin -> WindowSet -> WindowSet
unpinPure dest p ss = setFloat w st . insertQuiet (isJust st) w tag . dropCopiesExcept tag w $ ss
  where
    w   = pinWin p
    tag = fromMaybe (W.currentTag ss) $ find (`W.tagMember` ss) [dest, pinHome p]
    st  = M.findWithDefault (floatOf w ss) tag (pinFloat p)

-- The float state the pin should have on the current workspace, if the pin is
-- there and xmonad currently holds something else.
pinWanted :: Pin -> WindowSet -> Maybe (Maybe W.RationalRect)
pinWanted p ss = do
    guard (pinWin p `elem` W.index ss)
    st <- M.lookup (W.currentTag ss) (pinFloat p)
    guard (st /= floatOf (pinWin p) ss)
    pure st

applyPin :: Pin -> WindowSet -> WindowSet
applyPin p ss = maybe ss (\st -> setFloat (pinWin p) st ss) (pinWanted p ss)

-- Drop-in for `windows` on anything that changes the current workspace: the
-- float swap is folded into the same pass, so there is a single redraw.
pinWindows :: (WindowSet -> WindowSet) -> X ()
pinWindows f = getPin >>= \mp -> windows (maybe id applyPin mp . f)

-- Runs at the end of every `windows` call.
--  * Same workspace as last run: record the pin's float state for it. This is
--    what catches mouse move/resize, M-t, M-S-t, manage hooks, etc.
--  * Workspace changed by something that bypassed pinWindows (EWMH desktop
--    requests, M-. / M-, screen switching, gridselect, mouse focus onto the
--    other screen): apply the stored state now. Costs one extra redraw; the
--    nested run finds nothing left to change, so the recursion stops there.
pinLogHook :: X ()
pinLogHook = do
    mp   <- getPin
    ss   <- gets windowset
    prev <- XS.gets psLast
    let cur = W.currentTag ss
    XS.modify $ \s -> s { psLast = Just cur }
    whenJust mp $ \p -> case pinWanted p ss of
        Just st | prev /= Just cur -> windows (setFloat (pinWin p) st)
        _ -> when (pinWin p `elem` W.index ss) $
                 putPin (Just p { pinFloat = M.insert cur (floatOf (pinWin p) ss) (pinFloat p) })

pinWindow :: Window -> X ()
pinWindow w = do
    p <- gets (mkPin w . windowset)
    putPin (Just p)
    windows (pinPure p)

-- Unpin, sending the window to `dest pin`.
unpinTo :: (Pin -> WorkspaceId) -> X ()
unpinTo dest = getPin >>= \mp -> whenJust mp $ \p -> do
    putPin Nothing
    windows (unpinPure (dest p) p)

togglePin :: X ()
togglePin = do
    mp <- getPin
    ss <- gets windowset
    case (mp, W.peek ss) of
      (Just p, Just w) | pinWin p == w -> unpinTo pinHome
      (Just old, Just w) -> do                     -- hand the pin over to w
          let unpinOld = unpinPure (pinHome old) old
              new      = mkPin w (unpinOld ss)
          putPin (Just new)
          windows (pinPure new . unpinOld)
      (Just _, Nothing)  -> unpinTo pinHome
      (Nothing, Just w)  -> pinWindow w
      (Nothing, Nothing) -> pure ()

-- M-S-<n>. On the pinned window this means "unpin onto <n>", so M-S-<current>
-- is "unpin here". Plain W.shift on it already collapses every copy onto <n>
-- (W.delete' is global), but the pin state would survive and keep driving a
-- window that is no longer pinned.
shiftPinAware :: WorkspaceId -> X ()
shiftPinAware tag = do
    mp  <- getPin
    foc <- gets (W.peek . windowset)
    case mp of
      Just p | foc == Just (pinWin p) -> unpinTo (const tag)
      _                               -> windows (W.shift tag)

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  --(subtitle "Custom Keys":) $ mkNamedKeymap c $
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
  subKeys "Xmonad Essentials"
  [ ("M-C-r", addName "Recompile XMonad" $ spawn "xmonad --recompile; xmonad --restart")
  , ("M-S-r", addName "Restart XMonad"         $ spawn "xmonad --restart")
  --, ("M-S-q", addName "Quit XMonad"            $ sequence_ [spawn (mySoundPlayer ++ shutdownSound), io exitSuccess])
  --, ("M-S-q", addName "Quit XMonad"            $ io exitSuccess)
  , ("M-S-c", addName "Kill focused window"    $ kill1)
  , ("M-S-q", addName "Quit XMonad"            $ spawn "dm-logout")
  , ("M-S-a", addName "Kill all windows on WS" $ killAll)
  , ("M-S-<Return>", addName "Run prompt"      $ sequence_ [spawn (mySoundPlayer ++ dmenuSound), spawn "PATH='$PATH' dmenu_path | dmenu_run -c -bw 2 -l 20 -g 4"])
  , ("M-S-b", addName "Toggle bar show/hide"   $ sendMessage ToggleStruts)
  , ("M-/", addName "DTOS Help"                $ spawn "dtos-help")
  , ("M-<F2>", addName "Dmenu Helpers"         $ spawn "dm-helpers")]

  ^++^ subKeys "Switch to workspace"
  [ ("M-1", addName "Switch to workspace 1"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 0))
  , ("M-2", addName "Switch to workspace 2"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 1))
  , ("M-3", addName "Switch to workspace 3"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 2))
  , ("M-4", addName "Switch to workspace 4"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 3))
  , ("M-5", addName "Switch to workspace 5"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 4))
  , ("M-6", addName "Switch to workspace 6"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 5))
  , ("M-7", addName "Switch to workspace 7"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 6))
  , ("M-8", addName "Switch to workspace 8"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 7))
  , ("M-9", addName "Switch to workspace 9"    $ (pinWindows $ W.greedyView $ myWorkspaces !! 8))]

  ^++^ subKeys "Cycle non-empty workspaces"
  [ ("M-`",    addName "Next non-empty workspace" $ viewBy Next nonEmptyNonNSP)
  , ("M-S-`",  addName "Prev non-empty workspace" $ viewBy Prev nonEmptyNonNSP)]

  ^++^ subKeys "Pinned / Sticky windows"
  [ ("M-S-p", addName "Pin / unpin focused window (unpin returns it home)" $ togglePin)
  , ("M-C-p", addName "Unpin, send back to the workspace it was pinned on"  $ unpinTo pinHome)]

  ^++^ subKeys "Send window to workspace (pinned window: unpin onto it)"
  [ ("M-S-1", addName "Send to workspace 1"    $ (shiftPinAware $ myWorkspaces !! 0))
  , ("M-S-2", addName "Send to workspace 2"    $ (shiftPinAware $ myWorkspaces !! 1))
  , ("M-S-3", addName "Send to workspace 3"    $ (shiftPinAware $ myWorkspaces !! 2))
  , ("M-S-4", addName "Send to workspace 4"    $ (shiftPinAware $ myWorkspaces !! 3))
  , ("M-S-5", addName "Send to workspace 5"    $ (shiftPinAware $ myWorkspaces !! 4))
  , ("M-S-6", addName "Send to workspace 6"    $ (shiftPinAware $ myWorkspaces !! 5))
  , ("M-S-7", addName "Send to workspace 7"    $ (shiftPinAware $ myWorkspaces !! 6))
  , ("M-S-8", addName "Send to workspace 8"    $ (shiftPinAware $ myWorkspaces !! 7))
  , ("M-S-9", addName "Send to workspace 9"    $ (shiftPinAware $ myWorkspaces !! 8))]

  ^++^ subKeys "Move window to WS and go there"
  [ ("M-S-<Page_Down>", addName "Move window to next WS"   $ shiftAndView Next)
  , ("M-S-<Page_Up>", addName "Move window to prev WS" $ shiftAndView Prev)]

  ^++^ subKeys "Window navigation"
  [ ("M-j", addName "Move focus to next window"                $ windows W.focusDown)
  , ("M-k", addName "Move focus to prev window"                $ windows W.focusUp)
  , ("M-m", addName "Move focus to master window"              $ windows W.focusMaster)
  , ("M-S-j", addName "Swap focused window with next window"   $ windows W.swapDown)
  , ("M-S-k", addName "Swap focused window with prev window"   $ windows W.swapUp)
  , ("M-S-m", addName "Swap focused window with master window" $ windows W.swapMaster)
  , ("M-<Backspace>", addName "Move focused window to master"  $ promote)
  , ("M-S-,", addName "Rotate all windows except master"       $ rotSlavesDown)
  , ("M-S-.", addName "Rotate all windows current stack"       $ rotAllDown)]

  -- Dmenu scripts (dmscripts)
  -- In Xmonad and many tiling window managers, M-p is the default keybinding to
  -- launch dmenu_run, so I've decided to use M-p plus KEY for these dmenu scripts.
  ^++^ subKeys "Dmenu scripts"
  [ ("M-p h", addName "List all dmscripts"     $ spawn "dm-hub")
  , ("M-p a", addName "Choose ambient sound"   $ spawn "dm-sounds")
  , ("M-p b", addName "Set background"         $ spawn "dm-setbg")
  , ("M-p c", addName "Choose color scheme"    $ spawn "~/.local/bin/dtos-colorscheme")
  , ("M-p C", addName "Pick color from scheme" $ spawn "dm-colpick")
  , ("M-p e", addName "Edit config files"      $ spawn "dm-confedit")
  , ("M-p i", addName "Take a screenshot"      $ spawn "dm-maim")
  , ("M-p k", addName "Kill processes"         $ spawn "dm-kill")
  , ("M-p m", addName "View manpages"          $ spawn "dm-man")
  , ("M-p n", addName "Store and copy notes"   $ spawn "dm-note")
  , ("M-p o", addName "Browser bookmarks"      $ spawn "dm-bookman")
  , ("M-p p", addName "Passmenu"               $ spawn "passmenu -p \"Pass: \"")
  , ("M-p q", addName "Logout Menu"            $ spawn "dm-logout")
  , ("M-p r", addName "Listen to online radio" $ spawn "dm-radio")
  , ("M-p s", addName "Search various engines" $ spawn "dm-websearch")
  , ("M-p t", addName "Translate text"         $ spawn "dm-translate")]

  ^++^ subKeys "Favorite programs"
  [ ("M-<Return>", addName "Launch terminal"   $ sequence_ [spawn (mySoundPlayer ++ dmenuSound), spawn (myTerminal ++ " -e fish")])
  , ("M-alt-<Return>", addName "Launch terminal"   $ sequence_ [spawn (mySoundPlayer ++ dmenuSound), spawn (myTerminal ++ " -e fish")])
  , ("M-b", addName "Launch web browser"       $ spawn "zen-browser --new-window https://web.tabliss.io/")
  , ("M-S-l", addName "Bitwarden"  $ spawn  "dbus-run-session -- flatpak run com.bitwarden.desktop &")
  , ("M-d", addName "Launch Doom Emacs"        $ spawn "emacsclient -c -a ''")
  , ("M-M1-h", addName "Launch htop"           $ spawn (myTerminal ++ " -e htop"))
  , ("<Print>", addName "Selection screenshot" $ spawn "maim -s | xclip -selection clipboard -t image/png")
  , ("M-C-v", addName "Primary paste"          $ spawn "xdotool click 1")
  , ("M-v", addName "Selection paste"          $ spawn "$HOME/.local/bin/paste_primary")
  , ("C-M1-t", addName "Secondary Terminal"    $ spawn "alacritty")
  , ("M-f", addName "Freetube"                 $ spawn "freetube")
  , ("M-n", addName "Thunar"                   $ spawn "thunar")]

  ^++^ subKeys "Monitors"
  [ ("M-.", addName "Switch focus to next monitor" $ nextScreen)
  , ("M-,", addName "Switch focus to prev monitor" $ prevScreen)
  , ("M-S-<Tab>", addName "Switch focus to next monitor" $ nextScreen)]

  -- Switch layouts
  ^++^ subKeys "Switch layouts"
  [ ("M-<Escape>", addName "Switch to next layout"   $ sendMessage NextLayout)
--  , ("M-<Space>", addName "Toggle noborders/full" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)]
  , ("M-<Space>", addName "Toggle noborders/full" $ sendMessage (MT.Toggle NBFULL))]
  -- Window resizing
  ^++^ subKeys "Window resizing"
  [ ("M-l", addName "Shrink master window"        $ sendMessage Shrink)
  , ("M-h", addName "Expand master window"        $ sendMessage Expand)
  , ("M-M1-j", addName "Shrink window vertically" $ sendMessage MirrorShrink)
  , ("M-M1-k", addName "Expand window vertically" $ sendMessage MirrorExpand)]

  -- Floating windows
  ^++^ subKeys "Floating windows"
  [ ("M-t", addName "Sink a floating window"     $ withFocused $ windows . W.sink)
--("M-f", addName "Toggle float layout"        $ sendMessage (T.Toggle "floats"))
  , ("M-S-t", addName "Sink all floated windows" $ sinkAll)]

  -- Increase/decrease spacing (gaps)
  ^++^ subKeys "Window spacing (gaps)"
  [ ("C-M-j", addName "Decrease window spacing" $ decWindowSpacing 4)
  , ("C-M-k", addName "Increase window spacing" $ incWindowSpacing 4)
  , ("C-M-h", addName "Decrease screen spacing" $ decScreenSpacing 4)
  , ("C-M-l", addName "Increase screen spacing" $ incScreenSpacing 4)]

  -- Increase/decrease windows in the master pane or the stack
  ^++^ subKeys "Increase/decrease windows in master pane or the stack"
  [ ("M-S-<Up>", addName "Increase clients in master pane"   $ sendMessage (IncMasterN 1))
  , ("M-S-<Down>", addName "Decrease clients in master pane" $ sendMessage (IncMasterN (-1)))
  , ("M-=", addName "Increase max # of windows for layout"   $ increaseLimit)
  , ("M--", addName "Decrease max # of windows for layout"   $ decreaseLimit)]

  -- Sublayouts
  ^++^ subKeys "Some other description"-- This is used to push windows to  ^++^ subKeys "Mocp music player"
  [ ("M-u p", addName "mocp play"                $ spawn "mocp --play")
  , ("M-u l", addName "mocp next"                $ spawn "mocp --next")
  , ("M-u h", addName "mocp prev"                $ spawn "mocp --previous")
  , ("M-u <Space>", addName "mocp toggle pause"  $ spawn "mocp --toggle-pause")]

  ^++^ subKeys "GridSelect"
  -- , ("C-g g", addName "Select favorite apps"     $ runSelectedAction' defaultGSConfig gsCategories)
  [ ("C-M-<Return>", addName "Select favorite apps" $ spawnSelected'
       $ gsGames ++ gsEducation ++ gsInternet ++ gsMultimedia ++ gsOffice ++ gsSettings ++ gsSystem ++ gsUtilities)
  , ("C-M-c", addName "Select favorite apps"    $ spawnSelected' gsCategories)
  , ("C-M-t", addName "Goto selected window"    $ goToSelected $ mygridConfig myColorizer)
  , ("C-M-b", addName "Bring selected window"   $ bringSelected $ mygridConfig myColorizer)
  , ("C-M-1", addName "Menu of games"           $ spawnSelected' gsGames)
  , ("C-M-2", addName "Menu of education apps"  $ spawnSelected' gsEducation)
  , ("C-M-3", addName "Menu of Internet apps"   $ spawnSelected' gsInternet)
  , ("C-M-4", addName "Menu of multimedia apps" $ spawnSelected' gsMultimedia)
  , ("C-M-5", addName "Menu of office apps"     $ spawnSelected' gsOffice)
  , ("C-M-6", addName "Menu of settings apps"   $ spawnSelected' gsSettings)
  , ("C-M-7", addName "Menu of system apps"     $ spawnSelected' gsSystem)
  , ("C-M-8", addName "Menu of utilities apps"  $ spawnSelected' gsUtilities)]

  ^++^ subKeys "Volume control with Function keys"
  [ ("<F6>", addName "Toggle audio mute (all sinks)" $ spawn "pactl list short sinks | awk '{print $1}' | xargs -I {} pactl set-sink-mute {} toggle")
  , ("<F7>", addName "Lower volume (all sinks)"     $ spawn "pactl list short sinks | awk '{print $1}' | xargs -I {} pactl set-sink-volume {} -5%")
  , ("<F8>", addName "Raise volume (all sinks)"     $ spawn "pactl list short sinks | awk '{print $1}' | xargs -I {} pactl set-sink-volume {} +5%")
  ]
  -- Multimedia Keys
  ^++^ subKeys "Multimedia keys"
  [ ("<XF86AudioPlay>", addName "mocp play"           $ spawn "mocp --play")
  , ("<XF86AudioPrev>", addName "mocp next"           $ spawn "mocp --previous")
  , ("<XF86AudioNext>", addName "mocp prev"           $ spawn "mocp --next")
  , ("<XF86AudioMute>", addName "Toggle audio mute"   $ spawn "dinitctl start --pin pipewire && dinitctl start --pin pipewire-pulse && dinitctl start --pin wireplumber && amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", addName "Lower vol" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
  , ("<XF86AudioRaiseVolume>", addName "Raise vol" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
  , ("<XF86MonBrightnessDown>", addName "Decrease brightness" $ spawn "~/.local/bin/brightdown")
  , ("<XF86MonBrightnessUp>",   addName "Increase brightness" $ spawn "~/.local/bin/brightup")
  , ("<XF86HomePage>", addName "Open home page"       $ spawn (myBrowser ++ " https://web.tabliss.io"))
  , ("<XF86Search>", addName "Web search (dmscripts)" $ spawn "dm-websearch")
  , ("<XF86Mail>", addName "Email client"             $ runOrRaise "thunderbird" (resource =? "thunderbird"))
  , ("<XF86Calculator>", addName "Calculator"         $ runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
  , ("<XF86Eject>", addName "Eject /dev/cdrom"        $ spawn "eject /dev/cdrom")
  , ("<XF86TouchpadToggle>", addName "Toggle Touchpad"        $ spawn "$HOME/.local/bin/toggle-touchpad && $HOME/.local/bin/x-settings")

  ]
  -- The following lines are needed for named scratchpads.
    where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
          nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
          -- moveTo / shiftTo bodies, routed through the pin-aware versions
          viewBy dir t     = findWorkspace getSortByIndex dir t 1 >>= pinWindows . W.greedyView
          shiftAndView dir = do t <- findWorkspace getSortByIndex dir nonNSP 1
                                shiftPinAware t
                                pinWindows (W.greedyView t)

main :: IO ()
main = do

 -- Launching three instances of xmobar on their monitors.
  xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/.xmobarrc")
  --xmproc1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/.xmobarrc-secondary")
--  xmproc2 <- spawnPipe ("xmobar -x 2 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $ ewmh $ docks $ def
    { manageHook         = manageSpawn <+> myManageHook <+> manageDocks
    , handleEventHook    = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook
    , modMask            = myModMask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook = do
        -- per-workspace float state of the pinned window (must run on every refresh)
        pinLogHook

        -- your status bar logging
        dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
          { ppOutput = \x -> hPutStrLn xmproc0 x
          , ppCurrent = xmobarColor color06 "" . wrap
              ("<box type=Top width=2 mt=2 color=" ++ color06 ++ ">" ++
               "<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">")
               "</box></box>"
          , ppVisible = xmobarColor color06 "" . wrap
              ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">")
               "</box>" . clickable
          , ppHidden = xmobarColor color05 "" . wrap
                         ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">") "</box>" . clickable
          , ppHiddenNoWindows = xmobarColor color05 ""  . clickable
          , ppTitle   = xmobarColor color16 "" . shorten 60
          , ppSep     = "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
          , ppUrgent  = xmobarColor color02 "" . wrap "!" "!"
          , ppExtras  = [windowCount]
          , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t]
          }

        -- make the pointer follow the newly focused window (and thus the monitor)
        updatePointer (0.5, 0.5) (0, 0)   -- center of focused window
    }
