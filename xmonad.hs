-- default desktop configuration for Fedora

import System.FilePath (combine)
import System.IO (Handle, hPutStrLn)
import System.Exit
import Data.Monoid
import Text.Printf
import qualified Data.Map as Map

import XMonad
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.Dishes
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.PerWorkspace
import XMonad.Util.Scratchpad
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as StackSet

main :: IO ()
main = do
  rootPath <- getXMonadDir
  xmproc <- spawnPipe $ printf "xmobar %s" (combine rootPath myXmobarrc)
  xmonad  $ defaultConfig { terminal = myTerminal
                          , modMask = myModMask
                          , focusFollowsMouse = myFocusFollowsMouse
                          , borderWidth = myBorderWidth
                          , normalBorderColor = myNormalBorderColor
                          , focusedBorderColor = myFocusedBorderColor
                          , workspaces = myWorkspaceNames

                          , keys = (\conf@(XConfig { XMonad.modMask = modm }) ->
                                     Map.fromList $
                                     (myKeys modm myCommands)
                                     ++
                                     (myWorkspaceCommands conf modm)
                                     ++
                                     (myXineramaCommands conf modm))
                          , logHook = myLogHook xmproc
                          , layoutHook = myLayout
                          , manageHook = manageDocks <+> myManageHook
                          }

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 2

myTerminal :: String
myTerminal = "urxvt"

myEmacsclient :: String
myEmacsclient = "emacsclient -c -a ''"

myDmenu :: String
myDmenu = "dmenu_run -p '->' -b -fn 'DejaVu Sans Mono-14' -nb '#252525' -nf '#bcbcbc' -sb '#f39c12'"

myNormalBorderColor :: String
myNormalBorderColor = "#004358"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ffa72c"

myModMask :: KeyMask
myModMask = mod4Mask

myXmobarrc :: String
myXmobarrc = "xmobar/xmobarrc.hs"

myWorkspaces :: [(String, KeySym)]
myWorkspaces = [ ("main", xK_1)
               , ("web" , xK_2)
               , ("log" , xK_3)
               , ("misc", xK_4) ]

myWorkspaceNames :: [String]
myWorkspaceNames = map fst myWorkspaces

myWorkspaceKeys :: [KeySym]
myWorkspaceKeys = map snd myWorkspaces

myLogHook :: Handle -> X ()
myLogHook x = dynamicLogWithPP $ xmobarPP
  { ppOutput          = hPutStrLn x
  , ppTitle           = xmobarColor "orange" "" . shorten 50
  , ppLayout          = const ""
  , ppSort            = fmap (.scratchpadFilterOutWorkspace) $ ppSort xmobarPP
  , ppCurrent         = wrap "<fc=#f39c12>λ" "</fc>" . id
  , ppVisible         = wrap "<fc=#f39c12>." "</fc>" . id
  , ppHidden          = wrap "<fc=#bcbcbc>.." "</fc>" . id
  , ppHiddenNoWindows = xmobarColor "#bcbcbc" ""
  , ppSep             = " | "
  , ppWsSep           = " " }

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
               [ className =? "MPlayer"        --> doFloat
               , className =? "Gimp"           --> doFloat
               , className =? "Pidgin"         --> doShift misc
               , resource  =? "skype"          --> doShift misc
               , resource  =? "steam"          --> doShift misc
               , resource  =? "desktop_window" --> doIgnore
               , isFullscreen                  --> doFullFloat
               ] <+> myScratchpadManageHook
  where misc = (myWorkspaceNames !! 3)

myScratchpadManageHook :: ManageHook
myScratchpadManageHook = scratchpadManageHook dimensions
  where dimensions = StackSet.RationalRect left top width height
        height = 0.6
        width  = 1
        top    = 0.2
        left   = 0

data KeyMod = None | Plain | Shift deriving (Show)
type Command = (KeyMod, KeySym, X ())
type XMonadCommand = ((KeyMask, KeySym), X ())

commandToKey :: KeyMask -> Command -> XMonadCommand
commandToKey m (Shift, k, c) = ((m .|. shiftMask, k), c)
commandToKey m (Plain, k, c) = ((m, k), c)
commandToKey _ (None, k, c) = ((noModMask, k), c)

myKeys :: KeyMask -> [Command] -> [XMonadCommand]
myKeys m cs = map (commandToKey m) cs

myWorkspaceCommands conf modm =
  [((m .|. modm, k), windows (f i)) | (i, k) <- ws', (f, m) <- ws'']
  where
    ws' = zip (XMonad.workspaces conf) myWorkspaceKeys
    ws'' = [(StackSet.greedyView, 0), (StackSet.shift, shiftMask)]

myXineramaCommands conf modm =
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip ks [0..]
  , (f, m) <- [(StackSet.view, 0), (StackSet.shift, shiftMask)]]
  where
    ks = [xK_6, xK_7, xK_8]

myCommands :: [Command]
myCommands = [ (Plain, xK_Return, spawnHere myEmacsclient)
             , (Shift, xK_Return, spawnHere myTerminal)
             , (Plain, xK_s, spawn myDmenu)

               -- windows
             , (Shift, xK_c, kill)
             , (Plain, xK_grave, scratchpadSpawnActionTerminal myTerminal)
             , (Plain, xK_space, sendMessage NextLayout)
             , (Shift, xK_space, sendMessage ToggleLayout)
             , (Plain, xK_Tab, windows StackSet.focusDown)
             , (Shift, xK_Tab, windows StackSet.focusUp)
             , (Plain, xK_j, windows StackSet.focusDown)
             , (Plain, xK_k, windows StackSet.focusUp)
             , (Shift, xK_j, windows StackSet.swapDown)
             , (Shift, xK_k, windows StackSet.swapUp)
             , (Plain, xK_m, windows StackSet.focusMaster)
             , (Shift, xK_m, windows StackSet.swapMaster)
             , (Plain, xK_h, sendMessage Shrink)
             , (Plain, xK_l, sendMessage Expand)
             , (Plain, xK_t, withFocused (windows . StackSet.sink))
             , (Plain, xK_comma, sendMessage (IncMasterN 1))
             , (Plain, xK_period, sendMessage (IncMasterN (-1)))

               -- xmonad
             , (Shift, xK_e, io (exitWith ExitSuccess))
             , (Shift, xK_r, restart "xmonad" True)
             , (Plain, xK_Escape, spawn "xlock")

               -- media
             , (None, xK_F2, lowerVolume 4 >> return ())
             , (None, xK_F3, raiseVolume 4 >> return ())
             , (Plain, xK_d, lowerVolume 4 >> return ())
             , (Plain, xK_i, raiseVolume 4 >> return ())
             ]

myDefaultLayout = spacing 5 (Tall nmaster delta ratio)
  where nmaster = 1      -- default number of windows in the master pane
        delta   = 3/100  -- % of screen to increment by when resizing panes
        ratio   = 60/100 -- proportion of screen occupied by master pane

myDefaultLayouts =
  toggleLayouts Full (myDefaultLayout ||| Mirror myDefaultLayout ||| tab)
  where tab  = tabbed shrinkText myTabbedConfig

myLogLayout = Dishes nmaster ratio
  where nmaster = 1
        ratio   = 1/5

myLayout = avoidStruts (onWorkspace log myLogLayout $ myDefaultLayouts)
  where
    log  = myWorkspaceNames !! 2

myTabbedConfig = defaultTheme {
  activeColor           = "#0b0b0b"
  , activeTextColor     = "#ffa72c"
  , inactiveColor       = "#0b0b0b"
  , activeBorderColor   = "#0b0b0b"
  , inactiveBorderColor = "#0b0b0b"
  , inactiveTextColor   = "#bcbcbc"
  , fontName            = "xft:DejaVu Sans Mono:pixelsize=13:antialias=true:embolden=true"
  , decoHeight          = 14
}
