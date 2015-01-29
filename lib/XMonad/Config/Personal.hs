{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Personal (personalConfig) where

import System.Exit
import Data.Maybe (fromMaybe)

import XMonad
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import XMonad.Actions.WindowMenu (windowMenu)
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.Volume

import qualified XMonad.StackSet as W

import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.OneBig
import XMonad.Layout.CenteredMaster
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Util.Loggers (logCmd, date, logCurrent, logLayout, logTitle)

personalConfig = XMonad.defaultConfig { terminal = myTerminal
                                      , modMask = myModMask
                                      , focusFollowsMouse = True
                                      , borderWidth = 4
                                      , normalBorderColor = "black"
                                      , focusedBorderColor = "red"
                                      , workspaces = ["main", "web", "log", "misc"]
                                      , manageHook = myManageHook
                                      , layoutHook = myLayoutHook
                                      , handleEventHook = handleTimerEvent
                                      } `additionalKeysP` myKeys

myFormatVolumeValue :: Double -> String
myFormatVolumeValue = ("Vol:" ++) . (++ "%") . takeWhile (/= '.') . show

myLowerVolume, myRaiseVolume :: X ()
myLowerVolume = lowerVolume 1 >>= (flashText defaultSTConfig 1 . myFormatVolumeValue)
myRaiseVolume = raiseVolume 1 >>= (flashText defaultSTConfig 1 . myFormatVolumeValue)

myPrimaryAction :: String
myPrimaryAction = "emacsclient -c -a ''"

mySecondaryAction :: String
mySecondaryAction = myTerminal

myTerminal :: String
myTerminal = "urxvt"

myModMask :: KeyMask
myModMask = mod4Mask

myFileManager :: String
myFileManager = "nautilus"

myDmenu :: String
myDmenu = "mydmenu"

myPassDmenu :: String
myPassDmenu = "passmenu"

myManageHookFloat :: [String]
myManageHookFloat = ["MPlayer", "Gimp", "Steam", "Skype"]

myManageHookIgnore :: [String]
myManageHookIgnore = ["desktop_window"]

scratchpads = [ NS "quake" "urxvt -title quake" (title =? "quake")
                (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                -- run stardict, find it by class name, place it in the floating window
                -- 1/6 of screen width from the left, 1/6 of screen height
                -- from the top, 2/3 of screen width by 2/3 of screen height
              , NS "stardict" "stardict" (className =? "Stardict")
                (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
              ]
  where role = stringProperty "WM_WINDOW_CLASS"

myManageHook :: ManageHook
myManageHook = composeAll ([className =? x --> doFloat | x <- myManageHookFloat] ++
                           [resource =? x --> doIgnore | x <- myManageHookIgnore])
                <+>
               namedScratchpadManageHook scratchpads

myLayoutHook = toggleLayouts (noBorders Full) resizableTallSpaced |||
               (centerMaster resizableTallSpaced) |||
               Mirror resizableTallSpaced |||
               oneBig |||
               Grid
  where oneBig = OneBig (3/4) (3/4)
        resizableTallSpaced = spacing 5 (ResizableTall nmaster delta ratio [])
        nmaster = 1        -- default number of windows in the master pane
        delta   = 3/100    -- % of the screen to increment when resizing panes
        ratio   = 60/100   -- % of the screen occupied by master pane

myKeys :: [(String, X ())]
myKeys = [ ("M-<Return>", spawnHere myPrimaryAction)
         , ("M-S-<Return>", spawnHere mySecondaryAction)

           -- launchers
         , ("M-s", spawn myDmenu)
         , ("M-S-s", spawn myPassDmenu)
         , ("M-S-o", spawn myFileManager)
         , ("M-b", logCmd "acpi" >>= flashText defaultSTConfig 3 . fromMaybe "")
         , ("M-d", date "%H:%M - %a %b %d" >>= flashText defaultSTConfig 3 . fromMaybe "")
         , ("M-w", logCurrent >>= flashText defaultSTConfig 1 . fromMaybe "")
         , ("M-t", logTitle >>= flashText defaultSTConfig 3 . fromMaybe "")

           -- windows
         , ("M-q", kill)
         , ("M-m", windows W.focusMaster)
         , ("M-S-m", windows W.swapMaster)
         , ("M-p", windows W.focusUp)
         , ("M-S-p", windows W.swapUp)
         , ("M-n", windows W.focusDown)
         , ("M-S-n", windows W.swapDown)
         , ("M-S-t", withFocused (windows . W.sink))
         , ("M-S-j", sendMessage Expand)
         , ("M-S-k", sendMessage Shrink)
         , ("M-g", goToSelected defaultGSConfig)
         , ("M-l", windowMenu)
         , ("M-<Tab>", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP
                       Next HiddenNonEmptyWS 1)
         , ("M-S-<Tab>", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP
                         Prev HiddenNonEmptyWS 1)
         , ("M-.", sendMessage (IncMasterN 1))
         , ("M-,", sendMessage (IncMasterN (-1)))

           -- scratchpad
         , ("M--", namedScratchpadAction scratchpads "stardict")
         , ("M-`", namedScratchpadAction scratchpads "quake")

           -- layouts
         , ("M-<Space>", sendMessage NextLayout >> logLayout >>= flashText defaultSTConfig 1 . fromMaybe "")
         , ("M-S-<Space>", sendMessage FirstLayout >> logLayout >>= flashText defaultSTConfig 1 . fromMaybe "")
         , ("M-S-f", sendMessage (Toggle "Full"))

           -- xmonad
         , ("M-S-e", io (exitWith ExitSuccess))
         , ("M-S-r", restart "xmonad" True)
         , ("M-<Escape>", spawn "xlock")

           -- media
         , ("<XF86AudioLowerVolume>", myLowerVolume)
         , ("M-S-d", myLowerVolume)
         , ("<XF86AudioRaiseVolume>", myRaiseVolume)
         , ("M-S-i", myRaiseVolume)
         , ("<XF86AudioMute>", spawn "amixer -D pulse set Master 1+ toggle")

           -- brightness
         , ("<XF86MonBrightnessUp>", spawn "xbacklight + 10")
         , ("<XF86MonBrightnessDown>", spawn "xbacklight - 10")
         ]
  where
    getSortByIndexNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
