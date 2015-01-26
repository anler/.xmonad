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
import XMonad.StackSet (focusDown, focusUp, focusMaster, swapMaster, swapUp, swapDown, sink)

import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.OneBig
import XMonad.Layout.CenteredMaster
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig
import XMonad.Util.Loggers (logCmd, date, logCurrent, logLayout, logTitle)

personalConfig = XMonad.defaultConfig { terminal = myTerminal
                                      , modMask = myModMask
                                      , focusFollowsMouse = True
                                      , borderWidth = 4
                                      , normalBorderColor = "black"
                                      , focusedBorderColor = "white"
                                      , workspaces = ["main", "web", "log", "misc"]
                                      , manageHook = myManageHook
                                      , layoutHook = myLayoutHook
                                      , handleEventHook = handleTimerEvent
                                      } `additionalKeysP` myKeys

myPrimaryAction :: String
myPrimaryAction = "emacsclient -c -a ''"

mySecondaryAction :: String
mySecondaryAction = myTerminal

myTerminal :: String
myTerminal = "gnome-terminal --hide-menubar"

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

myManageHook :: ManageHook
myManageHook = composeAll ([className =? x --> doFloat | x <- myManageHookFloat] ++
                           [resource =? x --> doIgnore | x <- myManageHookIgnore])

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
         , ("M-S-n", spawn myFileManager)
         , ("M-b", logCmd "acpi" >>= flashText defaultSTConfig 3 . fromMaybe "")
         , ("M-d", date "%a %b %d" >>= flashText defaultSTConfig 1 . fromMaybe "")
         , ("M-w", logCurrent >>= flashText defaultSTConfig 1 . fromMaybe "")
         , ("M-t", logTitle >>= flashText defaultSTConfig 3 . fromMaybe "")

           -- windows
         , ("M-q", kill)
         , ("M-m", windows focusMaster)
         , ("M-S-m", windows swapMaster)
         , ("M-n", windows focusDown)
         , ("M-p", windows focusUp)
         , ("M-S-n", windows swapDown)
         , ("M-S-p", windows swapUp)
         , ("M-S-t", withFocused (windows . sink))
         , ("M-S-j", sendMessage Expand)
         , ("M-S-k", sendMessage Shrink)
         , ("M-g", goToSelected defaultGSConfig)
         , ("M-l", windowMenu)
         , ("M-<Tab>", nextWS)
         , ("M-S-<Tab>", prevWS)
         , ("M-.", sendMessage (IncMasterN 1))
         , ("M-,", sendMessage (IncMasterN (-1)))

           -- layouts
         , ("M-<Space>", sendMessage NextLayout)
         , ("M-S-f", sendMessage (Toggle "Full"))

           -- xmonad
         , ("M-S-e", io (exitWith ExitSuccess))
         , ("M-S-r", restart "xmonad" True)
         , ("M-<Escape>", spawn "xlock")

           -- media
         , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 1-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 1+ unmute")
         , ("<XF86AudioMute>", spawn "amixer -D pulse set Master 1+ toggle")

           -- brightness
         , ("<XF86MonBrightnessUp>", spawn "xbacklight + 10")
         , ("<XF86MonBrightnessDown>", spawn "xbacklight - 10")
         ]
