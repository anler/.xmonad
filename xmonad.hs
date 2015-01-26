import XMonad
import XMonad.Config.Personal

main :: IO ()
main = do
  spawn "feh --bg-scale ~/.xmonad/img/haskell.png"
  xmonad personalConfig
