module Main (
    main
  ) where

import Lib (introMessage)

main :: IO ()
main =
  putStrLn introMessage
