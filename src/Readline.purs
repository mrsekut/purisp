module Readline where


import Effect (Effect)


foreign import readLine :: String -> Effect String