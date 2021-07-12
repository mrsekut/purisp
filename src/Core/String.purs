module Core.String where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (round)
import Data.List (List(..), (:))
import Data.String (take)
import Data.Time.Duration (Milliseconds(..), toDuration)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Now (now)
import Reader (readStr)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Printer (printList, printListReadably)
import Readline (readLine)
import Types (MalExpr(..), MalFn)



prStr :: MalFn
prStr a = liftEffect $ MalString <$> printList a


str :: MalFn
str a = liftEffect $ MalString <$> printListReadably "" a


stringQ :: MalExpr -> Boolean
stringQ (MalString "") = true
stringQ (MalString s)  = take 1 s /= ":"
stringQ _              = false


prn :: MalFn
prn args = liftEffect $ do
  log =<< printList args
  pure MalNil


println :: MalFn
println args = liftEffect $ do
  log =<< printListReadably " " args
  pure MalNil


slurp :: MalFn
slurp (MalString path : Nil) = MalString <$> liftEffect (readTextFile UTF8 path)
slurp _                      = throw "invalid arguments to slurp"


readline' :: MalFn
readline' (MalString prompt : Nil) = MalString <$> readLine prompt
readline' _                        = throw "invalid arguments to readline"


readString :: MalFn
readString (MalString s : Nil) = readStr s
readString _                   = throw "invalid read-string"


-- FIXME: BigInt
timeMs :: MalFn
timeMs Nil = do
  n <- now
  pure $ MalInt $ round $ (unwap <<< toDuration <<< unInstant) n - 1624173000000.0
  where

  unwap :: Milliseconds -> Number
  unwap (Milliseconds n) = n

timeMs _ = throw "invalid time-ms"
