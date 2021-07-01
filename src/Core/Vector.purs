module Core.Vector  where

import Prelude

import Data.List (List(..), (:))
import Effect.Exception (throw)
import Types (MalExpr(..), MalFn, toVector)



vec :: MalFn
vec (MalList _ xs : Nil)   = pure $ toVector xs
vec (MalVector _ xs : Nil) = pure $ toVector xs
vec Nil                  = throw "vec: arg type"
vec _                    = throw "vec: arg type"


vector :: MalFn
vector = pure <<< toVector


vectorQ :: MalExpr -> Boolean
vectorQ (MalVector _ _) = true
vectorQ _               = false