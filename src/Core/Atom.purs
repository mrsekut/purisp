module Core.Atom  where

import Prelude

import Data.List (List(..), (:))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Types (MalExpr(..), MalFn, toAtom)



atom :: MalFn
atom (v:Nil) = toAtom <$> liftEffect (Ref.new v)
atom _       = throw "invalid atom call"


atomQ :: MalExpr -> Boolean
atomQ (MalAtom _ _) = true
atomQ _             = false


deref :: MalFn
deref (MalAtom _ ref : Nil) = liftEffect $ Ref.read ref
deref _                   = throw "invalid deref call"


resetB :: MalFn
resetB (MalAtom _ ref : val : Nil) = liftEffect $ Ref.write val ref *> pure val
resetB _                         = throw "invalid reset!"


swapB :: MalFn
swapB (MalAtom _ ref : MalFunction {fn:f} : args) = do
  val <- liftEffect $ Ref.read ref
  newVal <- f $ val:args
  liftEffect $ Ref.write newVal ref
  pure newVal
swapB _                                           = throw "Illegal swap!"