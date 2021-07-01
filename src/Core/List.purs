module Core.List where

import Prelude

import Data.List (List(..), concat, drop, length, reverse, (:))
import Data.String.CodeUnits (singleton)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Types (MalExpr(..), MalFn, stringToCharList, toList, toVector)



list :: MalFn
list = pure <<< toList


listQ :: MalExpr -> Boolean
listQ (MalList _ _ ) = true
listQ _              = false


nilQ :: MalExpr -> Boolean
nilQ MalNil = true
nilQ _      = false


emptyQ :: MalExpr -> Boolean
emptyQ (MalList _ Nil)   = true
emptyQ (MalVector _ Nil) = true
emptyQ _                 = false


count :: MalFn
count (MalNil:Nil)           = pure $ MalInt 0
count (MalList _ ex : Nil)   = pure $ MalInt $ length ex
count (MalVector _ ex : Nil) = pure $ MalInt $ length ex
count _                      = throw "non-sequence passed to count"


sequentialQ :: MalExpr -> Boolean
sequentialQ (MalList _ _)   = true
sequentialQ (MalVector _ _) = true
sequentialQ _               = false


cons :: MalFn
cons (x:Nil)                    = pure $ toList $ x:Nil
cons (x : MalList _ xs : Nil)   = pure $ toList $ x:xs
cons (x : MalVector _ xs : Nil) = pure $ toList $ x:xs
cons _                          = throw "illegal call to cons"


concat' :: MalFn
concat' args = toList <<< concat <$> traverse unwrapSeq args
  where

  unwrapSeq :: MalExpr -> Effect (List MalExpr)
  unwrapSeq (MalList _ xs)   = pure xs
  unwrapSeq (MalVector _ xs) = pure xs
  unwrapSeq _                = throw "invalid concat"


nth :: MalFn
nth (MalList _ xs : MalInt n : Nil)   =
  case drop n xs of
    x:_ -> pure x
    Nil -> throw "nth: index out of range"
nth (MalVector _ xs : MalInt n : Nil) =
  case drop n xs of
    x:_ -> pure x
    Nil -> throw "nth: index out of range"
nth _                                 = throw "invalid call to nth"


first :: MalFn
first (MalNil:Nil)              = pure MalNil
first (MalList _ Nil : Nil)     = pure MalNil
first (MalList _ (x:_) : Nil)   = pure x
first (MalVector _ Nil : Nil)   = pure MalNil
first (MalVector _ (x:_) : Nil) = pure x
first _                         = throw "illegal call to first"


rest :: MalFn
rest (MalNil:Nil)               = pure $ toList Nil
rest (MalList _ Nil : Nil)      = pure $ toList Nil
rest (MalList _ (_:xs) : Nil)   = pure $ toList xs
rest (MalVector _ Nil : Nil)    = pure $ toList Nil
rest (MalVector _ (_:xs) : Nil) = pure $ toList xs
rest _                          = throw "illegal call to rest"


apply' :: MalFn
apply' (MalFunction {fn:f} : as) = f =<< concatLast as
  where
  concatLast :: List MalExpr -> Effect (List MalExpr)
  concatLast (MalList _ lst : Nil)   = pure lst
  concatLast (MalVector _ lst : Nil) = pure lst
  concatLast (x:xs)                  = (:) x <$> concatLast xs
  concatLast _                       = throw "last argument of apply must be a sequence"
apply' _ = throw "Illegal call to apply"


map' :: MalFn
map' (MalFunction {fn:f} : MalList _ args : Nil)   = toList <$> traverse (\x -> f (x:Nil)) args
map' (MalFunction {fn:f} : MalVector _ args : Nil) = toList <$> traverse (\x -> f (x:Nil)) args
map' _ = throw "Illegal call to map"


mapQ :: MalExpr -> Boolean
mapQ (MalHashMap _ _) = true
mapQ _                = false


conj' :: MalFn
conj' (MalList _ es : args)   = pure $ toList $ reverse args <> es
conj' (MalVector _ es : args) = pure $ toVector $ es <> args
conj' _                       = throw "illegal arguments to conj"


seq :: MalFn
seq (MalNil:Nil)            = pure MalNil
seq (MalList _ Nil : Nil)   = pure MalNil
seq (MalList _ es : Nil)    = pure $ toList es
seq (MalVector _ Nil : Nil) = pure MalNil
seq (MalVector _ es : Nil)  = pure $ toList es
seq (MalString "" : Nil)    = pure MalNil
seq (MalString s : Nil)     = pure $ toList $ map (MalString <<< singleton) (stringToCharList s)
seq _                       = throw "seq: called on non-sequence"