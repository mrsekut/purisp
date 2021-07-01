module Core.Basics  where

import Prelude

import Data.List (List(..), (:))
import Data.String (take)
import Effect.Exception (throw)
import Printer (printStrReadably)
import Types (MalExpr(..), MalFn, Meta(..))



-- General

eqQ :: MalFn
eqQ (a:b:Nil) = pure $ MalBoolean $ a == b
eqQ _         = throw "illegal arguments to ="



-- Error/Exception

throw' :: MalFn
throw' (e:Nil) = throw =<< printStrReadably e
throw' _       = throw "illegal arguments to throw"



-- Boolean

trueQ :: MalExpr -> Boolean
trueQ (MalBoolean true) = true
trueQ _                 = false


falseQ :: MalExpr -> Boolean
falseQ (MalBoolean false) = true
falseQ _                  = false



-- Numeric

numOp :: (Int -> Int -> Int) -> MalFn
numOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op n1 n2
numOp _ _                                  = throw "invalid operator"


cmpOp :: (Int -> Int -> Boolean) -> MalFn
cmpOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalBoolean $ op n1 n2
cmpOp _ _                                  = throw "invalid operator"


numberQ :: MalExpr -> Boolean
numberQ (MalInt _) = true
numberQ _          = false



-- Scalar

symbolQ :: MalExpr -> Boolean
symbolQ (MalSymbol _) = true
symbolQ _             = false


symbol :: MalFn
symbol (MalString s : Nil) = pure $ MalSymbol s
symbol _                   = throw "symbol called with non-string"


keywordQ :: MalExpr -> Boolean
keywordQ (MalKeyword s) = take 1 s == ":"
keywordQ _              = false


keyword :: MalFn
keyword (kw@(MalString s) : Nil) | take 1 s == ":" = pure kw
keyword (MalString s : Nil)  = pure $ MalKeyword (":" <> s)
keyword (kw@(MalKeyword s) : Nil) | take 1 s == ":" = pure kw
keyword (MalKeyword s : Nil) = pure $ MalKeyword (":" <> s)
keyword _                    = throw "keyword called with non-string"



-- Metadata

meta :: MalFn
meta (MalList (Meta m) _ : Nil)    = pure m
meta (MalVector (Meta m) _ : Nil)  = pure m
meta (MalHashMap (Meta m) _ : Nil) = pure m
meta (MalAtom (Meta m) _ : Nil)    = pure m
meta (MalFunction {meta:m} : Nil)  = pure m
meta _                             = throw "invalid meta call"


withMeta :: MalFn
withMeta (MalList _ es : m : Nil)    = pure $ MalList (Meta m) es
withMeta (MalVector _ es : m : Nil)  = pure $ MalVector (Meta m) es
withMeta (MalHashMap _ es : m : Nil) = pure $ MalHashMap (Meta m) es
withMeta (MalAtom _ es : m : Nil)    = pure $ MalAtom (Meta m) es
withMeta ((MalFunction f) : m : Nil) = pure $ MalFunction $ f {meta = m}
withMeta _                           = throw "invalid with-meta call"



-- Macro

macroQ :: MalExpr -> Boolean
macroQ (MalFunction {macro:true}) = true
macroQ _                          = false



-- Function

fnQ :: MalExpr -> Boolean
fnQ (MalFunction {macro:false}) = true
fnQ _                           = false



-- Utils

pred1 :: (MalExpr -> Boolean) -> MalFn
pred1 f (x:Nil) = pure $ MalBoolean $ f x
pred1 _ _       = throw "illegal call to unary predicate"