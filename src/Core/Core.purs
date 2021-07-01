module Core.Core (ns) where

import Prelude

import Core.Atom as Atom
import Core.Basics as B
import Core.List as List
import Core.String as String
import Core.Vector as Vector
import Core.HashMap as HM
import Data.List (List(..), fromFoldable, (:))
import Data.Tuple (Tuple(..))
import Effect.Exception (throw)
import Types (MalExpr(..), MalFn)



ns :: List (Tuple String MalFn)
ns = fromFoldable
  [ Tuple "throw"      B.throw'

  , Tuple "true?"       $ pred1 B.trueQ
  , Tuple "false?"      $ pred1 B.falseQ

  , Tuple "="           B.eqQ
  , Tuple "+"           $ B.numOp (+)
  , Tuple "-"           $ B.numOp (-)
  , Tuple "*"           $ B.numOp (*)
  , Tuple "/"           $ B.numOp (/)
  , Tuple "<"           $ B.cmpOp (<)
  , Tuple "<="          $ B.cmpOp (<=)
  , Tuple ">"           $ B.cmpOp (>)
  , Tuple ">="          $ B.cmpOp (>=)
  , Tuple "number?"     $ pred1 B.numberQ

  , Tuple "pr-str"      String.prStr
  , Tuple "str"         String.str
  , Tuple "string?"     $ pred1 String.stringQ
  , Tuple "prn"         String.prn
  , Tuple "println"     String.println
  , Tuple "slurp"       String.slurp
  , Tuple "readline"    String.readline'
  , Tuple "read-string" String.readString
  , Tuple "time-ms"     String.timeMs

  , Tuple "symbol?"     $ pred1 B.symbolQ
  , Tuple "symbol"      B.symbol
  , Tuple "keyword?"    $ pred1 B.keywordQ
  , Tuple "keyword"     B.keyword

  , Tuple "list"        List.list
  , Tuple "list?"       $ pred1 List.listQ
  , Tuple "nil?"        $ pred1 List.nilQ
  , Tuple "empty?"      $ pred1 List.emptyQ
  , Tuple "count"       List.count
  , Tuple "sequential?" $ pred1 List.sequentialQ
  , Tuple "cons"        List.cons
  , Tuple "concat"      List.concat'
  , Tuple "nth"         List.nth
  , Tuple "first"       List.first
  , Tuple "rest"        List.rest
  , Tuple "apply"       List.apply'
  , Tuple "map"         List.map'
  , Tuple "map?"        $ pred1 List.mapQ
  , Tuple "conj"        List.conj'
  , Tuple "seq"         List.seq

  , Tuple "vec"         Vector.vec
  , Tuple "vector"      Vector.vector
  , Tuple "vector?"     $ pred1 Vector.vectorQ

  , Tuple "hash-map"    HM.hashMap
  , Tuple "assoc"       HM.assoc
  , Tuple "dissoc"      HM.dissoc
  , Tuple "get"         HM.get
  , Tuple "contains?"   HM.containsQ
  , Tuple "keys"        HM.keys
  , Tuple "vals"        HM.vals

  , Tuple "meta"        B.meta
  , Tuple "with-meta"   B.withMeta

  , Tuple "atom"        Atom.atom
  , Tuple "atom?"       $ pred1 Atom.atomQ
  , Tuple "deref"       Atom.deref
  , Tuple "reset!"      Atom.resetB
  , Tuple "swap!"       Atom.swapB

  , Tuple "macro?"      $ pred1 B.macroQ

  , Tuple "fn?"         $ pred1 B.fnQ
  ]


pred1 :: (MalExpr -> Boolean) -> MalFn
pred1 f (x:Nil) = pure $ MalBoolean $ f x
pred1 _ _       = throw "illegal call to unary predicate"