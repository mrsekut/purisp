module Core.HashMap where

import Prelude

import Data.List (List(..), foldM, (:))
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Printer (keyValuePairs)
import Types (Key(..), MalExpr(..), MalFn, keyToString, toHashMap, toList)



hashMap :: MalFn
hashMap kvs =
  case keyValuePairs kvs of
    Just pairs -> pure $ toHashMap $ Map.fromFoldable pairs
    Nothing    -> throw "invalid call to hash-map"


assoc :: MalFn
assoc (MalHashMap _ hm : kvs) =
    case keyValuePairs kvs of
        Just pairs -> pure $ toHashMap $ Map.union (Map.fromFoldable pairs) hm
        Nothing    -> throw "invalid assoc"
assoc _                       = throw "invalid call to assoc"


dissoc :: MalFn
dissoc (MalHashMap _ hm : ks) = toHashMap <$> foldM remover hm ks
  where
  remover :: Map.Map Key MalExpr -> MalExpr -> Effect (Map.Map Key MalExpr)
  remover m (MalKeyword k) = pure $ Map.delete (KeywordKey k) m
  remover m (MalString k)  = pure $ Map.delete (StringKey k) m
  remover _ _              = throw "invalid dissoc"
dissoc _                      = throw "invalid call to dissoc"


get :: MalFn
get (MalHashMap _ hm : MalString k : Nil)  =
  pure case Map.lookup (StringKey k) hm of
    Just mv -> mv
    Nothing -> MalNil
get (MalHashMap _ hm : MalKeyword k : Nil) =
  pure case Map.lookup (KeywordKey k) hm of
    Just mv -> mv
    Nothing -> MalNil
get (MalNil : MalString _ : Nil)           = pure MalNil
get _                                      = throw "invalid call to get"


containsQ :: MalFn
containsQ (MalHashMap _ hm : MalString k : Nil)  = pure $ MalBoolean $ Map.member (StringKey k) hm
containsQ (MalHashMap _ hm : MalKeyword k : Nil) = pure $ MalBoolean $ Map.member (KeywordKey k) hm
containsQ (MalNil : MalString _ : Nil)           = pure $ MalBoolean false
containsQ _                                      = throw "invalid call to contains?"


keys :: MalFn
keys (MalHashMap _ hm : Nil) = pure $ toList $ keyToString <$> Map.keys hm
keys _                     = throw "invalid call to keys"


vals :: MalFn
vals (MalHashMap _ hm : Nil) = pure $ toList $ Map.values hm
vals _                       = throw "invalid call to vals"