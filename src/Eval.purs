module Eval (eval) where

import Prelude

import Data.List (List(..), foldM, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Env as Env
import Types (MalExpr(..), MalFn, RefEnv, foldrM, toHashMap, toList, toVector)



eval :: RefEnv -> MalExpr -> Effect MalExpr
eval _ ast@(MalList _ Nil) = pure ast
eval env (MalList _ ast)   = case ast of
  MalSymbol "def!" : es              -> evalDef env es
  MalSymbol "let*" : es              -> evalLet env es
  MalSymbol "if" : es                -> evalIf env es >>= eval env
  MalSymbol "do" : es                -> evalDo env es
  MalSymbol "fn*" : es               -> evalFnMatch env es
  MalSymbol "quote" : es             -> evalQuote env es
  MalSymbol "quasiquote" : es        -> evalQuasiquote env es
  MalSymbol "quasiquoteexpand" : es  -> evalQuasiquoteexpand es
  _                     -> do
    es <- traverse (evalAst env) ast
    case es of
      MalFunction {fn:f} : args -> f args
      _                         -> throw "invalid function"
eval env ast               = evalAst env ast


evalAst :: RefEnv -> MalExpr -> Effect MalExpr
evalAst env (MalSymbol s)       = do
  result <- Env.get env s
  case result of
    Just k  -> pure k
    Nothing -> throw $ "'" <> s <> "'" <> " not found"
evalAst env ast@(MalList _ _)   = eval env ast
evalAst env (MalVector _ envs)  = toVector <$> traverse (eval env) envs
evalAst env (MalHashMap _ envs) = toHashMap <$> traverse (eval env) envs
evalAst _ ast                   = pure ast



-- Def

evalDef :: RefEnv -> List MalExpr -> Effect MalExpr
evalDef env (MalSymbol v : e : Nil) = do
  evd <- evalAst env e
  Env.set env v evd
  pure evd
evalDef _ _                         = throw "invalid def!"



-- Let

evalLet :: RefEnv -> List MalExpr -> Effect MalExpr
evalLet env (MalList _ ps : e : Nil)   = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  evalAst letEnv e
evalLet env (MalVector _ ps : e : Nil) = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  evalAst letEnv e
evalLet _ _                            = throw "invalid let*"


letBind :: RefEnv -> List MalExpr -> Effect Unit
letBind _ Nil                       = pure unit
letBind env (MalSymbol ky : e : es) = do
  Env.set env ky =<< evalAst env e
  letBind env es
letBind _ _                         = throw "invalid let*"



-- If

evalIf :: RefEnv -> List MalExpr -> Effect MalExpr
evalIf env (b:t:e:Nil) = do
  cond <- evalAst env b
  pure case cond of
    MalNil           -> e
    MalBoolean false -> e
    _                -> t
evalIf env (b:t:Nil)   = do
  cond <- evalAst env b
  pure case cond of
    MalNil           -> MalNil
    MalBoolean false -> MalNil
    _                -> t
evalIf _ _             = throw "invalid if"



-- Do

evalDo :: RefEnv -> List MalExpr -> Effect MalExpr
evalDo env es    = foldM (const $ evalAst env) MalNil es



-- Fn

evalFnMatch :: RefEnv -> List MalExpr -> Effect MalExpr
evalFnMatch env (MalList _ params : body : Nil)   = evalFn env params body
evalFnMatch env (MalVector _ params : body : Nil) = evalFn env params body
evalFnMatch _ _                                   = throw "invalid fn*"


evalFn :: RefEnv -> List MalExpr -> MalExpr -> Effect MalExpr
evalFn env params body = do
  paramsStr <- traverse unwrapSymbol params
  pure $ MalFunction { fn     : fn paramsStr body
                     , params : paramsStr
                     , macro  : false
                     , meta   : MalNil
                     }
  where

  fn :: List String -> MalExpr -> MalFn
  fn params' body' = \args -> do
    fnEnv <- Env.newEnv env
    ok <- Env.sets fnEnv params' args
    if ok
      then evalAst fnEnv body'
      else throw "actual parameters do not match signature "

  unwrapSymbol :: MalExpr -> Effect String
  unwrapSymbol (MalSymbol s) = pure s
  unwrapSymbol _             = throw "fn* parameter must be symbols"



-- Quote

evalQuote :: RefEnv -> List MalExpr -> Effect MalExpr
evalQuote _ (e:Nil) = pure e
evalQuote _ _       = throw "invalid quote"


evalQuasiquote :: RefEnv -> List MalExpr -> Effect MalExpr
evalQuasiquote env (e:Nil) = evalAst env =<< quasiquote e
evalQuasiquote  _ _        = throw "invalid quasiquote"


evalQuasiquoteexpand :: List MalExpr -> Effect MalExpr
evalQuasiquoteexpand (e:Nil) = quasiquote e
evalQuasiquoteexpand _       = throw "invalid quasiquote"


quasiquote :: MalExpr -> Effect MalExpr
quasiquote (MalList _ (MalSymbol "unquote" : x : Nil)) = pure x
quasiquote (MalList _ (MalSymbol "unquote" : _))       = throw "invalid unquote"
quasiquote (MalList _ xs)                              = foldrM qqIter (toList Nil) xs
quasiquote (MalVector _ xs)                            = do
  lst <- foldrM qqIter (toList Nil) xs
  pure $ toList $ MalSymbol "vec" : lst : Nil
quasiquote ast@(MalHashMap _ _)                        = pure $ toList $ MalSymbol "quote" : ast : Nil
quasiquote ast@(MalSymbol _)                           = pure $ toList $ MalSymbol "quote" : ast : Nil
quasiquote ast                                         = pure ast


qqIter :: MalExpr -> MalExpr -> Effect MalExpr
qqIter (MalList _ (MalSymbol "splice-unquote" : x : Nil)) acc = pure $ toList $ MalSymbol "concat" : x : acc : Nil
qqIter (MalList _ (MalSymbol "splice-unquote" : _)) _         = throw "invalid splice-unquote"
qqIter elt acc                                                = do
  qqted <- quasiquote elt
  pure $ toList $ MalSymbol "cons" : qqted : acc : Nil