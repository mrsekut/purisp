module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Core.Core as Core
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (message, throw)
import Env as Env
import Eval (eval, evalAst)
import Printer (printStr)
import Reader (readStr)
import Readline (args, readLine)
import Types (MalExpr(..), MalFn, RefEnv, toList)



main :: Effect Unit
main = do
  env <- Env.newEnv Nil
  traverse (setFn env) Core.ns
    *> setFn env (Tuple "eval" $ setEval env)
    *> rep env "(def! not (fn* (a) (if a false true)))"
    *> rep env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
    *> rep env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
    *> case args of
      Nil               -> do
        Env.set env "*ARGV*" $ toList Nil
        -- rep env "(println (str \"Mal [\" *host-language* \"]\"))"
        *> loop env
      script:scriptArgs -> do
        Env.set env "*ARGV*" $ toList $ MalString <$> scriptArgs
        rep env $ "(load-file \"" <> script <> "\")"
        *> pure unit



-- Repl

rep :: RefEnv -> String -> Effect String
rep env str = print =<< evalAst env =<< read str


loop :: RefEnv -> Effect Unit
loop env = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    _    -> do
      result <- try $ rep env line
      case result of
        Right exp -> log exp
        Left err  -> error $ "Error: " <> message err
      loop env


setFn :: RefEnv -> Tuple String MalFn -> Effect Unit
setFn env (Tuple sym f) =
  Env.set env sym $ MalFunction { fn:f, params:Nil, macro:false, meta:MalNil }


setEval :: RefEnv -> MalFn
setEval env (ast:Nil) = eval env ast
setEval _ _           = throw "illegal call of eval"



-- Read

read :: String -> Effect MalExpr
read = readStr



-- Print

print :: MalExpr -> Effect String
print = printStr
