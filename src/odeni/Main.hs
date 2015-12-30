{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.List (foldl')
import Data.Monoid
import qualified Data.Text.Lazy as L

import System.Console.Repline

import qualified Oden.Syntax as Syntax
import Oden.Core.Untyped
import Oden.Identifier
import Oden.Infer
import Oden.Parser
import Oden.Pretty
import Oden.Eval
import qualified Oden.Env as Env
import Oden.Predefined

data REPLState = REPLState { tyctx      :: Env.Env  -- Type environment
                           , tmctx      :: TermEnv  -- Value environment
                           , currentId  :: Int
                           }

initState :: REPLState
initState = REPLState predefined emptyTmenv 0

type REPL a = HaskelineT (StateT REPLState IO) a

hoistErr :: Show e => Either e a -> REPL a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

nextName :: REPL Name
nextName = do
  modify' (\st -> st { currentId = currentId st + 1 })
  n <- gets currentId
  return $ "$" ++ show n

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
  where (val, tmctx') = runEval env nm ex

exec :: Bool -> L.Text -> REPL ()
exec update source = do
  name <- nextName
  let i = Unqualified name

  -- Get the current interpreter state
  st <- get

  -- Parser ( returns untyped syntax )
  untyped <- hoistErr $ Syntax.explodeExpr <$> parseExpr source

  -- Type Inference ( returns typing environment )
  (sig, _) <- hoistErr $ inferExpr (tyctx st) untyped

  let (value, tmctx') = runEval (tmctx st) name untyped

   -- Create the new environment
  let st' = st { tmctx = tmctx st <> tmctx'
               , tyctx = tyctx st `Env.extends` [(i, sig)]
               }

  -- Update the interpreter state
  when update (put st')

  showSignature i
  showValue value

showSignature :: Identifier -> REPL ()
showSignature name = do
  st <- get
  case Env.lookup name (tyctx st)  of
    Just val -> liftIO $ do
      putStr "\ESC[1;35m\STX"
      putStr $ ppsignature (name, val)
      putStrLn "\ESC[0m\STX"
    Nothing -> return ()

showValue :: Value -> REPL ()
showValue val = liftIO $ do
  putStr "\ESC[1m\STX"
  putStr $ show val
  putStrLn "\ESC[0m\STX"

cmd :: String -> REPL ()
cmd source = exec True (L.pack source)

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
    --, (":type"  , values)
  ]

comp :: (Monad m, MonadState REPLState m) => WordCompleter m
comp _ =
  return []

welcome :: REPL ()
welcome = liftIO $ putStrLn "Welcome to the Oden REPL!"

help :: [String] -> REPL ()
help _ = liftIO $ putStrLn $ "Commands available: " ++ show (map fst options)

load :: [String] -> REPL ()
load _ = liftIO $ putStrLn "Not implemented yet."

options :: [(String, [String] -> REPL ())]
options = [
    ("help", help)  -- :help
  , ("load", load)  -- :load
  ]

completer :: CompleterStyle (StateT REPLState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

main :: IO ()
main = evalStateT (evalRepl "\ESC[1;32m\STXoden>\ESC[0m\STX " cmd options completer welcome) initState

