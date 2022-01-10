{-|
Module: P1 
Description: Project 1: A Spreadsheet Application with DeerLang
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}
-- This lists what this module exports. Don't change this!
module P1
  (
    evalDeer,
    computeSpreadsheet
  )
where

-- You *may not* add imports from Data.Map 
import P1Types(Spreadsheet(..), Definition(..), Column(..),
               Expr(..), Value(..),
               Env, exampleSpreadsheet)
import Prelude hiding (lookup)
import qualified Data.Map (lookup, insert, empty, union)

-------------------------------------------------------------------------------
-- Main Functions: 
-- | These are the functions that we will be directly testing.
-- | Do not change the type signatures of these functions.
-------------------------------------------------------------------------------

evalDeer :: Expr -> Env -> Value
evalDeer (Id id) env = case (Data.Map.lookup id env) of
                           Just value -> value
                           Nothing    -> Error
evalDeer (Literal v) env = v
evalDeer (Builtin operation exprs) env = 
  case (operation, (length exprs)) of
    ("+", 2) -> applyfnAriOperation (head exprs) (last exprs) env False (+) 
    ("-", 2) -> applyfnAriOperation (head exprs) (last exprs) env False (-) 
    ("*", 2) -> applyfnAriOperation (head exprs) (last exprs) env False (*) 
    ("/", 2) -> applyfnAriOperation (head exprs) (last exprs) env True (/) 
    (">", 2) -> applyfnNumsCompare (head exprs) (last exprs) env (>)
    ("=", 2) -> applyfnNumsCompare (head exprs) (last exprs) env (==) 
    (">=", 2) -> applyfnNumsCompare (head exprs) (last exprs) env (>=) 
    ("++", 2) -> applyfnString (head exprs) (last exprs) env (++)
    ("!", 1) -> applyfnBool (head exprs) env (not)
    _ -> Error
evalDeer (Lambda param body) env = VClosure param body env
evalDeer (Apply fcnExp args) env = 
  case (evalDeer fcnExp env) of 
    (VClosure param body fcnEnv) -> 
      if (length param) == (length args)
        then
          let argsValues = map (\argExpr -> evalDeer argExpr env) args
              argsValueTuples = zip param argsValues
              newEnv = foldl (\fcnEnv (paramName, argValue) -> Data.Map.insert paramName argValue fcnEnv) fcnEnv argsValueTuples 
          in evalDeer body newEnv
        else Error
    _ -> Error


computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) = 
  let defEnv    = foldl (\env (Def id expr) -> Data.Map.insert id (evalDeer expr env) env) Data.Map.empty defs
      valueCols = filter (\column -> isValueCol column) columns
      dataEnvs  = buildDataEnvs valueCols defEnv
  in map (\column -> case column of
                    (ValCol identifier values) -> column
                    (ComputedCol identifier formula) -> (ValCol identifier (buildValColByFormula formula (map (\env -> Data.Map.union env defEnv) dataEnvs)))) columns


-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------

-- Return an environment with the appropriate identifier-to-value bindings.
getEnvironment:: Definition -> Env
getEnvironment def = undefined

-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.
buildDataEnvs :: [Column] -> Env -> [Env]
buildDataEnvs columns env = foldl (\rowEnv (ValCol id values) -> 
  map (\(value, row) -> Data.Map.insert id value row) (zip values rowEnv)) (take (getNumRows (head columns)) (repeat Data.Map.empty)) columns

--This function takes a formula from a formula column(which is a DeerLang expression) 
--and a list of environments for rows, and returns a list of values.
buildValColByFormula :: Expr -> [Env] -> [Value]
buildValColByFormula formula envs = map (\env -> evalDeer formula env) envs

--Apply basic arithmetic operation to two numbers and return the result
applyfnAriOperation :: Expr -> Expr -> Env -> Bool -> (Float -> Float -> Float) -> Value
applyfnAriOperation e1 e2 env division op =
    case ((evalDeer e1 env), (evalDeer e2 env), division) of
        ((VNum a), (VNum 0.0), True) -> Error 
        ((VNum a), (VNum b), True) -> VNum (a / b)
        ((VNum a), (VNum b), _) -> VNum (op a b)
        _                        -> Error

--Compare two numbers 
applyfnNumsCompare :: Expr -> Expr -> Env -> (Float -> Float -> Bool) -> Value
applyfnNumsCompare e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VNum a), (VNum b)) -> VBool (op a b)
        _                        -> Error

--Concatenate two strings
applyfnString :: Expr -> Expr -> Env -> (String -> String -> String) -> Value
applyfnString e1 e2 env op =
    case ((evalDeer e1 env), (evalDeer e2 env)) of
        ((VStr s1), (VStr s2)) -> VStr (s1 ++ s2)
        _                        -> Error

--Flip a boolean
applyfnBool :: Expr -> Env -> (Bool -> Bool) -> Value
applyfnBool e1 env op =
    case (evalDeer e1 env) of
        (VBool c) -> VBool (op c)
        _                        -> Error

--Return True if the provided column is a value column
isValueCol :: Column -> Bool
isValueCol (ValCol identifier values) = True
isValueCol _ = False

--Return the number of rows in a column
getNumRows :: Column -> Int
getNumRows (ValCol id values) = length values
-------------------------------------------------------------------------------
-- The example from the handout
-------------------------------------------------------------------------------

result = computeSpreadsheet exampleSpreadsheet
