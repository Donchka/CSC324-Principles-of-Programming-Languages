{-|
Module: P1Types
Description: Types for Project 1

This module provides the public types required for Project 1.
You should review the data types carefully, but do not change anything in
this file! We will use a fresh copy of this file for testing purposes.
-}
module P1Types
    ( Spreadsheet(..), Definition(..), Column(..), Expr(..), Value(..),
      Env, exampleSpreadsheet
    )
where

-- An environment is a mapping of a string identifier to a value
import Data.Map
type Env =  Map String Value

-- A spreadsheet consists of a list of definitions and a list
-- of columns
data Spreadsheet = Spreadsheet [Definition] [Column]
                 deriving (Show, Eq)

-- A definition consists of a string identifier, and a DeerLang expression
-- to be evaluated
data Definition = Def String Expr
                deriving (Show, Eq)

-- A column can be either a
--     * ValCol with an identifier and raw data
--     * ComputedCol with an identifier and a DeerLang expression
data Column = ValCol String [Value]
            | ComputedCol String Expr
            deriving (Show, Eq)
            
-- DeerLang Expressions
data Expr = Literal Value
          | Id String
          | Builtin String [Expr]
          | Lambda [String] Expr
          | Apply Expr [Expr]
          deriving (Show, Eq)

-- Values
data Value = VNum  Float
           | VBool Bool
           | VStr  String
           | VClosure [String] Expr Env
           | Error          -- Use this value liberally, whenever there is an error
           deriving (Show, Eq)


exampleSpreadsheet :: Spreadsheet
exampleSpreadsheet  =
    Spreadsheet [Def "voting-age" (Literal $ VNum 18),
                 Def "concat" (Lambda ["x", "y"]
                                (Builtin "++" [Id "x", Id "y"])),
                 Def "canvote" (Lambda ["x"] 
                                (Builtin ">=" [Id "x", Id "voting-age"]))]
                [ValCol "id" [VNum 1, VNum 2, VNum 3, VNum 4, VNum 5],
                 ValCol "name" [VStr "adam",
                                VStr "betty",
                                VStr "clare",
                                VStr "eric",
                                VStr "sam"],
                 ValCol "age" [VNum 12, VNum 15, VNum 18, VNum 49, VNum 17],
                 ComputedCol "voter" (Apply (Id "canvote") [Id "age"]),
                 ComputedCol "name2" (Apply (Id "concat") [Id "name", Id "name"]),
                 ComputedCol "year-until-100" (Builtin "-" [Literal $ VNum 100,
                                                            Id "age"])]
