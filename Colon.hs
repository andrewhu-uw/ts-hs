module Colon where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>),)
import Data.Char (isAlpha,isSpace,)

import Data.HashMap.Strict (HashMap, empty, lookup, insert)
import qualified Data.HashMap.Strict as HM

import Parser

-- Changelog: I *really* need to move this to git
-- Added string assignment functionality
-- Started on type checking, it can check one statement at a time, given the environment hashmap

-- I didn't build a real lexer, so input must be separated by ; or \n
{- Note: Informal grammar notation mixes between BNF and ReadP syntax
Block := sepBy1 (Stmnt <|> Stmnt ; <|> { Block } <|> function [name](params) { Block }) ('\n' <|> ';')
Stmnt := Decl | Init | Ident = Expr | Expr
Init := Decl = Expr
Decl := var Ident [: Type] | let Ident [: Type]
Type := any | number | string | object | Ident
Expr := Term + Expr | Term - Expr | Term | Obj
Obj := { sepBy ([a-Z]+: Expr) ','}
Term := Factor * Term | Factor / Term | Factor | String
String := " [a-Z,0-9,']* " | ' [a-Z,0-9,"] ' -- TODO add \" and \'
Factor := ( Expr ) | Num | Ident | Call
Call := Ident([Expr, Expr, ... Expr])
Ident := sepBy1 [a-Z]+ '.'
Num := [0..9]+
-}

type VarName = [String] -- Identifier is the full path of the variable name
data Type -- Type information
  = TypeString
  | TypeAny
  | TypeObj -- This needs to contain the hashmap from prop names to types
  | TypeUnknown -- Just for debuggingm, a valid program should *never* have a variable with type unknown
  deriving Show
--type SymbolTable = HashMap VarName Type
data TCRes = TCSuccess (HashMap VarName Type) | TCFail String (HashMap VarName Type) deriving Show
-- Automatically creates and accumulates the symbol table while traversing the AST
runCheck :: a -> TCRes
runCheck root = error "Not implemented yet"

checkBlock :: Block -> TCRes
checkBlock block = error "Not implemented yet"


-- TODO: fix discrepencies between namespacing/how types are stored in env/how types are defined
checkStmnt :: Stmnt -> HashMap VarName Type -> TCRes
checkStmnt stmnt env = case stmnt of
  DeclVar name typeList -> let lookupRes = HM.lookup [name] env
                               outerType = last typeList in
                             case lookupRes of
                               Just entryType -> if typeEqual entryType outerType then TCSuccess env else TCFail "Subsequent variable declarations must have same type" env
                               Nothing -> TCSuccess (insert [name] TypeUnknown env) -- TODO Fix the type being entered, it should translate the string into a type name, maybe looking it up in the environment
                             

typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



