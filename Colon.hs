module Colon where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>),)
import Data.Char (isAlpha,isSpace,)

import Data.HashMap.Strict (HashMap, empty, lookup, insert)
import qualified Data.HashMap.Strict as HM

import Parser

data Type -- Type information
  = TypeString
  | TypeAny
  | TypeObj -- Only for temp objects. This needs to contain the hashmap from prop names to types
  | TypeClass -- Also should contain a map
  | TypeUnknown -- Just for debuggingm, a valid program should *never* have a variable with type unknown
  | TypeModule -- This also needs to contain a map
  deriving Show
--type SymbolTable = HashMap VarName Type
data TCRes = TCSuccess (HashMap String Type) | TCFail String (HashMap String Type) deriving Show
-- Automatically creates and accumulates the symbol table while traversing the AST
runCheck :: a -> TCRes
runCheck root = error "Not implemented yet"

checkBlock :: Block -> TCRes
checkBlock block = error "Not implemented yet"

checkStmnt :: Stmnt -> HashMap String Type -> TCRes
checkStmnt stmnt env = case stmnt of
  DeclVar name singleType -> checkDecl name singleType env
  DeclLet name singleType -> checkDecl name singleType env
  Init decl val -> checkInit decl val env

-- TODO: Check that the type of the expression matches the declared type
checkInit :: Stmnt -> Expr -> HashMap String Type -> TCRes
checkInit decl val env = case decl of
  DeclVar name singleType -> checkDecl name singleType env
  DeclLet name singleType -> checkDecl name singleType env
  _ -> TCFail "Invalid declaration in an init statement" env

checkDecl :: String -> String -> HashMap String Type -> TCRes
checkDecl name singleType env = case HM.lookup name env of
                                  Nothing -> TCSuccess (insert name TypeUnknown env) -- TODO Fix the type being entered, it should translate the string into a type name, maybe looking it up in the environment
                                  Just entryType -> if typeEqual entryType singleType then TCSuccess env else TCFail "Subsequent variable declarations must have same type" env
                             

typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



