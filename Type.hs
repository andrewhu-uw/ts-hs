module Colon where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>),)
import Data.Char (isAlpha,isSpace,)

import Data.HashMap.Strict (HashMap, empty,)
import qualified Data.HashMap.Strict as HM

import Parser

data Type -- Type information
  = TypeString
  | TypeNumber
  | TypeAny
  | TypeObj -- Only for temp objects. This needs to contain the hashmap from prop names to types
  | TypeClass -- Also should contain a map
  | TypeUnknown -- Just for debuggingm, a valid program should *never* have a variable with type unknown
  | TypeModule -- This also needs to contain a map
  deriving Show
data SymbolTable = SymbolTable (HashMap String Type) (Maybe SymbolTable) deriving Show
getType :: String -> SymbolTable -> Maybe Type
getType varName (SymbolTable env parent) = case HM.lookup varName env of
                                             Just bind -> Just bind
                                             Nothing -> case parent of
                                               Nothing -> Nothing
                                               Just parent -> getType varName parent

insert :: String -> Type -> SymbolTable -> SymbolTable
insert name defType (SymbolTable env parent) = SymbolTable (HM.insert name defType env) parent

data TCRes = TCSuccess SymbolTable | TCFail String SymbolTable deriving Show
-- Automatically creates and accumulates the symbol table while traversing the AST
runCheck :: a -> TCRes
runCheck root = error "Not implemented yet"

checkBlock :: Block -> TCRes
checkBlock block = error "Not implemented yet"

checkStmnt :: Stmnt -> SymbolTable -> TCRes
checkStmnt stmnt env = case stmnt of
  DeclVar name singleType -> checkDecl name singleType env
  DeclLet name singleType -> checkDecl name singleType env
  Init decl val -> checkInit decl val env

-- TODO: Check that the type of the expression matches the declared type
checkInit :: Stmnt -> Expr -> SymbolTable -> TCRes
checkInit decl val env = case decl of
  DeclVar name singleType -> checkDecl name singleType env
  DeclLet name singleType -> checkDecl name singleType env
  _ -> TCFail "Invalid declaration in an init statement" env

checkDecl :: String -> String -> SymbolTable -> TCRes
checkDecl name singleType env = case getType name env of
                                  Nothing -> bindVar name singleType env -- TODO Fix the type being entered, it should translate the string into a type name, maybe looking it up in the environment
                                  Just entryType -> if typeEqual entryType singleType then TCSuccess env else TCFail "Subsequent variable declarations must have same type" env
  
bindVar :: String -> String -> SymbolTable -> TCRes
bindVar name singleType env = case castToType singleType env of
                                TypeUnknown -> TCFail ( "Could not find type " ++ singleType ++ " in scope") env
                                typeListing -> TCSuccess (insert name typeListing env)

castToType :: String -> SymbolTable -> Type
castToType str env
  | str == "string" = TypeString
  | str == "number" = TypeNumber
  | str == "any" = TypeAny
  | otherwise = TypeUnknown

typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



