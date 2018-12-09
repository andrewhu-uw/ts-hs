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
  | TypeClass SymbolTable -- env is the derived class fields, parent is the base class fields
  | TypeUnknown -- Just for debuggingm, a valid program should *never* have a variable with type unknown
  | TypeModule -- This also needs to contain a map
  deriving Show

-- SymbolTable needs to map (a) local ids to types, (b) higher scope ids to types, (c) class names to their symbol tables
data SymbolTable = SymbolTable (HashMap String Type) (Maybe SymbolTable) (HashMap String SymbolTable) deriving Show
initSymbolTable = SymbolTable empty Nothing empty

getType :: String -> SymbolTable -> Maybe Type
getType varName (SymbolTable env parent classes) =
  case HM.lookup varName env of
    Just bind -> Just bind
    Nothing -> case parent of
                 Nothing -> Nothing
                 Just parent -> getType varName parent

insert :: String -> Type -> SymbolTable -> SymbolTable
insert name defType (SymbolTable env parent classes) = SymbolTable (HM.insert name defType env) parent classes

data TCRes = TCSuccess SymbolTable | TCFail String SymbolTable deriving Show
-- Automatically creates and accumulates the symbol table while traversing the AST
runCheck :: a -> TCRes
runCheck root = error "Not implemented yet"

checkBlock :: Block -> TCRes
checkBlock block = error "Not implemented yet"

checkStmnt :: Stmnt -> SymbolTable -> TCRes
checkStmnt stmnt env = case stmnt of
  DeclVar name varType -> checkDecl name varType env
  DeclLet name varType -> checkDecl name varType env
  Init decl val -> checkInit decl val env

-- TODO: Check that the type of the expression matches the declared type
checkInit :: Stmnt -> Expr -> SymbolTable -> TCRes
checkInit decl val env = case decl of
  DeclVar name varType -> checkDecl name varType env
  DeclLet name varType -> checkDecl name varType env
  _ -> TCFail "Invalid declaration in an init statement" env

checkDecl :: String -> String -> SymbolTable -> TCRes
checkDecl name varType env = case getType name env of
                                  Nothing -> bindVar name varType env
                                  Just entryType -> if typeEqual entryType varType then TCSuccess env else TCFail "Subsequent variable declarations must have same type" env
  
bindVar :: String -> String -> SymbolTable -> TCRes
bindVar name varType env = case castToType varType env of
                                TypeUnknown -> TCFail ( "Could not find type " ++ varType ++ " in scope") env
                                typeListing -> TCSuccess (insert name typeListing env)

castToType :: String -> SymbolTable -> Type
castToType varType env = case varType of
  "string" -> TypeString
  "number" -> TypeNumber
  "any" -> TypeAny
  _ -> TypeUnknown -- for now assume this is a class, but it could be an array, using regexes here would be better



typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



