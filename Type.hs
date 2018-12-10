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
  | TypeModule -- This also needs to contain a map
  | TypeUnknown -- Just for debuggingm, a valid program should *never* have a variable with type unknown
  deriving (Show, Eq)

-- SymbolTable needs to map (a) local ids to types, (b) higher scope ids to types, (c) class names to their symbol tables
data SymbolTable = SymbolTable (HashMap String Type) (Maybe SymbolTable) (HashMap String Type) deriving (Show, Eq)
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

-- type checking needs to be able to (a) evaluate the type of a
-- expression (b) relay an error message when something doesn't type check
-- and (c) update the symbol tables
-- On success, the type is the type of the evaluated expression. TBH, not sure why we need the symbol table if it fails
-- TODO: I really gotta make this a monad
data TCRes = TCSuccess SymbolTable Type | TCFail String SymbolTable deriving Show
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

-- Do the type of this expression and the goal match?
checkExpr :: Expr -> SymbolTable -> TCRes
checkExpr e env = case e of
  Binop op left right -> checkBinop op left right env
  Imm int -> TCSuccess env TypeNumber

checkBinop :: String -> Expr -> Expr -> SymbolTable -> TCRes
checkBinop op left right env =
  case op of
    "+" -> checkPlus left right env

checkPlus :: Expr -> Expr -> SymbolTable -> TCRes
checkPlus left right env =
  let leftRes = checkExpr left env
      rightRes = checkExpr right env in
    case (leftRes, rightRes) of
      (TCFail reason env, _) -> TCFail reason env
      (_, TCFail reason env) -> TCFail reason env
      (TCSuccess _ leftType, TCSuccess _ rightType) -> 
        if leftType == rightType && leftType == TypeNumber
        then TCSuccess env TypeNumber
        else TCFail "operator (+) cannot be used on expressions of different types or types that are not [number, string]" env

checkDecl :: String -> String -> SymbolTable -> TCRes
checkDecl name varType env =
  case getType name env of
    Nothing -> bindVar name varType env
    Just entryType -> if strToType varType env  == entryType then TCSuccess env entryType else TCFail "Subsequent variable declarations must have same type" env
  
bindVar :: String -> String -> SymbolTable -> TCRes
bindVar name varTypeString env = case strToType varTypeString env of
                                TypeUnknown -> TCFail ( "Could not find type " ++ varTypeString ++ " in scope") env
                                varType -> TCSuccess (insert name varType env) varType

strToType :: String -> SymbolTable -> Type
strToType varType env = case varType of
  "string" -> TypeString
  "number" -> TypeNumber
  "any" -> TypeAny
  _ -> TypeUnknown -- for now assume this is a class, but it could be an array, using regexes here would be better



typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



