module Type where

import Control.Monad.Except hiding (Maybe)
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
  | TypeBool
  | TypeObj -- Only for temp objects. This needs to contain the hashmap from prop names to types
  | TypeClass String SymbolTable -- env is the derived class fields, parent is the base class fields
  | TypeModule -- This also needs to contain a map
  | TypeUnknown -- Just for debugging, a valid program should *never* have a variable with type unknown
  deriving (Show, Eq)

-- SymbolTable needs to map (a) local ids to types, (b) higher scope ids to types, (c) class names to their symbol tables
data SymbolTable = SymbolTable (HashMap String Type) (Maybe SymbolTable) deriving (Show, Eq)
initSymbolTable = SymbolTable empty Nothing 

getType :: String -> SymbolTable -> TCRes
getType varName (SymbolTable env parent) = let (id, fields) = splitAtDot varName in
  case HM.lookup id env of
    Just bind -> case bind of
                   TypeClass className fieldEnv -> case getType fields fieldEnv of
                                                TCFail reason -> fail $ "Could not find type of field `"++id++"` in class `"++className++"`"
                                                x -> x
                   _ -> return bind
    Nothing -> case parent of
                 Nothing -> fail $ "Could not find type of identifier `"++id++"`"
                 Just parent -> getType varName parent

insert :: String -> Type -> SymbolTable -> SymbolTable
insert name defType (SymbolTable env parent) = SymbolTable (HM.insert name defType env) parent

-- type checking needs to be able to (a) evaluate the type of an
-- expression (b) relay an error message when something doesn't type check
-- On success, the type is the type of the evaluated expression.
data TCResA a = TCSuccess a | TCFail String deriving Show

instance Monad TCResA where
  return = TCSuccess
  TCFail reason >>= _ = TCFail reason
  TCSuccess val >>= k = k val
  fail reason = TCFail reason

type TCRes = TCResA Type

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
  _ -> TCFail "Invalid declaration in an init statement" 

-- Do the type of this expression and the goal match?
checkExpr :: Expr -> SymbolTable -> TCRes
checkExpr e env = case e of
  Binop op left right -> checkBinop op left right env
  Imm _ -> return TypeNumber
  Boolean _ -> return TypeBool
  Ident strs -> checkIdent strs env

checkIdent :: [String] -> SymbolTable -> TCRes
checkIdent [] env = TCFail "Compiler error: Identifier was empty"
checkIdent (parent:children) env = do
  parentTy <- getType parent env
  case parentTy of
    TypeClass className subenv -> checkIdent children subenv
    _ -> return parentTy

checkBinop :: String -> Expr -> Expr -> SymbolTable -> TCRes
checkBinop "+" left right env = checkPlus left right env

checkPlus :: Expr -> Expr -> SymbolTable -> TCRes
checkPlus leftexp rightexp env = do
  left <- checkExpr leftexp env
  right <- checkExpr rightexp env
  if left == right
    then return left
    else case (left, right) of
           (TypeAny, _) -> return TypeAny
           (_, TypeAny) -> return TypeAny
           (TypeString, TypeNumber) -> return TypeString
           (TypeNumber, TypeString) -> return TypeString
           _ -> fail $ "Operator (+) could not coerce "++ show left ++" and "++ show right

checkDecl :: String -> String -> SymbolTable -> TCRes
checkDecl name varType env =
  case getType name env of
    TCFail _ -> bindVar name varType env
    TCSuccess entryType -> if strToType varType env == entryType
                           then return entryType
                           else fail "Subsequent variable declarations must have same type" 
  
bindVar :: String -> String -> SymbolTable -> TCRes
bindVar name varTypeString env = case strToType varTypeString env of
                                TypeUnknown -> TCFail ( "Could not find type " ++ varTypeString ++ " in scope") 
                                varType -> TCSuccess varType

strToType :: String -> SymbolTable -> Type
strToType varType env = case varType of
  "string" -> TypeString
  "number" -> TypeNumber
  "any" -> TypeAny
  _ -> TypeUnknown -- for now assume this is a class, but it could be an array, using regexes here would be better

splitAtDot :: String -> (String,String)
splitAtDot "" = ([],[])
splitAtDot ('.':cs) = ([],cs)
splitAtDot (c:cs) = (c:bf,af) where (bf,af) = splitAtDot cs


