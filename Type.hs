module Colon where

import Control.Monad.Except
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
  | TypeUnknown -- Just for debugging, a valid program should *never* have a variable with type unknown
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

-- type checking needs to be able to (a) evaluate the type of an
-- expression (b) relay an error message when something doesn't type check
-- On success, the type is the type of the evaluated expression.
-- TODO: I really gotta make this a monad
data TCRes'  = TCSuccess' Type | TCFail' String deriving Show
data TCResA a = TCSuccess a | TCFail String deriving Show

-- I want to be able to write this:

-- leftTy <- checkExpr left
-- rightTy <- checkExpr right
-- if leftTy == rightTy then TCSuccess leftTy else TCFail "Types must be equal"

instance Monad TCResA where
  return = TCSuccess
  TCFail reason >>= _ = TCFail reason
  TCSuccess val >>= k = k val

type TCRes = TCResA Type

testError :: TCRes
testError = TCFail "Hey, this is a test error"

checkLeft :: TCRes
checkLeft = TCSuccess TypeString

checkRight :: TCRes
checkRight = TCSuccess TypeNumber

useError :: TCRes
useError = do
  left <- checkLeft
  right <- checkRight
  case (left, right) of
    (TypeAny,_) -> TCFail "Cannot be of type 'any'"
    (TypeString, _) -> TCSuccess TypeString

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
  Imm int -> TCSuccess TypeNumber
  Ident strs -> checkIdent strs env

checkIdent :: [String] -> SymbolTable -> TCRes
checkIdent [] env = TCFail "Compiler error: Identifier was empty"
checkIdent (parent:children) env = case getType parent env of
  Nothing -> TCFail ("Could not find identifier `" ++ parent ++ "`")
  Just parent_t -> case parent_t of
                     TypeClass subenv -> checkIdent children subenv
                     _ -> TCSuccess parent_t

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
           _ -> TCFail $ "Operator (+) could not coerce "++ show left ++" and "++ show right
{-
checkPlus left right env =
  let leftRes = checkExpr left env
      rightRes = checkExpr right env in
    case (leftRes, rightRes) of
      (TCFail reason, _) -> TCFail reason
      (_, TCFail reason) -> TCFail reason
      (TCSuccess leftTy, TCSuccess rightTy) -> 
        if leftTy == rightTy
        then TCSuccess leftTy
        else if leftTy == TypeString || rightTy == TypeString
             then TCSuccess TypeString
             else if leftTy == TypeAny || rightTy == TypeAny
                  then TCSuccess TypeAny
                  else TCFail
-}
checkDecl :: String -> String -> SymbolTable -> TCRes
checkDecl name varType env =
  case getType name env of
    Nothing -> bindVar name varType env
    Just entryType -> if strToType varType env  == entryType then TCSuccess entryType else TCFail "Subsequent variable declarations must have same type" 
  
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

typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



