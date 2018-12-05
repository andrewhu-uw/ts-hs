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
  | TypeObj -- This needs to contain the hashmap from prop names to types
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


-- TODO: fix discrepencies between namespacing/how types are stored in env/how types are defined
checkStmnt :: Stmnt -> HashMap String Type -> TCRes
checkStmnt stmnt env = case stmnt of
  -- 
  DeclVar name singleType -> let lookupRes = HM.lookup name env in
                             case lookupRes of
                               Nothing -> TCSuccess (insert name TypeUnknown env) -- TODO Fix the type being entered, it should translate the string into a type name, maybe looking it up in the environment
                               Just entryType -> if typeEqual entryType singleType then TCSuccess env else TCFail "Subsequent variable declarations must have same type" env
     
                             

typeEqual :: Type -> String -> Bool
typeEqual t str = case t of
  TypeString -> str == "string"
  _ -> False



