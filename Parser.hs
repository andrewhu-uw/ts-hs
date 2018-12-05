module Parser where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>),)
import Data.Char (isAlpha,isSpace,)

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

-- e.g. runParser expr "(42)+5*9"
-- yields Just (Binop "+" (Imm 42) (Binop "*" Imm 5 Imm 9))
runParser :: ReadP a -> String -> Maybe a
runParser parser input =
  let res = readP_to_S parser (removeSpaces input)  -- Use the magic function to parse string into list of possible parsing tuples
  in
    if null res || snd (last res) /= "" then Nothing  -- Failure to parse is an empty list, whereas a partial parsing is has a remainder in the snd spot of the tuple
    else Just (fst (last res))

data Expr
  = Binop String Expr Expr
  | Ident [String]
  | Imm Int
  | Call [String] [Expr]
  | Obj [(String, Expr)] -- Object builder syntax
  | InnerString String
  deriving Show

expr :: ReadP Expr
expr = (addsub <|> term <|> obj)

data Stmnt
  = Assign [String] Expr
  | CallStmnt [String] [Expr]
  | Init Stmnt Expr -- Stmnt must be DeclVar or DeclLet
  | DeclVar String [String] -- String list is type name
  | DeclLet String [String]
  deriving Show

stmnt :: ReadP Stmnt
stmnt = (initialization <|> decl <|> assign <|> (call >>= (\(Call name args) -> return (CallStmnt name args))))

initialization :: ReadP Stmnt
initialization = do
  left <- decl
  string "="
  right <- expr
  return (Init left right)

decl :: ReadP Stmnt
decl = do
  declType <- (string "var" <|> string "let")
  name <- munch1 isAlpha
  typeName <- option ["any"] typedecl
  case declType of
    "var" -> return (DeclVar name typeName)
    "let" -> return (DeclLet name typeName)

typedecl :: ReadP [String]
typedecl = do
  char ':'
  sepBy1 (munch1 isAlpha) (char '.')

call :: ReadP Expr
call = do
  nameExpr <- ident
  name <- case extractIdent nameExpr of
    Nothing -> pfail
    Just name -> return name
  argList <- between (char '(') (char ')') (sepBy expr (string ","))
  return (Call name argList)
  
assign :: ReadP Stmnt
assign = do
  leftExpr <- ident
  left <- case extractIdent leftExpr of
    Nothing -> pfail
    Just left -> return left
  string "="
  right <- expr
  return (Assign left right)

type Block = [Line]
data Line
  = SLine Stmnt
  | FLine String [String] Line -- Will always try to parse a block line AKA {...}
  | BLine Block
  deriving Show

block :: ReadP Block
block = sepBy (sline <|> bline <|> fline) (munch1 (\c -> c == '\n' || c == ';'))

fline :: ReadP Line
fline = do
  string "function"
  name <- option "" (munch1 isAlpha)
  params <- paramList
  body <- bline
  return (FLine name params body)

paramList :: ReadP [String]
paramList = between (char '(') (char ')') (sepBy (munch1 isAlpha) (char ','))

bline :: ReadP Line
bline = between (char '{') (char '}') (block >>= (\b -> return (BLine b)))
  
sline :: ReadP Line -- TODO Only let semicolon be optional for some statements
sline = do
  s <- stmnt
  optional (char ';') -- must be qualified b/c of namespace conflict
  return (SLine s)

obj :: ReadP Expr
obj = between (char '{') (char '}') (sepBy jsonPair (char ',')) >>= \obj -> return (Obj obj)

jsonPair :: ReadP (String, Expr)
jsonPair = do
  prop <- munch1 isAlpha
  char ':'
  right <- expr
  return (prop, right)


addsub :: ReadP Expr
addsub = do
  left <- term
  op <- (string "+" <|> string "-")
  right <- expr
  return (Binop op left right)

term :: ReadP Expr
term = (muldiv <|> factor <|> innerString)

innerString :: ReadP Expr
innerString = do
  inner <- between (char '"') (char '"') (munch1 isAlpha) -- TODO add support for single quote strings
  return (InnerString inner)

muldiv :: ReadP Expr
muldiv = do
  left <- factor
  op <- (string "*" <|> string "/")
  right <- term
  return (Binop op left right)

factor :: ReadP Expr
factor = do
  (paren <|> num <|> ident <|> call)

paren :: ReadP Expr
paren = between (char '(') (char ')') expr 

num :: ReadP Expr
num = do
  str <- munch1 (\c -> c >= '0' && c <= '9')
  return (Imm (read str))

ident :: ReadP Expr
ident = do
  nameList <- sepBy1 (munch1 isAlpha) (string ".")
  return (Ident nameList)

extractIdent :: Expr -> Maybe [String]
extractIdent (Ident ss) = Just ss
extractIdent _ = Nothing

removeSpaces :: String -> String  -- Remove spaces, but not newlines
removeSpaces input = case input of
    "" -> ""
    c:"" -> if (isSpace c && c /= '\n') then "" else c:""
    c:cs -> if (isSpace c && c /= '\n') then removeSpaces cs else c : (removeSpaces cs)

