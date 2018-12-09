module Parser where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>),)
import Data.Char (isAlpha,isSpace,)

-- I didn't build a real lexer, so input must be separated by ; or \n
{- Note: Informal grammar notation mixes between BNF and ReadP syntax
Block := sepBy1 (Stmnt <|> Stmnt ; <|> { Block } <|> function name (params) { Block } <|> return Expr) ('\n' <|> ';')
Stmnt := Decl | Init | Ident = Expr | if ( Expr ) { Block } [else { Block }]* | Expr | Class
Class := { sepBy (Field | Accessor) ('\n' | ';') }
Field := A method or property or constructor
Accessor := set Ident ( Decl ) { Block } | get Ident ( Decl ) { Block }
Init := Decl = Expr
Decl := var Ident [: Type] | let Ident [: Type]
Type := any | number | string | object | Ident
Expr := Term + Expr | Term - Expr | Term | Obj | Array | function (params) { Block }
Array := [ sepBy Expr ',' ]
Obj := { sepBy ([a-Z]+: Expr) ','}
Term := Factor * Term | Factor / Term | Factor | String
String := " [a-Z,0-9,']* " | ' [a-Z,0-9,"] ' -- TODO add \" and \'
Factor := ( Expr ) | Num | Ident | Call
Call := Ident([Expr, Expr, ... Expr])
Ident := sepBy1 [a-Z]+ '.'
Num := [0..9]+
-}

-- e.g. runParser expr "(42)+5*9" yields Just (Binop "+" (Imm 42) (Binop "*" Imm 5 Imm 9))
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
  | Array [Expr]
  | StringConst String
  deriving Show

expr :: ReadP Expr
expr = (addsub <|> term <|> obj <|> array)

data Stmnt
  = Assign [String] Expr
  | CallStmnt [String] [Expr]
  | Init Stmnt Expr -- Stmnt must be DeclVar or DeclLet
  | DeclVar String String -- Type is only stored as one string for now
  | DeclLet String String
  | IfStmnt Expr Block [(Expr, Block)] (Maybe Block)
  | ClassStmnt Block -- Lines will only be methods, ctor, accessors, or decl/init
  deriving Show

classDef :: ReadP Stmnt
classDef = do
  string "class"
  char '{'
  defs <- classBlock -- TODO: fix this to only accept valid class definitions
  char '}'
  return (ClassStmnt defs)

stmnt :: ReadP Stmnt
stmnt = (initialization <|> decl <|> assign <|> ifstmnt <|> (call >>= (\(Call name args) -> return (CallStmnt name args))) <|> classDef)

ifstmnt :: ReadP Stmnt
ifstmnt = do
  (cond, thenBody) <- partialIf
  elseIfs <- many (string "else" >> partialIf)
  lastElse <- option Nothing (string "else" >> block >>= \b -> return (Just b))
  return (IfStmnt cond thenBody elseIfs lastElse)

partialIf :: ReadP (Expr, Block)
partialIf = do
  string "if"
  cond <- between (char '(') (char ')') expr
  thenBody <- between (char '{') (char '}') block -- TODO allow body without curlies
  return (cond, thenBody)

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
  typeName <- option "any" typedecl
  case declType of
    "var" -> return (DeclVar name typeName)
    "let" -> return (DeclLet name typeName)

typedecl :: ReadP String
typedecl = do
  char ':'
  munch1 isAlpha -- type declarations only support one string types like 'string' and 'number' as of now

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
  | FLine String [(String,String)] String Line -- param list: (name, type)
  | BLine Block
  | RetLine Expr
  | AcsrLine String (Maybe Stmnt) Block -- only has param if it's a setter
  | CtorLine [(Bool, Stmnt)] Block -- List of params: (isPublic, Decl)
  deriving Show

block :: ReadP Block
block = sepBy (sline <|> bline <|> fline <|> retline) (munch1 (\c -> c == '\n' || c == ';'))

retline :: ReadP Line
retline = do
  string "return"
  expr >>= \val -> optional (char ';') >> return (RetLine val)

classBlock :: ReadP Block
classBlock = sepBy
             (method <|> getter <|> setter <|>
                    (initialization <|> decl >>= \stmnt -> return (SLine stmnt))) -- Wrap the Stmnt as a Line
             (munch1 (\c -> c == '\n' || c == ';'))

setter :: ReadP Line
setter = do
  string "set"
  name <- munch1 isAlpha
  param <- between (char '(') (char ')') decl
  body <- block
  return (AcsrLine name (Just param) body)

getter :: ReadP Line
getter = do
  string "get"
  name <- munch1 isAlpha
  char '(' >> char ')' -- TODO when you rework the lexer, check if this needs to be fixed
  body <- block
  return (AcsrLine name Nothing body)

method :: ReadP Line
method = do
  name <- munch1 (isAlpha)
  params <- paramList
  ret <- option "any" typedecl
  body <- bline
  return (FLine name params ret body)

fline :: ReadP Line
fline = do
  string "function"
  name <- munch1 isAlpha
  params <- paramList
  ret <- option "any" typedecl
  body <- bline
  return (FLine name params ret body)

paramList :: ReadP [(String,String)]
paramList = between (char '(') (char ')') (sepBy
                                           -- TODO figure out a way to factor out this monstrosity
                                           (munch1 isAlpha >>= (\name -> (option "any" (char ':' >> (munch1 isAlpha)) >>= (\typeName -> return (name, typeName)))))
                                           (char ','))

bline :: ReadP Line
bline = between (char '{') (char '}') (block >>= (\b -> return (BLine b)))
  
sline :: ReadP Line -- TODO Only let semicolon be optional for some statements
sline = do
  s <- stmnt
  optional (char ';') -- must be qualified b/c of namespace conflict
  return (SLine s)

obj :: ReadP Expr
obj = between (char '{') (char '}') (sepBy jsonPair (char ',')) >>= \obj -> return (Obj obj)

array :: ReadP Expr
array = between (char '[') (char ']') (sepBy expr (char ',')) >>= \list -> return (Array list)

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
term = (muldiv <|> factor <|> stringconst)

stringconst :: ReadP Expr
stringconst = do
  inner <- between (char '"') (char '"') (munch1 isAlpha) -- TODO add support for single quote strings
  return (StringConst inner)

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
    '/':('/':cs) -> removeSpaces (removeUpToNewline cs)
    '/':('*':cs) -> removeSpaces (removeEndComment cs)
    c:"" -> if (isSpace c && c /= '\n') then "" else c:""
    c:cs -> if (isSpace c && c /= '\n') then removeSpaces cs else c : (removeSpaces cs)

removeEndComment :: String -> String
removeEndComment input
  | take 2 input == "*/" = drop 2 input
  | otherwise = removeEndComment (tail input)

removeUpToNewline :: String -> String
removeUpToNewline input
  | head input == '\n' =  input
  | otherwise = removeUpToNewline (tail input)
