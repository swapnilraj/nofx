module Parser where

import Prelude(Unit, ($), (<>), (==), pure, bind, discard, unit)

import Control.Alt((<|>))
import Control.Lazy(fix)

import Data.Array(many, some) as Array
import Data.Char.Unicode(isLower)
import Data.List(List(..), foldl, length, some)
import Data.Maybe(Maybe(..), isJust)
import Data.String.CodeUnits as SCU
import Data.Tuple(Tuple(..))

import Text.Parsing.Parser(Parser)
import Text.Parsing.Parser.Combinators((<?>), choice, option, optionMaybe, sepBy, sepBy1, try)
import Text.Parsing.Parser.Language(haskellStyle)
import Text.Parsing.Parser.String(oneOf, satisfy)
import Text.Parsing.Parser.Token(GenLanguageDef(LanguageDef), TokenParser,
alphaNum, makeTokenParser, unGenLanguageDef, upper)

import Syntax

lower :: Parser String Char
lower = satisfy isLower <?> "identifier"

newlines :: Parser String (Array Char)
newlines = Array.some $ satisfy (_ == '\n') <?> "one or more newlines"

lex :: TokenParser
lex = makeTokenParser $ LanguageDef (unGenLanguageDef haskellStyle)
                { identStart = lower <|> oneOf ['_', '\'']
                , reservedOpNames = ["=","\\","->","=>","|"]
                , reservedNames   = [ "let","rec","in","if","then","else"
                                    , "fun","data","type","case", "of"
                                    , "true","false"
                                    ]
                }

var :: Parser String CorePAST
var = do
  name <- lex.identifier
  pure $ Var name

inlineExpr :: Parser String CorePAST
inlineExpr = fix \inline ->
  choice [ lex.parens inline
         , constructor
         , try $ caseE unit
         , try letE
         , ifThen
         , lambda
         , var
         , bool
         , integer
         ]

expr :: Unit -> Parser String CorePAST
expr u = fix \e -> try (app u) <|> inlineExpr

integer :: Parser String CorePAST
integer = do
  i <- lex.integer
  pure $ Lit $ LInt i

bool :: Parser String CorePAST
bool = f <|> t
  where
    t = do lex.reserved "true"
           pure $ Lit $ LBool true
    f = do lex.reserved "false"
           pure $ Lit $ LBool false

ifThen :: Parser String CorePAST
ifThen = do
  lex.reserved "if"
  cond <- expr unit
  lex.reserved "then"
  consequent <- expr unit
  lex.reserved "else"
  alternative <- expr unit
  pure $ If cond consequent alternative

lambda :: Parser String CorePAST
lambda = do
  _ <- lex.symbol "\\"
  arg <- lex.identifier
  lex.reserved "->"
  body <- expr unit
  pure $ Lam arg body

letE :: Parser String CorePAST
letE = do
  lex.reserved "let"
  rec <- optionMaybe (lex.reserved "rec")
  bindings <- (kv `sepBy1` lex.symbol ";")
  lex.reserved "in"
  body <- expr unit
  pure $ Let (isJust rec) bindings body
  where
    kv :: Parser String (Tuple Name CorePAST)
    kv = do
       binder <- lex.identifier
       lex.reservedOp "="
       exprE <- expr unit
       pure $ Tuple binder exprE

constructor :: Parser String CorePAST
constructor = do
  name <- try multiLetterConstructor <|> singleLetterConstructor
  pure $ Constr name
  where
    singleLetterConstructor :: Parser String String
    singleLetterConstructor = do
       name <- lex.lexeme upper
       pure $ SCU.singleton name
    multiLetterConstructor :: Parser String String
    multiLetterConstructor = do
       s <- upper
       tart <- lex.identifier
       pure $ (SCU.singleton s) <> tart

app :: Unit -> Parser String CorePAST
app _ = fix \app' -> do
  e <- inlineExpr
  es <- some $ inlineExpr
  pure $ foldl App e es

caseE :: Unit -> Parser String CorePAST
caseE _ = do
  lex.reserved "case"
  sucritinise <- expr unit
  lex.reserved "of"
  lex.whiteSpace
  alters <- al
  pure $ Case sucritinise alters
  where
    al :: Parser String (Alter String)
    al = (do
       d <- (expr unit)
       lex.reserved "=>"
       t <- expr unit
       pure $ { cons: d, rhs: t })
       `sepBy1` lex.symbol ";"

func :: Parser String CorePSC
func = do
  lex.reserved "fun"
  fname <- lex.identifier
  args <- lex.identifier `sepBy` lex.whiteSpace
  lex.reservedOp "="
  body <- expr unit
  pure $ Func fname args body

supercombinators :: Parser String (List CorePSC)
supercombinators = some func
