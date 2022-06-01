module Language.SomnaScript.Parser where

import           Control.Applicative
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Function
import           Data.Functor
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Scientific
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.SomnaScript.AST


type Parser = P.Parsec Void Text

sc :: Parser ()
sc =
  L.space P.space1 (L.skipLineComment "#") (L.skipBlockCommentNested "#[" "]#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

char :: Char -> Parser Char
char = lexeme . P.char

parens :: Parser a -> Parser a
parens = P.between (char '(') (char ')')

braces :: Parser a -> Parser a
braces = P.between (char '{') (char '}')

identifier :: Parser Text
identifier = lexeme $ P.try do
  t <- T.cons <$> hd <*> tl
  if t `S.member` reserved
    then P.unexpected . P.Tokens . NE.fromList $ "reserved keyword " <> show t
    else return t
 where
  hd = P.letterChar
  tl = P.takeWhileP Nothing isAlphaNum
  reserved =
    S.fromList ["if", "else", "func", "let", "var", "for", "while", "return"]

keyword :: Text -> Parser ()
keyword kw = lexeme . P.try $ P.string kw >> P.notFollowedBy P.alphaNumChar

parseProgram :: Parser [SmnStmt]
parseProgram = many stmt

integerLit :: Parser SmnExpr
integerLit = lexeme $ SEInteger <$> L.signed (return ()) integer
 where
  bin     = P.try (P.string' "0b") >> L.binary
  oct     = P.try (P.string' "0o") >> L.octal
  hex     = P.try (P.string' "0x") >> L.hexadecimal
  integer = bin <|> oct <|> hex <|> L.decimal

floatLit :: Parser SmnExpr
floatLit = lexeme $ SEFloat <$> L.signed (return ()) L.float

stringLit :: Parser SmnExpr
stringLit =
  lexeme
    $   SEText
    .   T.pack
    <$> (P.char '"' >> P.manyTill L.charLiteral (P.char '"'))

charLit :: Parser SmnExpr
charLit =
  lexeme $ SEChar <$> P.between (P.char '\'') (P.char '\'') L.charLiteral

boolLit :: Parser SmnExpr
boolLit = (keyword "true" $> SEBool True) <|> (keyword "false" $> SEBool False)

variable :: Parser SmnExpr
variable = SEVar <$> identifier

operator :: Text -> Parser Text
operator op = lexeme . P.try $ P.string op <* P.notFollowedBy opChars
  where opChars = P.oneOf ['+', '-', '*', '/', '&', '|', '>', '=', '<']

aexpr :: Parser SmnExpr
aexpr =
  P.try floatLit
    <|> integerLit
    <|> stringLit
    <|> charLit
    <|> boolLit
    <|> variable
    <|> parens expr

fexpr :: Parser SmnExpr
fexpr = do
  e     <- aexpr
  calls <- many (normal <|> ufcs)
  return $ foldl (&) e calls
 where
  normal = do
    args <- parens $ P.sepBy expr (char ',')
    return \e -> SECall e args

  ufcs = do
    char '.'
    var  <- variable
    args <- parens $ P.sepBy expr (char ',')
    return \e -> SECall var (e : args)

expr :: Parser SmnExpr
expr = makeExprParser
  fexpr
  [ [infl "*" SOMultiply, infl "/" SODivide]
  , [infl "+" SOPlus, infl "-" SOMinus]
  , [ infn "==" SOEq
    , infn ">"  SOGt
    , infn ">=" SOGe
    , infn "<"  SOLt
    , infn "<=" SOLe
    ]
  , [infl "&&" SOAnd, infl "||" SOOr]
  ]
 where
  infl op node = InfixL $ operator op $> (\e1 e2 -> SEInfix e1 node e2)
  infn op node = InfixN $ operator op $> (\e1 e2 -> SEInfix e1 node e2)

emptyStmt :: Parser SmnStmt
emptyStmt = char ';' $> SSEmpty

exprStmt :: Parser SmnStmt
exprStmt = SSExpr <$> expr <* char ';'

letStmt :: Parser SmnStmt
letStmt = do
  keyword "let"
  ident <- identifier
  operator "="
  e <- expr
  char ';'
  return $ SSLet ident e

varStmt :: Parser SmnStmt
varStmt = do
  keyword "var"
  ident <- identifier
  operator "="
  e <- expr
  char ';'
  return $ SSVar ident e

returnStmt :: Parser SmnStmt
returnStmt = do
  keyword "return"
  e <- expr
  char ';'
  return $ SSReturn e

ifStmt :: Parser SmnStmt
ifStmt = do
  ifc  <- ifClause
  eics <- many elseifClause
  elc  <- elseClause
  return $ SSIfElse (ifc : eics) elc
 where
  ifClause = do
    keyword "if"
    e     <- expr
    stmts <- braces $ many stmt
    return (e, stmts)
  elseifClause = do
    P.try (keyword "else" >> keyword "if")
    e     <- expr
    stmts <- braces $ many stmt
    return (e, stmts)
  elseClause = do
    p <- P.optional $ keyword "else"
    if isJust p then Just <$> braces (many stmt) else return Nothing

funcStmt :: Parser SmnStmt
funcStmt = do
  keyword "func"
  name <- identifier
  args <- parens $ P.sepBy identifier (char ',')
  body <- braces $ many stmt
  return $ SSFunc name args body

stmt :: Parser SmnStmt
stmt =
  emptyStmt
    <|> exprStmt
    <|> letStmt
    <|> varStmt
    <|> returnStmt
    <|> ifStmt
    <|> funcStmt
