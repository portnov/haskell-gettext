
module Data.Gettext.Parsers where

import Control.Monad.Identity
import Data.Either
import qualified Data.Text.Lazy as T
import Text.Parsec
import Text.Parsec.Text.Lazy
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr

import Data.Gettext.Plural

type Header = (T.Text, T.Text)
type Headers = [Header]

pHeader :: Parser Header
pHeader = do
  name <- (many1 $ alphaNum <|> char '-') <?> "Header name"
  char ':'
  many $ oneOf " \t"
  value <- many $ noneOf "\r\n"
  return (T.pack name, T.pack value)

pHeaders :: Parser Headers
pHeaders = pHeader `sepEndBy` newline

parseHeaders :: T.Text -> Either String Headers
parseHeaders t =
  case parse pHeaders "<Gettext file headers>" t of
    Left e -> Left (show e)
    Right hs -> Right hs

pSimpleExpr :: Parser Expr
pSimpleExpr = buildExpressionParser table term <?> "simple expression"
  where
    term = parens pExpr <|> (symbol "n" >> return N) <|> (Literal `fmap` natural)

    table = 
      [ [prefix "-" Negate, prefix "!" Not],
        [binary "*" Multiply AssocLeft, binary "/" Divide AssocLeft, binary "%" Mod AssocLeft],
        [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft],
        [binary "==" Equals AssocLeft, binary "!=" NotEquals AssocLeft,
         binary ">" Greater AssocLeft, binary "<=" NotGreater AssocLeft,
         binary "<" Less AssocLeft, binary ">=" NotLess AssocLeft],
        [binary "&&" And AssocLeft, binary "||" Or AssocLeft, binary "^" Xor AssocLeft]
      ]

    binary  name fun assoc = Infix (do{ reservedOp name; return (Binary fun) }) assoc
    prefix  name fun       = Prefix (do{ reservedOp name; return fun })
    -- postfix name fun       = Postfix (do{ reservedOp name; return fun })

pExpr :: Parser Expr
pExpr = do
    expr <- pSimpleExpr
    mbCont <- optionMaybe pTernary
    case mbCont of
      Nothing -> return expr
      Just (true, false) -> return $ If expr true false

pTernary :: Parser (Expr, Expr)
pTernary = do
  reservedOp "?"
  true <- pExpr
  colon
  false <- pExpr
  return (true, false)


pPlural :: Parser (Int, Expr)
pPlural = do
  symbol "nplurals"
  reservedOp "="
  n <- natural
  semi
  symbol "plural"
  reservedOp "="
  expr <- pExpr
  return (n, expr)


cStyle :: GenLanguageDef T.Text () Identity
cStyle = Token.LanguageDef
                { Token.commentStart   = "/*"
                , Token.commentEnd     = "*/"
                , Token.commentLine    = "//"
                , Token.nestedComments = True
                , Token.identStart     = letter
                , Token.identLetter    = alphaNum <|> oneOf "_'"
                , Token.opStart        = Token.opLetter cStyle
                , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.reservedNames  = []
                , Token.reservedOpNames= []
                , Token.caseSensitive  = True
                }

lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser cStyle

natural :: Parser Int
natural = fromIntegral `fmap` Token.natural lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semi :: Parser String
semi = Token.semi lexer

colon :: Parser String
colon = Token.colon lexer

parsePlural :: Headers -> Either String (Int, Expr)
parsePlural headers =
  case lookup (T.pack "Plural-Forms") headers of
    Nothing -> Left $ "Plural-Forms header not found: " ++ show headers
    Just str -> either (Left . show) Right $ parse pPlural "<plural form selection expression>" str

  
