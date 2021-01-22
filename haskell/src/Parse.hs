module Parse (parse, parseFile, typed, parseLam, parseApp) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Term (Sort (..), Term (..), apps, lams, pis)
import Text.Parsec (spaces, (<|>))
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

-- Helper parsers
str :: String -> Parser ()
str = void . Parsec.string

chr :: Char -> Parser ()
chr = void . Parsec.char

space :: Parser ()
space = void $ Parsec.many1 (Parsec.char ' ')

nl :: Parser ()
nl = void $ Parsec.many1 (Parsec.char '\n')

paren :: Parser a -> Parser a
paren = Parsec.between (Parsec.char '(') (Parsec.char ')')

-- Important parseApp is last, as it interprets any spaces as an application
parseT :: Parser Term
parseT = Parsec.choice $ Parsec.try <$> [S <$> parseSort, parseLam, parsePi, parseVar, parseApp]

-- Parse a typed variable name, e.g. "(x: Int)"
typed :: Parser (String, Term)
typed = paren f
  where
    f = do
      name <- Parsec.many1 Parsec.alphaNum
      Parsec.char ':' >> spaces
      term <- parseT
      return (name, term)

-- "a b c" -> App (App a b) c
parseApp :: Parser Term
parseApp = apps <$> paren (Parsec.sepBy1 parseT (Parsec.char ' '))

-- Parse multiargument pi type
parsePi :: Parser Term
parsePi = do
  chr '∀'
  args <- map (first pack) <$> Parsec.many1 (Parsec.try (space >> typed))
  Parsec.char ',' >> space
  pis args <$> parseT

-- Parse multiargument lam type
parseLam :: Parser Term
parseLam = do
  chr 'λ'
  args <- map (first pack) <$> Parsec.many1 (Parsec.try (space >> typed))
  spaces >> str "=>" >> space
  lams args <$> parseT

parseSort :: Parser Sort
parseSort =
  (Parsec.string "Prop" >>= const (pure Prop))
    <|> (Parsec.string "Set" >>= const (pure Set))
    <|> do
      str "Type"
      i <- Parsec.many Parsec.digit
      return $ Type (if null i then 0 else read i :: Int)

parseVar :: Parser Term
parseVar = do
  x <- Parsec.many1 Parsec.alphaNum
  return $ Var $ pack x

parseDef :: Parser (Text, Term)
parseDef = do
  str "def "
  name <- Parsec.many1 Parsec.alphaNum
  str " = "
  t <- parseT
  nl
  return (pack name, t)

-- High level parsers
parse :: String -> Either Parsec.ParseError Term
parse = Parsec.parse parseT "src"

parseFile :: FilePath -> IO [(Text, Term)]
parseFile fp = do
  contents <- readFile fp
  return $ case Parsec.parse (Parsec.many1 parseDef) fp contents of
    Left e -> error (show e)
    Right x -> x