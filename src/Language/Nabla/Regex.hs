module Language.Nabla.Regex where
-- TODO Support unicode, however sbv don't support it.
-- cf. https://github.com/LeventErkok/sbv/issues/365

import Text.Megaparsec (Parsec, parse, some, try, (<|>), sepBy)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.SBV
import Data.SBV.RegExp

type Parser = Parsec Void String

regex exp = parse regExp "" exp

example :: String -> IO SatResult
example exp = case regex exp of
  Right r -> sat $ \s ->  (s :: SString) `match` r
  --Left e ->

regExp :: Parser RegExp
regExp = Conc <$> (some
  $ try star
  <|> try plus
  <|> try opt
  <|> try loop
  <|> atom
  )

star :: Parser RegExp
star = KStar <$> atom <* char '*'

plus :: Parser RegExp
plus = KPlus <$> atom <* char '+'

group :: Parser RegExp
group = Union <$> (char '(' *> sepBy regExp sep <* char ')')
  where
    sep = char '|' *> pure ()

loop :: Parser RegExp
loop = do
  r <- atom
  char '{'
  n <- L.decimal
  char ','
  m <- L.decimal
  char '}'
  return $ Loop n m r

opt :: Parser RegExp
opt = Opt <$> atom <* char '?'

atom :: Parser RegExp
atom = w <|> c <|> group

c :: Parser RegExp
c = exactly <$> (:[]) <$>
  (spaceChar
  <|> upperChar
  <|> lowerChar
  <|> digitChar
  <|> char '!'
  <|> char '"'
  <|> char '#'
  <|> char '$'
  <|> char '%'
  <|> char '&'
  <|> char '\''
  <|> string "\\(" *> pure '('
  <|> string "\\)" *> pure ')'
  <|> string "\\*" *> pure '*'
  <|> string "\\+" *> pure '+'
  <|> char ','
  <|> char '-'
  <|> string "\\." *> pure '.'
  <|> char '/'
  <|> char ':'
  <|> char ';'
  <|> char '<'
  <|> char '='
  <|> char '>'
  <|> string "\\?" *> pure '?'
  <|> char '@'
  <|> string "\\[" *> pure '['
  <|> string "\\\\" *> pure '\\'
  <|> string "\\]" *> pure ']'
  <|> string "\\^" *> pure '^'
  <|> char '_'
  <|> char '`'
  <|> string "\\{" *> pure '{'
  <|> string "\\|" *> pure '|'
  <|> string "\\}" *> pure '}'
  <|> char '~'
  )

w :: Parser RegExp
w = string "\\w" *> pure (asciiLetter + digit)
 <|> string "\\d" *> pure digit
 <|> char '.' *> pure (Range '!' '~')
 <|> char '[' *> (Union <$> some atom) <* char ']'
