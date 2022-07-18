module Language.Experiment.Regex where
-- TODO Support unicode, however sbv don't support it.
-- cf. https://github.com/LeventErkok/sbv/issues/365

import Text.Megaparsec (Parsec, parse, some, try, (<|>), sepBy)
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Functor
import Data.SBV
import Data.SBV.RegExp

type Parser = Parsec Void String

regex :: String -> Either (ParseErrorBundle String Void) RegExp
regex = parse regExp ""

regExp :: Parser RegExp
regExp = Conc <$> some (try star
  <|> try plus
  <|> try opt
  <|> try loop
  <|> atom)

star :: Parser RegExp
star = KStar <$> atom <* char '*'

plus :: Parser RegExp
plus = KPlus <$> atom <* char '+'

group :: Parser RegExp
group = Union <$> (char '(' *> sepBy regExp sep <* char ')')
  where
    sep = char '|' $> ()

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
c = exactly . (: []) <$>
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
  <|> string "\\(" $> '('
  <|> string "\\)" $> ')'
  <|> string "\\*" $> '*'
  <|> string "\\+" $> '+'
  <|> char ','
  <|> char '-'
  <|> string "\\." $> '.'
  <|> char '/'
  <|> char ':'
  <|> char ';'
  <|> char '<'
  <|> char '='
  <|> char '>'
  <|> string "\\?" $> '?'
  <|> char '@'
  <|> string "\\[" $> '['
  <|> string "\\\\" $> '\\'
  <|> string "\\]" $> ']'
  <|> string "\\^" $> '^'
  <|> char '_'
  <|> char '`'
  <|> string "\\{" $> '{'
  <|> string "\\|" $> '|'
  <|> string "\\}" $> '}'
  <|> char '~'
  )

w :: Parser RegExp
w = string "\\w" $> (asciiLetter + digit)
 <|> string "\\d" $> digit
 <|> char '.' $> Range '!' '~'
 <|> char '[' *> (Union <$> some atom) <* char ']'
