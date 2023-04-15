module Syntax.Parser where

import Error (Pos (..))

import Data.Text (Text)
import Data.Void (Void)

import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.Foldable (Foldable (foldl', foldr'))
import Data.Text qualified as T
import Syntax.Ast qualified as Ast
import Text.Megaparsec (MonadParsec (eof, takeWhile1P, takeWhileP), ParseErrorBundle, Parsec, between, empty, getOffset, option, satisfy, sepBy, sepBy1, some, (<?>), (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text
type ParseError = ParseErrorBundle Text Void

type BareExpr = Ast.Expr ()

spanOf :: Parser a -> Parser Pos
spanOf p = do
  a <- getOffset
  _ <- p
  Pos a <$> getOffset

spanned :: Parser a -> Parser (Pos, a)
spanned p = do
  a <- getOffset
  p' <- p
  b <- getOffset
  return (Pos a b, p')

spanMap :: (Pos -> a -> b) -> Parser a -> Parser b
spanMap f p = uncurry f <$> spanned p

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

consP :: Parser Char -> Parser Text -> Parser Text
consP hd tl = T.cons <$> hd <*> tl

betweenS :: Text -> Text -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

symbolP :: Parser Text
symbolP = consP (satisfy isLower) (takeWhileP (Just "symbol") isAlphaNum) <?> "symbol"

typenameP :: Parser Text
typenameP = consP (satisfy isUpper) (takeWhileP (Just "symbol") isAlphaNum) <?> "typename"

symbolicP :: Parser Text
symbolicP = symbolP <|> typenameP

thenMap :: Parser b -> (b -> a -> b) -> Parser a -> Parser b
thenMap p con p' = do
  e <- p
  (con e <$> p')
    <|> return e

tupleP :: (Pos -> [a] -> a) -> Parser a -> Parser a
tupleP con p = do
  pos <- spanOf $ symbol "("
  sepBy p (symbol ",") <* symbol ")" >>= \case
    [a] -> return a
    xs -> return $ con pos xs

accessP :: (a -> b -> a) -> Parser a -> Parser b -> Parser a
accessP con pa pb = do
  a <- lexeme pa
  b <- option [] $ symbol "." *> sepBy1 (lexeme pb) (symbol ".")
  return $ foldl' con a b

uncurry2 :: (a -> b -> c -> d) -> a -> (b, c) -> d
uncurry2 f a (b, c) = f a b c

qualifiedName :: Parser Text -> Parser Text
qualifiedName p = T.intercalate "." <$> sepBy1 p (symbol ".")

typeT :: Parser Ast.Type
typeT = arrow
 where
  base = lexeme $ spanMap Ast.TSymbol (qualifiedName typenameP)
  app = thenMap base Ast.TApp (betweenS "[" "]" (sepBy typeT $ symbol ","))
  tup = tupleP Ast.TTuple typeT <|> app
  arrow = thenMap tup Ast.TArrow (symbol "->" *> tup)

patternUnannotatedP :: Parser Ast.Pattern
patternUnannotatedP = tup <|> app <|> var
 where
  var = lexeme $ spanMap Ast.PVar symbolP
  tup = tupleP Ast.PTup patternP
  app = do
    (sp, name) <- lexeme $ spanned typenameP
    let con = Ast.PApp sp name
    (con <$> betweenS "(" ")" (sepBy1 patternP $ symbol ","))
      <|> return (con [])

patternP :: Parser Ast.Pattern
patternP = ann
 where
  ann = thenMap patternUnannotatedP Ast.PAnn (symbol ":" *> typeT)

atomE :: Parser BareExpr
atomE = call
 where
  base =
    tupleP (Ast.Tuple ()) exprE
      <|> spanMap (Ast.Symbol ()) (qualifiedName symbolicP)
      <|> spanMap (Ast.Symbol ()) symbolP
      <|> spanMap (Ast.Numeric ()) (takeWhile1P (Just "numeric") isDigit)

  -- moduleAccess = uncurry Ast.ModuleAccess <$> spanned typenameP <*> option [] (symbol "." *> sepBy1 (spanned typenameP) (symbol "."))
  call = thenMap (lexeme base) (Ast.Call ()) (betweenS "(" ")" (sepBy exprE $ symbol ","))

type Stmt = BareExpr -> BareExpr

defSignature :: Parser a -> Parser (Pos, Text, [Ast.Constraint], [a], Ast.Type)
defSignature p =
  symbol "def" *> do
    (sp, name) <- lexeme (spanned symbolP)
    cs <- option [] constraints
    params <- betweenS "(" ")" (sepBy p $ symbol ",") <* symbol ":"
    retTy <- typeT
    return (sp, name, cs, params, retTy)

constraint :: Parser Ast.Constraint
constraint = spanMap Ast.CVar typenameP

constraints :: Parser [Ast.Constraint]
constraints = betweenS "[" "]" (sepBy1 constraint $ symbol ",")

defS :: Parser Stmt
defS = do
  (sp, name, cs, params, retTy) <- defSignature term <* symbol "="
  Ast.Def () sp name cs params retTy <$> exprE
 where
  term = (,) <$> (lexeme patternUnannotatedP <* symbol ":") <*> typeT

openS :: Parser Stmt
openS =
  uncurry (Ast.Open ())
    <$> (symbol "open" *> (lexeme . spanned) (qualifiedName typenameP))

openE :: Parser BareExpr
openE = openS <*> (symbol "in" *> exprE)

constructorP :: Parser (Ast.Constructor ())
constructorP = do
  (sp, name) <- lexeme (spanned typenameP)
  let con = Ast.Constructor () sp name
  (con <$> betweenS "(" ")" (sepBy1 typeT (symbol ",")))
    <|> return (con [])

typeS :: Parser Stmt
typeS =
  symbol "type" *> do
    (sp, name) <- lexeme (spanned typenameP)
    cs <- option [] constraints <* symbol "="
    Ast.Type () sp name cs <$> sepBy1 constructorP (symbol "|")

branchB :: Parser (Ast.Branch ())
branchB = Ast.Branch <$> patternP <*> (symbol "=>" *> exprE)

matchE :: Parser BareExpr
matchE = do
    sp <- spanOf (symbol "match")
    e <- exprE
    Ast.Match () sp e <$> betweenS "{" "}" (sepBy branchB $ symbol ",")

moduleStmts :: Parser BareExpr
moduleStmts = foldr' ($) (Ast.Unit ()) <$> some stmts
 where
  stmts = defS <|> typeS <|> moduleS <|> openS

moduleS :: Parser Stmt
moduleS =
  symbol "module" *> do
    (sp, name) <- lexeme (spanned typenameP)
    Ast.Module () sp name <$> betweenS "{" "}" moduleStmts

exprE :: Parser BareExpr
exprE = openE <|> matchE <|> atomE

program :: Parser BareExpr
program = moduleStmts <* eof