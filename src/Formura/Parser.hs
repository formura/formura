{-|
Module      : Formura.Parser
Description : parser combinator
Copyright   : (c) Takayuki Muranushi, 2015
License     : MIT
Maintainer  : muranushi@gmail.com
Stability   : experimental

This module contains combinator for writing Formura parser, and also the parsers for Formura syntax.
-}

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, ImplicitParams, TypeFamilies, TypeOperators #-}
module Formura.Parser where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char (isSpace, isLetter, isAlphaNum, isPrint, toUpper)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import System.IO.Unsafe
import Text.Trifecta hiding (ident)
import Text.Trifecta.Delta
import qualified Text.Parser.Expression as X
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr

import Text.Parser.LookAhead

import Formura.Utilities (readYamlDef)
import Formura.CommandLineOption
import Formura.Language.Combinator
import Formura.NumericalConfig
import Formura.Type (elementTypenames)
import Formura.Vec
import Formura.Syntax

-- * The parser comibnator

-- | The parser monad.
newtype P a = P { runP :: Parser a }
            deriving (Alternative, Monad, Functor, MonadPlus, Applicative, CharParsing, LookAheadParsing, Parsing, DeltaParsing, MarkParsing Delta)

instance Errable P where
  raiseErr = P . raiseErr

instance TokenParsing P where
  someSpace =
    let  f '\n' = False
         f '\r' = False
         f x | isSpace x = True
             | otherwise = False

    in "whitespace" ?> some ((satisfy f >> return ())
             <|> comment
             <|> lineContinuation)
       >> return ()

-- | Document the parser.
(?>) :: String -> P a -> P a
s ?> p = p <?> s

infixr 0 ?>

-- | Parse a string as a keyword. Check if the keyword is indeed in a keyword list.
keyword :: IdentName -> P IdentName
keyword k = "keyword " ++ k ?> do
  when (k `S.notMember` keywordSet) $
    raiseErr $ failed $
    "Please report the compiler developer: \"" ++ k ++ "\" is not in a keyword list!"

  let moreLikeThis = head $ filter ($ last k) [isIdentifierAlphabet1, isIdentifierSymbol]
  token $ try $ string k <* notFollowedBy (satisfy moreLikeThis)

-- | The set of reserved keywords. The string is not parsed as a identifier if it's in the keyword list.
keywordSet :: S.Set IdentName
keywordSet = S.fromList
             ["begin", "end", "function", "returns", "let", "in",
              "fun", "dimension", "axes",
              "if", "then", "else",
              "const","extern","manifest",
              "+","-","*","/",".","**",
              "::","=", ","]
             <> minMaxOperatorNames
             <> comparisonOperatorNames


comment :: P ()
comment = "comment" ?> do
  char '#'
  manyTill anyChar (lookAhead newline)
  return ()

lineContinuation :: P ()
lineContinuation = "line continuation" ?> do
  char '\\'
  whiteSpace
  newline
  return ()

-- | Run parser, and record the metadata for the parsed syntax component
parseIn :: Functor f => P (Fix f) -> P (Fix f)
parseIn p = do
  r1 <- rend
  (In m x)  <- p
  r2 <- rend
  let m2 = Just $ Metadata r1 (delta r1) (delta r2)
  return $ In (m <|> m2) x


-- * The parser for Formura syntax

isIdentifierAlphabet0 :: Char -> Bool
isIdentifierAlphabet0 = isLetter
isIdentifierAlphabet1 :: Char -> Bool
isIdentifierAlphabet1 c = isAlphaNum c || c == '_'  || c == '\''
isIdentifierSymbol :: Char -> Bool
isIdentifierSymbol c = isPrint c &&
  not (isIdentifierAlphabet1 c || isSpace c ||
      c `elem` "\"#();[\\]{}")

identName :: P IdentName
identName = identNameWith "SA"

alphabetIdentName :: P IdentName
alphabetIdentName = identNameWith "A"

identNameWith :: String -> P IdentName
identNameWith sw = "identifier" ?> try $ do
  let s :: P String
      s = some $ "symbolic character" ?> satisfy isIdentifierSymbol
      a0 :: P Char
      a0 = "identifier alphabet character" ?> satisfy isIdentifierAlphabet0
      a1 :: P Char
      a1 = "identifier alphabet character" ?> satisfy isIdentifierAlphabet1
      a :: P String
      a = (:) <$> a0 <*> many a1
  str <- case sw of
    "S" -> s
    "A" -> a
    _ -> s <|> a
  guard $  str `S.notMember` keywordSet
  whiteSpace
  return str



ident :: (IdentF ∈ fs) => P (Lang fs)
ident = "identifier" ?> parseIn $ Ident <$> identName

elemType :: (ElemTypeF ∈ fs) => P (Lang fs)
elemType = "element type" ?> parseIn $ do
  str <- identName
  guard $ str `S.member` elementTypenames
  return $ ElemType str

funType :: (FunTypeF ∈ fs) => P (Lang fs)
funType = "function type" ?> parseIn $ keyword "function" *> pure FunType


tupleOf :: (TupleF ∈ fs) => P (Lang fs) -> P (Lang fs)
tupleOf p = "tuple" ?> {- don't parseIn here ... -} do
  r1 <- rend
  "tuple opening" ?> try $ symbolic '('
  xs <- p `sepBy` symbolic ','
  symbolic ')'
  r2 <- rend
  case xs of
    -- ... because we treat one-element tuple as parenthesized expression.
    [x] -> return x
    _   -> return $ In (Just $ Metadata r1 (delta r1) (delta r2)) $ Tuple xs

gridIndicesOf :: P a -> P (Vec a)
gridIndicesOf parseIdx = "grid index" ?> do
  "grid opening" ?> try $ symbolic '['
  xs <- parseIdx `sepBy` symbolic ','
  symbolic ']'
  return $ Vec xs

nPlusK :: P NPlusK
nPlusK = "n+k pattern" ?>  do
  mx <- optional alphabetIdentName
  mn <- optional $ do
    s <- symbolic '+' <|> symbolic '-'
    n <- constRationalExpr
    if s == '+' then return n else return (negate n)
  lookAhead (symbolic ',' <|> symbolic ']')
  return $ NPlusK (fromMaybe "" mx) (maybe 0 id mn)


imm :: (ImmF ∈ fs) => P (Lang fs)
imm = "rational literal" ?> parseIn $
  Imm <$> constRational

exprOf :: (OperatorF ∈ fs, ApplyF  ∈ fs) => P (Lang fs) -> P (Lang fs)
exprOf termParser = X.buildExpressionParser tbl termParser
  where
    tbl = [[binary "." (Binop ".") X.AssocRight],
           [binary "**" (Binop "**") X.AssocLeft],
           [binary "*" (Binop "*") X.AssocLeft, binary "/" (Binop "/") X.AssocLeft],
           [unary "+" (Uniop "+") , unary "-" (Uniop "-") ],
           [binary "+" (Binop "+") X.AssocLeft, binary "-" (Binop "-") X.AssocLeft],
           [binary sym (catNary sym) X.AssocLeft | sym <- S.toList minMaxOperatorNames],
           [binary sym (Binop sym) X.AssocNone | sym <- S.toList comparisonOperatorNames]
          ]
    unary  name fun = X.Prefix (pUni name fun)
    binary name fun assoc = X.Infix (pBin name fun) assoc

    catNary sim a b = let
      aparts = case a of Naryop sim' xs | sim == sim' -> xs
                         _                          -> [a]
      bparts = case b of Naryop sim' xs | sim == sim' -> xs
                         _                          -> [b]
      in Naryop sim $ aparts ++ bparts


    pUni name fun = "unary operator " ++ name ?> do
      r1 <- rend
      f <- fun <$ keyword name
      r2 <- rend
      return $ \a -> f a & metadata .~ (Just $ joinMeta r1 r2 a a)

    pBin name fun = "binary operator " ++ name ?> do
      r1 <- rend
      f <- fun <$ keyword name
      r2 <- rend
      return $ \a b -> f a b & metadata .~ (Just $ joinMeta r1 r2 a b)

    joinMeta r1 r2 a b = let
      da = case a ^. metadata of
        Nothing -> delta r1
        Just ma -> min (ma ^. metadataBegin) (delta r1)
      db = case b ^. metadata of
        Nothing -> delta r2
        Just mb -> max (mb ^. metadataEnd) (delta r2)
      in Metadata r1 da db

expr10 :: P RExpr
expr10 = fexpr

fexpr :: P RExpr
fexpr = "function application chain" ?> do
  f <- aexpr
  findArgument f
  where
    findArgument :: RExpr -> P RExpr
    findArgument f = parseIn $ do
      mx' <- optional $ gridIndicesOf nPlusK
      case mx' of
        Just x -> findArgument $ Grid x f
        Nothing ->do
          mx <- optional aexpr
          case mx of
            Just x -> findArgument $ Apply f x
            Nothing ->  return f


aexpr :: P RExpr
aexpr = tupleOf rExpr <|> letExpr <|> lambdaExpr <|> ifThenElseExpr <|> ident <|> imm


letExpr :: P RExpr
letExpr = "let expression" ?> parseIn $ do
  "keyword let" ?> try $ keyword "let"
  xs <- binding
  keyword "in"
  x <- rExpr
  return $ Let xs x

lambdaExpr :: P RExpr
lambdaExpr = "lambda expression" ?> parseIn $ do
  "keyword fun" ?> try $ keyword "fun"
  x <- tupleOf lExpr
  y <- rExpr
  return $ Lambda x y

ifThenElseExpr :: P RExpr
ifThenElseExpr = "if-then-else expression" ?> parseIn $ do
  "keyword if" ?> try $ keyword "if"
  cond <- rExpr
  keyword "then"
  x <- rExpr
  keyword "else"
  y <- rExpr
  return $ Triop "ite" cond x y


binding :: P (BindingF RExpr)
binding = "statements" ?> do
  stmts <- statementCompound `sepEndBy` statementDelimiter
  return $ Binding $ concat stmts

statementDelimiter :: P ()
statementDelimiter = "statement delimiter" ?> some d >> return ()
  where
    d = (symbolic ';' >> return ()) <|> (newline >> whiteSpace)

statementCompound :: P [StatementF RExpr]
statementCompound = functionSyntaxSugar <|> typeValueStatements

functionSyntaxSugar :: P [StatementF RExpr]
functionSyntaxSugar = "function definition" ?> do
  keyword "begin"
  keyword "function"
  (funName, inExpr, outExpr) <-
    ("returns-form" ?> try returnsForm) <|>
    ("equal-form" ?> try equalForm) <|>
    raiseErr (Err (Just $ Ppr.text "Malformed Function Syntax" <> Ppr.line)
              [Ppr.text "Please check if you are using one of the following forms:",
               Ppr.text "・  begin function f(x) returns y",
               Ppr.text "・  begin function y = f(x)"]
              S.empty
              [])
  statementDelimiter
  b <- binding
  keyword "end"
  keyword "function"
  return [Subst funName $ Lambda inExpr $ Let b outExpr]
  where
    returnsForm :: P (LExpr, LExpr, RExpr)
    returnsForm = do
      fn <- ident
      inx <- tupleOf lExpr
      keyword "returns"
      outx <- rExpr
      return (fn, inx, outx)

    equalForm :: P (LExpr, LExpr, RExpr)
    equalForm = do
      outx <- rExpr
      keyword "="
      fn <- ident
      inx <- tupleOf lExpr
      return (fn, inx, outx)


typeValueStatements :: P [StatementF RExpr]
typeValueStatements = "type-decl and/or substitiution statement" ?> do
  maybeType <- optional $ "statement start by type decl" ?> try $ modTypeExpr <* keyword "::"

  let lhsAndMaybeRhs :: P (LExpr, Maybe RExpr)
      lhsAndMaybeRhs = do
        lhs   <- lExpr
        mRhs  <- optional (keyword "=" >> rExpr)
        return (lhs, mRhs)
  lamrs <- case maybeType of
    -- When there is type, we allow multiple substitutions, and lhs-only terms.
    Just _ -> lhsAndMaybeRhs `sepBy1` symbol ","
    -- When there is no type, we allow only one substitution.
    Nothing -> "simple substitution expression" ?> do
      lhs <- lExpr
      keyword "="
      rhs <- rExpr
      return [(lhs, Just rhs)]

  let typePart = [ TypeDecl typ lhs
                 | typ <- maybeToList maybeType,
                   lhs <- map fst lamrs
                   ]
      substPart = [Subst lhs rhs
                   | (lhs, Just rhs) <- lamrs]
  -- Type definitions always come before the values.
  return $ typePart ++ substPart




lAexpr :: P LExpr
lAexpr = "atomic l-expr" ?> tupleOf lExpr <|> ident

vectorIndexOf :: P a -> P a
vectorIndexOf content = do
  "vector index access" ?> try $ symbolic '('
  r <- content
  symbolic ')'
  return r

lFexpr :: P LExpr
lFexpr = "applied l-expr" ?> do
  f <- lAexpr
  go f
  where
    go :: LExpr -> P LExpr
    go f = parseIn $ do
      mx <- "grid option" ?> optional $ gridIndicesOf nPlusK
      case mx of
        Just x -> go $ Grid x f
        Nothing -> do
          mx' <- "grid option" ?>  optional (vectorIndexOf identName)
          case mx' of
            Just x -> go $ Vector x f
            Nothing -> return f

lExpr :: P LExpr
lExpr = "l-expr" ?> lFexpr



modTypeExpr :: P ModifiedTypeExpr
modTypeExpr = do
  tm1 <- many typeModifier
  typ <- optional typeFexpr
  tm2 <- many typeModifier
  return $ case typ of
   Nothing -> ModifiedTypeExpr (tm1 ++ tm2) TopType
   Just t -> ModifiedTypeExpr (tm1 ++ tm2) t

typeModifier :: P TypeModifier
typeModifier = (TMConst <$ keyword "const") <|> (TMExtern <$ keyword "extern") <|> (TMManifest <$ keyword "manifest")

typeAexpr :: P TypeExpr
typeAexpr = "atomic type-expression" ?> tupleOf typeFexpr <|> elemType <|> funType

typeFexpr :: P TypeExpr
typeFexpr = "applied type-expression" ?> do
  f <- typeAexpr
  go f
  where
    go :: TypeExpr -> P TypeExpr
    go f = parseIn $ do
      mx <- optional (gridIndicesOf constRationalExpr)
      case mx of
        Just x -> go $ GridType x f
        Nothing -> do
          mx' <- optional (vectorIndexOf constIntExpr)
          case mx' of
            Just x -> go $ VectorType x f
            Nothing -> return f


rExpr :: P RExpr
rExpr = "r-expr" ?> exprOf expr10

constRationalExpr :: P Rational
constRationalExpr = "const rational expression" ?> do
  cre <- exprOf imm
  mfoldout evalCRE cre

evalCRE :: ConstRationalExprF Rational -> P Rational
evalCRE (Imm x) = return x
evalCRE (Uniop "+" x) = return x
evalCRE (Uniop "-" x) = return $ negate x
evalCRE (Binop "+" a b) = return $ a + b
evalCRE (Binop "-" a b) = return $ a - b
evalCRE (Binop "*" a b) = return $ a * b
evalCRE (Binop "/" a b) = return $ a / b
evalCRE _ = raiseErr $ failed "unsupported operator in const rational expression"

constRational :: P Rational
constRational = "const rational expression" ?> do
  nos <- naturalOrScientific
  return $ either toRational toRational  nos


constIntExpr :: P Int
constIntExpr = fromInteger <$> natural


specialDeclaration :: P SpecialDeclaration
specialDeclaration = dd  <|> ad
  where
    dd = do
      "dimension declaration" ?> try $ keyword "dimension"
      keyword "::"
      n <- natural
      return $ DimensionDeclaration $ fromInteger n
    ad = do
      "axes declaration" ?> try $ keyword "axes"
      keyword "::"
      xs <- identName `sepBy` symbolic ','
      return $ AxesDeclaration xs

program :: WithCommandLineOption => P Program
program = do

  ps <- choice [Left <$> specialDeclaration, Right <$> statementCompound]
        `sepEndBy` statementDelimiter
  let (decls, stmts) = partitionEithers ps

  let mnc = unsafePerformIO $ readYamlDef defaultNumericalConfig ncFilePath
  nc0 <- case mnc of
     Nothing -> raiseErr $ failed $ "cannot parse numerical config .yaml file: " ++ show ncFilePath
     Just x -> return (x :: NumericalConfig)

  let nc = nc0 & ncOptionStrings %~ ((?commandLineOption ^. auxNumericalConfigOptions) ++)
  let globalExtents = toList $ (nc ^. ncIntraNodeShape) * (nc ^. ncMPIGridShape)
      ivars = head [x | AxesDeclaration x <- decls]

      extentVarNames :: [IdentName]
      extentVarNames = map (("N" ++) . map toUpper) ivars

      mkExtentStmt :: IdentName -> Int -> StatementF RExpr
      mkExtentStmt x n = SubstF (Ident x) (Imm $ fromIntegral n)

      globalExtentStmts = zipWith mkExtentStmt extentVarNames globalExtents

  return $ Program decls (BindingF $ globalExtentStmts ++ concat stmts) nc
