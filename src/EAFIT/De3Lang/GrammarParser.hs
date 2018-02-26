module EAFIT.De3Lang.GrammarParser(pCFGFile
                                  ,pDrvFile
                                  ,pTreeFile
                                  )
    where

import EAFIT.De3Lang.CFG(Term(..),
                         NoTerm(..),
                         Symbol(..),
                         CFG(..),
                         Derivation,
                         ParserTree(..))
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language(emptyDef)
import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import qualified Data.Map as Map

-- pNoTerm :: GenParser Char st NoTerm
-- pNoTerm  = NoTerm <$> pure <$> upper

-- pTerm :: GenParser Char st Term
-- pTerm = Term <$> pure <$> lower

-- pLeftSpaces :: GenParser Char st a -> GenParser Char st a
-- pLeftSpaces p = spaces *> p

-- pLSSemi :: GenParser Char st Char
-- pLSSemi = pLeftSpaces $ char ','

-- pNoTerms :: GenParser Char st [NoTerm]
-- pNoTerms = pNoTerm `sepBy` pLSSemi

-- pTerms :: GenParser Char st [Term]
-- pTerms = pTerm `sepBy` pLSSemi

-- pBraces :: GenParser Char st a -> GenParser Char st a
-- pBraces = between (spaces *> char '{')
--                   (spaces *> char '}')

-- pSym :: GenParser Char st Symbol
-- pSym = (SymTerm <$> pTerm) <|> (SymNoTerm <$> pNoTerm)

-- pSyms :: GenParser Char st [Symbol]
-- pSyms = spaces *> many pSym

-- pSymbols :: GenParser Char st [[Symbol]]
-- pSymbols = pSyms `sepBy` (spaces *> char '|')

-- pPrdSym :: GenParser Char st ()
-- pPrdSym = string "->" >> return ()

-- pProdT :: GenParser Char st (NoTerm ,[[Symbol]])
-- pProdT = (,) <$> (spaces *> pNoTerm)
--              <*  (spaces *> pPrdSym)
--              <*> (spaces *> pSymbols)

-- pProdTs :: GenParser Char st [(NoTerm, [[Symbol]])]
-- pProdTs = pProdT `sepBy` pLSSemi

-- pProds :: GenParser Char st (Map.Map NoTerm [[Symbol]])
-- pProds = Map.fromList <$> pProdTs

-- pInnerCfg :: GenParser Char st CFG
-- pInnerCfg = do
--   nts   <- (pBraces pNoTerms)
--   pLSSemi
--   ts    <- (pBraces pTerms)
--   pLSSemi
--   prds  <- (pBraces pProds)
--   pLSSemi
--   st    <- (spaces *> pNoTerm <* spaces)
--   return $ CFG { noTerms = Set.fromList nts,
--                  terms   = Set.fromList ts,
--                  prods   = prds,
--                  start   = st }

-- pCfg :: GenParser Char st CFG
-- pCfg = spaces *> pBraces pInnerCfg <* spaces

-- pCFGFile :: FilePath -> IO (Either ParseError CFG)
-- pCFGFile fname = parseFromFile pCfg fname

-- pDrv :: GenParser Char st Derivation
-- pDrv = (spaces *> pSyms) `sepBy` (spaces *> string "=>")
--        <?> "Derivation error"

-- pDrvFile :: FilePath -> IO (Either ParseError Derivation)
-- pDrvFile fname = parseFromFile (pDrv <* spaces) fname

-- pParens :: GenParser Char st a -> GenParser Char st a
-- pParens = between (spaces *> char  '(') (spaces *> char ')')

-- pChildTerm :: GenParser Char st ParserTree
-- pChildTerm = do
--   t <- pTerm
--   return $ ParserTree (SymTerm t) []

-- pNoTermSubTree :: GenParser Char st ParserTree
-- pNoTermSubTree =
--     do nt <- pNoTerm
--        chd <- pChildren
--        return $ ParserTree (SymNoTerm nt) chd

-- pChildNoTerm :: GenParser Char st ParserTree
-- pChildNoTerm = pNoTermSubTree

-- pChild :: GenParser Char st ParserTree
-- pChild =  pParens $ (pChildNoTerm
--                      <|> pChildTerm)

-- pChildren :: GenParser Char st [ParserTree]
-- pChildren = many pChild

-- pTree :: GenParser Char st ParserTree
-- pTree = pParens pChildNoTerm

-- pTreeFile :: FilePath -> IO (Either ParseError ParserTree)
-- pTreeFile fname = parseFromFile pTree fname
lexer      = P.makeTokenParser emptyDef {
               P.commentLine = "#"
             }

parens     = P.parens lexer
braces     = P.braces lexer
identifier = P.identifier lexer
whitespace = P.whiteSpace lexer
integer    = P.integer lexer
comma      = P.comma lexer
noTerm     = between (char '<') (char '>') identifier
bar        = P.symbol lexer "|"
produce    = P.symbol lexer ":="
derive     = P.symbol lexer "=>"
lexeme     = P.lexeme lexer

pNoTerm :: GenParser Char st NoTerm
pNoTerm = NoTerm <$> lexeme noTerm

pNoTerms :: GenParser Char st [NoTerm]
pNoTerms = braces pNoTerms'
    where pNoTerms' = sepBy1 pNoTerm comma

pTerm :: GenParser Char st Term
pTerm = Term <$> identifier

pTerms :: GenParser Char st [Term]
pTerms = braces pTerms'
    where pTerms' = sepBy1 pTerm comma

pSenForm :: GenParser Char st [Symbol]
pSenForm = many $ lexeme pSym
    where pSym = SymNoTerm <$> pNoTerm
                 <|>
                 SymTerm <$> pTerm
                    
pProdT :: GenParser Char st (NoTerm, [[Symbol]])
pProdT = (,) <$> pNoTerm
         <*  (lexeme produce)
         <*> pSenForms
    where pSenForms = sepBy1 pSenForm $ lexeme bar

pProds :: GenParser Char st (Map.Map NoTerm [[Symbol]])
pProds = Map.fromList <$> braces pProdTs
    where pProdTs = pProdT `sepBy` comma

pCfg :: GenParser Char st CFG
pCfg = braces pInnerCfg
    where pInnerCfg = do
            nts  <- pNoTerms
            comma
            ts   <- pTerms
            comma
            prds <- pProds
            comma
            st   <- pNoTerm
            return $ CFG { noTerms = Set.fromList nts,
                           terms   = Set.fromList ts,
                           prods   = prds,
                           start   = st }

pCFGFile :: FilePath -> IO (Either ParseError CFG)
pCFGFile fname = parseFromFile pCfg fname

pDrv :: GenParser Char st Derivation
pDrv = do
  nts  <- pNoTerm
  drvs <- many1 (derive *> pSenForm)
  return ([SymNoTerm nts] : drvs)

pDrvFile :: FilePath -> IO (Either ParseError Derivation)
pDrvFile fname = parseFromFile pDrv fname

pChildTerm :: GenParser Char st ParserTree
pChildTerm = f <$> pTerm
  where f t = ParserTree (SymTerm t) []

pTree :: GenParser Char st ParserTree
pTree = parens pTree'
    where pTree' = 
              do nt  <- pNoTerm
                 chd <- pChildren
                 return $ ParserTree (SymNoTerm nt) chd
                               
pChild :: GenParser Char st ParserTree
pChild = pTree <|> pChildTerm

pChildren = many pChild

pTreeFile :: FilePath -> IO (Either ParseError ParserTree)
pTreeFile fname = parseFromFile pTree fname
