module EAFIT.De3Lang.GrammarParser(pCFGFile
                                  ,pNoTerm
                                  ,pSyms
                                  ,pDrvFile
                                  ,pTreeFile) where

import EAFIT.De3Lang.CFG(Term(..),
                         NoTerm(..),
                         Symbol(..),
                         CFG(..),
                         Derivation,
                         ParserTree(..))
import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import qualified Data.Map as Map
 
pNoTerm :: GenParser Char st NoTerm
pNoTerm  =
    do c  <- upper 
       return $ NoTerm [c]

pTerm :: GenParser Char st Term
pTerm =
    do c  <- lower
       return $ Term [c]

pNoTerms :: GenParser Char st [NoTerm]
pNoTerms = pNoTerm `sepBy` (char ',')

pTerms :: GenParser Char st [Term]
pTerms = pTerm `sepBy` (char ',')

pBraces :: GenParser Char st a -> GenParser Char st a
pBraces = between (char  '{') (char '}')

pSym :: GenParser Char st Symbol
pSym = (pTerm >>= \t -> return $ SymTerm t) <|> (pNoTerm >>= \nt -> return $ SymNoTerm nt)

pSyms :: GenParser Char st [Symbol]
pSyms = many pSym

pSymbols :: GenParser Char st [[Symbol]]
pSymbols = pSyms `sepBy` (char '|')

pPrdSym :: GenParser Char st ()
pPrdSym = do
  char '-'
  char '>'
  return $ ()
          
pProdT :: GenParser Char st (NoTerm ,[[Symbol]])
pProdT = do
  nt <- pNoTerm
  pPrdSym
  pr <- pSymbols
  return $ (nt, pr)

pProdTs :: GenParser Char st [(NoTerm, [[Symbol]])]
pProdTs = pProdT `sepBy` (char ',')

pProds :: GenParser Char st (Map.Map NoTerm [[Symbol]])
pProds = do
  prds <- pProdTs
  return $ Map.fromList prds
         
pInnerCfg :: GenParser Char st CFG
pInnerCfg = do
  nts   <- pBraces pNoTerms
  char ','
  ts    <- pBraces pTerms
  char ','
  prds  <- pBraces pProds
  char ','
  st    <- pNoTerm
  return $ CFG { noTerms = Set.fromList nts,
                 terms   = Set.fromList ts,
                 prods   = prds,
                 start   = st }
                 
pCfg :: GenParser Char st CFG
pCfg = pBraces pInnerCfg

pCFGFile :: String -> IO (Either ParseError CFG)
pCFGFile fname = parseFromFile pCfg fname
  
pDrv :: GenParser Char st Derivation
pDrv = pSyms `sepBy` (do { char '='; char '>' })

pDrvFile :: String -> IO (Either ParseError Derivation)
pDrvFile fname = parseFromFile pDrv fname

pParens :: GenParser Char st a -> GenParser Char st a
pParens = between (char  '(') (char ')')

pChildTerm :: GenParser Char st ParserTree
pChildTerm = do
  t <- pParens pTerm
  return $ ParserTree (SymTerm t) []

pNoTermSubTree :: GenParser Char st ParserTree
pNoTermSubTree = do
  nt <- pNoTerm
  chd <- pChildren
  return $ ParserTree (SymNoTerm nt) chd

pChildNoTerm :: GenParser Char st ParserTree
pChildNoTerm = pParens pNoTermSubTree
  
pChild :: GenParser Char st ParserTree
pChild = pChildTerm <|> pChildNoTerm

pChildren :: GenParser Char st [ParserTree]
pChildren = many pChild

pTree :: GenParser Char st ParserTree
pTree = pChildNoTerm

pTreeFile :: String -> IO (Either ParseError ParserTree)
pTreeFile fname = parseFromFile pTree fname
