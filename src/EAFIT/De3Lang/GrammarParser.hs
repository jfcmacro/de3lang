module EAFIT.De3Lang.GrammarParser-- (pCFGFile
                                  -- ,pDrvFile
                                  -- ,pTreeFile
                                  -- ,module Text.ParserCombinators.Parsec
                                  -- ,pSymbols
                                  -- )
    where

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
pNoTerm  = NoTerm <$> pure <$> upper
    -- do c  <- upper 
    --    return $ NoTerm [c]

pTerm :: GenParser Char st Term
pTerm = Term <$> pure <$> lower
    -- do c  <- lower
    --    return $ Term [c]

pNoTerms :: GenParser Char st [NoTerm]
pNoTerms = pNoTerm `sepBy` (spaces *> char ',')

pTerms :: GenParser Char st [Term]
pTerms = pTerm `sepBy` (spaces *> char ',')

pBraces :: GenParser Char st a -> GenParser Char st a
pBraces = between (spaces *> char '{')
                  (spaces *> char '}')

pSym :: GenParser Char st Symbol
pSym = (SymTerm <$> pTerm) <|> (SymNoTerm <$> pNoTerm)

pSyms :: GenParser Char st [Symbol]
pSyms = spaces *> many pSym

pSymbols :: GenParser Char st [[Symbol]]
pSymbols = pSyms `sepBy` (spaces *> char '|')

pPrdSym :: GenParser Char st ()
pPrdSym = string "->" >> return ()
  -- char '-'
  -- char '>'
  -- return $ ()
          
pProdT :: GenParser Char st (NoTerm ,[[Symbol]])
pProdT = (,) <$> (spaces *> pNoTerm)
             <*  (spaces *> pPrdSym)
             <*> (spaces *> pSymbols)
  -- do
  -- nt <- pNoTerm
  -- pPrdSym
  -- pr <- pSymbols
  -- return $ (nt, pr)


pProdTs :: GenParser Char st [(NoTerm, [[Symbol]])]
pProdTs = pProdT `sepBy` (spaces *> char ',')

pProds :: GenParser Char st (Map.Map NoTerm [[Symbol]])
pProds = Map.fromList <$> pProdTs
    -- do
    --   prds <- pProdTs
    --   return $ Map.fromList prds
         
pInnerCfg :: GenParser Char st CFG
pInnerCfg = do
  nts   <- (pBraces pNoTerms)
  (spaces *> char ',')
  ts    <- (pBraces pTerms)
  (spaces *> char ',')
  prds  <- (pBraces pProds)
  (spaces *> char ',')
  st    <- (spaces *> pNoTerm <* spaces)
  return $ CFG { noTerms = Set.fromList nts,
                 terms   = Set.fromList ts,
                 prods   = prds,
                 start   = st }
                 
pCfg :: GenParser Char st CFG
pCfg = spaces *> pBraces pInnerCfg <* spaces

pCFGFile :: String -> IO (Either ParseError CFG)
pCFGFile fname = parseFromFile pCfg fname
  
pDrv :: GenParser Char st Derivation
pDrv = pSyms `sepBy` (string "=>")

pDrvFile :: String -> IO (Either ParseError Derivation)
pDrvFile fname = parseFromFile pDrv fname

pParens :: GenParser Char st a -> GenParser Char st a
pParens = between (char  '(') (char ')')

pChildTerm :: GenParser Char st ParserTree
pChildTerm = do
  t <- pTerm
  return $ ParserTree (SymTerm t) []

pNoTermSubTree :: GenParser Char st ParserTree
pNoTermSubTree = do { nt <- pNoTerm;
                      chd <- pChildren ;
                      return $ ParserTree (SymNoTerm nt) chd }

pChildNoTerm :: GenParser Char st ParserTree
pChildNoTerm = pNoTermSubTree
  
pChild :: GenParser Char st ParserTree
pChild =  pParens $ (pChildNoTerm
                     <|> pChildTerm)
pChildren :: GenParser Char st [ParserTree]
pChildren = many pChild

pTree :: GenParser Char st ParserTree
pTree = pParens pChildNoTerm

pTreeFile :: String -> IO (Either ParseError ParserTree)
pTreeFile fname = parseFromFile pTree fname
