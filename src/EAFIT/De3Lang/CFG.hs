module EAFIT.De3Lang.CFG
    (
     Term(..),
     NoTerm(..),
     Symbol(..),
     CFG(..),
     Derivation,
     ParserTree(..)
    )
    where

import qualified Data.Set as Set
import qualified Data.Map as Map

data Term = Term String
          | TermLit String
          | TermCharLit Char
          | TermStringLit String
          | TermNat Integer
          | TermInt Integer
          | TermDouble Double
          | TermIdent String
          | TermChar  Char
          | TermHex   Integer
         -- | TermNat   Integer
          | TermDec   Integer
          | TermOct   Integer
            deriving (Show, Eq, Ord)

newtype NoTerm = NoTerm String deriving (Show, Eq, Ord)

data Symbol = SymTerm Term
            | SymNoTerm NoTerm
              deriving (Show, Eq, Ord)

data CFG = CFG { noTerms :: Set.Set NoTerm,
                 terms   :: Set.Set Term,
                 prods   :: Map.Map NoTerm [[Symbol]],
                 start   :: NoTerm }
         deriving (Show)

type Derivation = [[Symbol]]

data ParserTree = ParserTree Symbol [ParserTree]
                deriving (Show)
