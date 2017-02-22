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

newtype Term = Term String deriving (Show, Eq, Enum)

newtype NoTerm = NoTerm String deriving (Show, Eq, Enum)
    
data Symbol = SymTerm Term
            | SymNoTerm NoTerm
              deriving (Show, Eq, Enum)

data CFG = CFG { noTerms :: Set.Set NoTerm,
                 terms   :: Set.Set Term,
                 prods   :: Map.Map NoTerm (Set.Set Symbol),
                 start   :: NoTerm }
         deriving (Show)

type Derivation = [[Symbol]]

data ParserTree = ParserTree Symbol [ParserTree]
                deriving (Show)
