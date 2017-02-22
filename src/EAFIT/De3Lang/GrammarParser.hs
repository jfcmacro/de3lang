module EAFIT.De3Lang.GrammarParser where

import EAFIT.De3Lang.CFG(Term(..),
                         NoTerm(..),
                         Symbol(..),
                         CFG(..))
import Text.ParserCombinators.Parsec
 
pNoTerm :: GenParser Char st NoTerm
pNoTerm  =
    do c  <- upper 
       return $ NoTerm [c]

pTerm :: GenParser Char st Term
pTerm =
    do c  <- lower
       return $ Term [c]

