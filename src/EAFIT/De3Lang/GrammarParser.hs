module EAFIT.De3Lang.GrammarParser where

import EAFIT.De3Lang.CFG(Term(..),
                         NoTerm(..),
                         Symbol(..),
                         CFG(..))
import Text.ParserCombinators.Parsec

parserNoTerm :: GenParser Char st String
parserNoTerm  =
    do c <- upper
       return [c]

parserTerm :: 

    
