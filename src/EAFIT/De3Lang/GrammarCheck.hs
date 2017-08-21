module EAFIT.De3Lang.GrammarCheck(cfgIsClean,cfgIsWellDef) where

import EAFIT.De3Lang.CFG(Term(..),
                         NoTerm(..),
                         Symbol(..),
                         CFG(..),
                         Derivation,
                         ParserTree(..))

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map(foldrWithKey)


cfgIsWellDef :: CFG -> Bool
cfgIsWellDef cfg
    | S.null $ noTerms cfg                     = False
    | S.null $ terms cfg                       = False
    | M.null $ prods cfg                       = False
    | not $ S.member (start cfg) (noTerms cfg) = False
    | otherwise                                = let a = start cfg
                                                     v = noTerms cfg
                                                     s = terms cfg
                                                     p = prods cfg
                                                     pwd = M.foldrWithKey (isPWD v s) True p 
                                                 in pwd
    where  isRWD v s (SymTerm t) r    = r && S.member t s
           isRWS v s (SymNoTerm nt) r = r && S.member nt v
           isPWD v s lhs rhs r = r && S.member lhs v && and(map (foldr (isRWD v s) True) rhs)

rhsIsOnlyTerms :: [Symbol] -> Bool
rhsIsOnlyTerms = foldr isTerm True
    where isTerm (SymTerm _)   r = r && True
          isTerm (SymNoTerm _) r = r && False
               
anyTerminalRule :: [[Symbol]] -> Bool
anyTerminalRule = any rhsIsOnlyTerms

rhsGenTerms :: S.Set NoTerm -> [Symbol] -> Bool
rhsGenTerms def rhs = foldr isTerm True rhs
    where isTerm (SymTerm _)    r = r && True
          isTerm (SymNoTerm nt) r = r && S.member nt def

anyTerminalRuleGen :: S.Set NoTerm -> [[Symbol]] -> Bool
anyTerminalRuleGen def rhss = any (rhsGenTerms def) rhss

cfgIsClean' :: M.Map NoTerm [[Symbol]] -> S.Set NoTerm -> S.Set NoTerm
cfgIsClean' p def' = let def'' = M.foldrWithKey lhsPrdTerms def' p
                     in if def'' == def'
                        then def''
                        else cfgIsClean' p def''
    where  lhsPrdTerms lhs rhss r =
               if anyTerminalRuleGen r rhss then S.insert lhs r else r
                  
cfgIsClean :: CFG -> Bool
cfgIsClean cfg = let a = start cfg
                     v = noTerms cfg
                     s = terms cfg
                     p = prods cfg
                     def0 = M.foldrWithKey lhsIsGen S.empty p
                 in cfgIsClean' p def0 == v
    where lhsIsGen lhs rhss r =
              if anyTerminalRule rhss then S.insert lhs r else r
 
cfgEachLhsIsReached :: CFG -> Bool
cfgEachLhsIsReached cfg = let a = start cfg
                              v = noTerms cfg
                              s = terms cfg
                              p = prods cfg
                          in
