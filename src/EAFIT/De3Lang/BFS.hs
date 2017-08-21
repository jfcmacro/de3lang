module EAFIT.De3Lang.BFS where

import qualified Data.Set as S
import qualified Data.Map as M

data Color  = White | Gray | Black deriving (Show, Eq, Ord)
data Dist   = Inf | Dist Integer deriving (Show, Eq, Ord)
data Pi  a  = Nil | Pi a deriving (Show, Eq, Ord) 

data BFSInfo a = BFSInfo { color :: M.Map a Color,
                           dist  :: M.Map a Dist,
                           pi    :: M.Map a
                         }

bfs :: (Eq a, Eq b) => S.Set a -> M.Map a [[b]] -> a -> BFSInfo a 
bfs v adj s =
    let bfsinfo = BFSInfo {
                    S.foldr (\M.insert) (v S.\\ S.singleton s),
                    
                     
