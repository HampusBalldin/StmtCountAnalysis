module CFG where

import Stmt

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Control.Monad.State
import Data.Bifunctor
import System.IO
import Data.Maybe (fromJust)

data Node = DEF Identifier Expr
          | BRANCH Expr
          | RETURN Expr
          | EMPTY
          deriving(Show,Eq)

type NodeIndex = Int
type Edge = (NodeIndex, NodeIndex)
type CFG = (Map NodeIndex Node, [Edge])

type CFGState = State
                (NodeIndex, NodeIndex) -- (ParentIndex, IndexCounter)
                CFG

combineMaps :: Ord a => Map a b -> Map a b -> Map a b
combineMaps  m1 m2 = Map.fromList $ Map.toList m1 ++ Map.toList m2

seqModify :: State (NodeIndex, NodeIndex) (NodeIndex, NodeIndex)
seqModify = get >>= \(p, i) -> put (i + 1, i + 1) >> pure (p, i + 1)

buildAtomic :: (NodeIndex -> Map NodeIndex Node) -> CFGState
buildAtomic f = get >>= \(p, i) -> let i' = i + 1 in put (i',i') >> pure (f i', [(p, i')])

build :: Stmt -> CFGState
build Skip       = buildAtomic (\i  -> Map.fromList [(i, EMPTY)])
build (i :<-: e) = buildAtomic (\i' -> Map.fromList [(i', DEF i e)])
build (Return e) = buildAtomic (\i  -> Map.fromList [(i, RETURN e)])

build (Compound s) = foldM (\(m, es) s -> build s >>= \(m', es') ->
                               pure (combineMaps m m', es ++ es')) (Map.empty, []) s 

build (If cond th el) = seqModify >>= \(p, i) ->
                        build th >>= \(m1, e1) ->
                        get >>= \(_, i') ->
                        put (i, i') >>
                        build el >>= \(m2, e2) ->
                        get >>= \(_, i'') ->
                        put (i''+1,i''+1) >> -- Insert artificial continue node after branch
                        pure (Map.insert (i''+1) EMPTY $
                              Map.insert i (BRANCH cond) (combineMaps m1 m2),
                              (i'', i'' + 1):  -- Else to Artificial 
                              (i', i'' +1):    -- Them to Artificial
                              (p, i):          -- Parent of BRANCH node to BRANCH node
                              e1 ++ e2)
                                       
build (While cond s) = seqModify >>= \(p, i) ->
                       build s >>= \(m, e) ->
                       get >>= \(_, i') ->
                       put (i'+1, i'+1) >> -- Insert artificial continue node after branch
                       pure (Map.insert (i' + 1) EMPTY $
                             Map.insert i (BRANCH cond) m,
                             (i, i' + 1):  -- While node to empty node after done
                             (p, i):       -- Parent of BRANCH node to BRANCH node
                             (i', i):      -- Loop back
                             e)
                       
build (Twice s) = (uncurry bimap . bimap combineMaps (++)) <$> build s <*> build s

compileCFG :: Stmt -> CFG
compileCFG prog = let (m,e) = evalState (build prog) (0,0)
                      nodes = Set.toList . Set.fromList $ join [[x] ++ [y] | (x, y) <- e, x /= 0]
                      rets  = Set.fromList [n | (n, re) <- ((\n ->
                                 (n, fromJust $ Map.lookup n m)) <$> nodes), isReturn re]
       in (m, (0, -1) : [(x,y) | (x,y) <- e, not $ Set.member x rets] ++
                        (flip (,) (-1) <$> Set.toList rets))
    where
      isReturn :: Node -> Bool
      isReturn (RETURN _) = True
      isReturn _          = False

cfgToDot :: FilePath -> CFG -> IO ()
cfgToDot fp (m, e) =
  withFile fp WriteMode $ \fh ->
        hPutStrLn fh "digraph CFG { " >>
        hPutStrLn fh "{" >>
        hPutStrLn fh "forcelabels=true" >>
        hPutStrLn fh "ratio=fill" >>
        hPutStrLn fh "node [shape=box]" >>
        sequence ((\(i, s) -> hPutStrLn fh $ show i ++
                    " [label=\"" ++ show i ++ ": " ++ show s ++ "\"]") <$> Map.toList m) >>
        hPutStrLn fh "}" >>
        sequence ((\(t, h) -> hPutStrLn fh $ show t ++ " -> " ++ show h ++ ";") <$> e) >>
        hPutStrLn fh "} "

compileToDot fp = cfgToDot fp . compileCFG
