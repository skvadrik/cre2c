{-# LANGUAGE BangPatterns #-}

module CFA
    ( NCFA
    , DCFA
--    , CFANode
    , Label
    , State
    , SignNum
    , SignSet

    , emptyNCFA

    , initStateNCFA
    , initStateDCFA
    , initNodeDCFA
    , maxState
--    , cfaGraph
    , isFinalDCFA
    , isFinalNCFA
    , acceptedSignatures

    , setFinal
    , addTransitionNCFA
    , determine

    , toDotNCFA
    , toDotDCFA
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl', intersect, find, partition, delete, nub, sort)
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust, fromJust)
import           Data.Hashable
import           Control.DeepSeq          (force)

import           Types


emptyNCFA :: NCFA
emptyNCFA = NCFA 0 1 M.empty M.empty


initStateNCFA :: NCFA -> State
initStateNCFA = ncfa_init_state


initStateDCFA :: DCFA -> State
initStateDCFA = dcfa_init_state


initNodeDCFA :: DCFA -> DCFAInitNode
initNodeDCFA = dcfa_init_node


maxState :: NCFA -> State
maxState = ncfa_max_state


dcfaGraph :: DCFA -> DCFAGraph
dcfaGraph = dcfa_graph


isFinalDCFA :: State -> DCFA -> Bool
isFinalDCFA st dcfa = isJust $ M.lookup st (dcfa_final_states dcfa)


isFinalNCFA :: State -> NCFA -> Bool
isFinalNCFA st ncfa = isJust $ M.lookup st (ncfa_final_states ncfa)


acceptedSignatures :: State -> DCFA -> SignSet
acceptedSignatures s (DCFA _ _ _ fss) = M.lookupDefault (S.empty) s fss


setFinal :: State -> SignNum -> NCFA -> NCFA
setFinal s k (NCFA s0 sl g fss) = NCFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


addTransitionNCFA :: NCFA -> (State, Label, SignNum, State) -> (NCFA, State)
addTransitionNCFA ncfa@(NCFA s0 sl g fss) (s1, l, k, s2) =
    let g'  = M.insertWith (\ _ node -> (l, k, s2) : node) s1 [(l, k, s2)] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


------------------------------------------------------------------------------------------------

type Node = [(Char, SignNum, State)]
type MultiArc = (Label, SignSet, S.Set State)


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


to_label :: String -> Label
to_label [c] = LabelChar c
to_label r   = LabelRange r


determine_init_node :: NCFANode -> NCFAGraph -> State -> (DCFAInitNode, NCFAGraph, State, S.Set State)
determine_init_node n g s_max =
    let f :: (SignNum, State) -> M.HashMap Char ([SignNum], [State]) -> Char -> M.HashMap Char ([SignNum], [State])
        f (k, s) n c = M.insertWith (\ _ (ks, ss) -> (k : ks, s : ss)) c ([k], [s]) n
        n' = foldl'
            (\ n (l, k, s) -> case l of
                LabelChar c  -> f (k, s) n c
                LabelRange r -> foldl' (f (k, s)) n r
            ) M.empty n
        (xs, ys) = M.foldlWithKey'
            (\ (xs, ys) c (ks@(k : ks'), ss@(s : ss')) -> if ss' == []
                then (xs, M.insertWith (\ _ r -> c : r) (k, s) [c] ys)
                else
                    let ks' = S.fromList ks
                    in  (M.insertWith (\ _ (r, ks'') -> (c : r, S.union ks' ks'')) (S.fromList ss) ([c], ks') xs, ys)
            ) (M.empty, M.empty) n'
        n'' = M.foldlWithKey' (\ n (k, s) r -> M.insert (to_label r) (S.insert k S.empty, s) n) M.empty ys
        (n''', sss, s_max') = M.foldlWithKey' (\ (n, sss, i) ss (r, ks) -> (M.insert (to_label r) (ks, i) n, M.insert i ss sss, i + 1)) (n'', M.empty, s_max) xs
        g' = M.foldlWithKey'
            (\ g s ss ->
                let n = S.foldl' (\ n s' -> M.lookupDefault [] s' g ++ n) [] ss
                in  M.insert s n g
            ) g sss
        ss = M.foldl' (\ ss (_, s) -> S.insert s ss) S.empty n'''
    in  (n''', g', s_max', ss)


determine_node :: NCFANode -> NCFAGraph -> State -> (DCFANode, NCFAGraph, State, S.Set State)
determine_node n g s_max =
    let f :: (SignNum, State) -> M.HashMap Char ([SignNum], [State]) -> Char -> M.HashMap Char ([SignNum], [State])
        f (k, s) n c = M.insertWith (\ _ (ks, ss) -> (k : ks, s : ss)) c ([k], [s]) n
        n' = foldl'
            (\ n (l, k, s) -> case l of
                LabelChar c  -> f (k, s) n c
                LabelRange r -> foldl' (f (k, s)) n r
            ) M.empty n
        (xs, ys) = M.foldlWithKey'
            (\ (xs, ys) c (ks@(k : ks'), ss@(s : ss')) -> if ss' == []
                then (xs, M.insertWith (\ _ r -> c : r) (k, s) [c] ys)
                else (M.insertWith (\ _ r -> c : r) (S.fromList ss) [c] xs, ys)
            ) (M.empty, M.empty) n'
        n'' = M.foldlWithKey' (\ n (_, s) r -> M.insert (to_label r) s n) M.empty ys
        (n''', sss, s_max') = M.foldlWithKey' (\ (n, sss, i) ss r -> (M.insert (to_label r) i n, M.insert i ss sss, i + 1)) (n'', M.empty, s_max) xs
        g' = M.foldlWithKey'
            (\ g s ss ->
                let n = S.foldl' (\ n s' -> M.lookupDefault [] s' g ++ n) [] ss
                in  M.insert s n g
            ) g sss
        ss = M.foldl' (\ ss s -> S.insert s ss) S.empty n'''
    in  (n''', g', s_max', ss)


f :: (NCFAGraph, DCFAGraph, State, S.Set State) -> DCFAGraph
f (_, dg, _, ss) | ss == S.empty = dg
f (ng, dg, s_max, ss)            = f $ S.foldl'
    (\ (ng, dg, s_max, ss) s ->
        let n = M.lookupDefault [] s ng
        in  if n /= []
                then
                    let (n', ng', s_max', ss') = determine_node n ng s_max
                    in  (M.delete s ng', M.insert s n' dg, s_max', S.union ss' ss)
                else (ng, dg, s_max, ss)
    ) (ng, dg, s_max, S.empty) ss


determine :: NCFA -> DCFA
determine ncfa@(NCFA s0 sl g fss) =
    let n0                = M.lookupDefault (error "init node absent in ncfa") s0 g
        (n0', g', s_max', ss) = determine_init_node n0 g sl
        g''               = M.delete s0 g'
        dg                = f (g'', M.empty, s_max', ss)
    in  DCFA s0 n0' dg fss


toDotNCFA :: NCFA -> FilePath -> IO ()
toDotNCFA (NCFA _ _ g fss) fp = do
    writeFile  fp "digraph CFA {\n"
    appendFile fp "\trankdir = LR\n\tnode [shape=\"circle\"]\n"
    forM_ (M.keys fss) $
        \ i -> appendFile fp $ concat
            [ "\t"
            , show i
            , " [shape=\"doublecircle\" label=\""
            , show i
            , show (M.lookupDefault undefined i fss)
            , "\"]\n"
            ]
    forM_ (M.toList g) $
        \ (s, node) -> forM_ node $
            \ (l, k, s') -> appendFile fp $ concat
                [ "\t"
                , show s
                , " -> "
                , show s'
                , " [label=\""
                , show l
                , ", "
                , show k
                , "\"]\n"
                ]
    appendFile fp "}\n"


toDotDCFA :: DCFA -> FilePath -> IO ()
toDotDCFA (DCFA s0 node0 g fss) fp = do
    writeFile  fp "digraph CFA {\n"
    appendFile fp "\trankdir = LR\n\tnode [shape=\"circle\"]\n"
    forM_ (M.keys fss) $
        \ i -> appendFile fp $ concat
            [ "\t"
            , show i
            , " [shape=\"doublecircle\" label=\""
            , show i
            , show (M.lookupDefault undefined i fss)
            , "\"]\n"
            ]
    forM_ (M.toList node0) $
        \ (l, (ks, s')) -> appendFile fp $ concat
            [ "\t"
            , show s0
            , " -> "
            , show s'
            , " [label=\""
            , show l
            , show $ S.toList ks
            , case l of
                LabelRange _ -> "\", style=bold, color=red]\n"
                _            -> "\"]\n"
            ]
    forM_ (M.toList g) $
        \ (s, node) -> forM_ (M.toList node) $
            \ (l, s') -> appendFile fp $ concat
                [ "\t"
                , show s
                , " -> "
                , show s'
                , " [label=\""
                , show l
                , case l of
                    LabelRange _ -> "\", style=bold, color=red]\n"
                    _            -> "\"]\n"
                ]
    appendFile fp "}\n"
