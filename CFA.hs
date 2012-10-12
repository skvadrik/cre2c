{-# LANGUAGE BangPatterns #-}

module CFA
    ( NCFA
    , DCFA
    , Label
    , State
    , SignNum
    , SignSet

    , emptyNCFA

    , initStateNCFA
    , initStateDCFA
    , initNodeDCFA
    , maxState
    , dcfaGraph
    , isFinalDCFA
    , isFinalNCFA
    , acceptedSignatures
    , finalStates

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
import           Control.DeepSeq

import           Types


emptyNCFA :: NCFA
emptyNCFA = NCFA 0 1 M.empty M.empty


initStateNCFA :: NCFA -> State
initStateNCFA = ncfa_init_state


initStateDCFA :: DCFA -> State
initStateDCFA = dcfa_init_state


initNodeDCFA :: DCFA -> DCFANode
initNodeDCFA = dcfa_init_node


maxState :: NCFA -> State
maxState = ncfa_max_state


dcfaGraph :: DCFA -> DCFAGraph
dcfaGraph = dcfa_graph


isFinalDCFA :: State -> DCFA -> Bool
isFinalDCFA st (DCFA _ _ _ fss) = isJust $ M.lookup st fss


isFinalNCFA :: State -> NCFA -> Bool
isFinalNCFA st (NCFA _ _ _ fss) = isJust $ M.lookup st fss


acceptedSignatures :: State -> DCFA -> SignSet
acceptedSignatures s (DCFA _ _ _ fss) = M.lookupDefault S.empty s fss


finalStates :: DCFA -> M.HashMap State SignSet
finalStates (DCFA _ _ _ fss) = fss


setFinal :: State -> SignNum -> NCFA -> NCFA
setFinal s k (NCFA s0 sl g fss) = NCFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


addTransitionNCFA :: NCFA -> (State, Label, SignNum, State) -> (NCFA, State)
addTransitionNCFA ncfa@(NCFA s0 sl g fss) (s1, l, k, s2) =
    let g' = M.insertWith (\ _ node -> (l, k, s2) : node) s1 [(l, k, s2)] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


------------------------------------------------------------------------------------------------


to_label :: String -> Label
to_label [c] = LabelChar c
to_label r   = LabelRange r


determine_node :: NCFANode -> NCFAGraph -> M.HashMap State SignSet -> M.HashMap State SignSet -> State -> (DCFANode, NCFAGraph, M.HashMap State SignSet, State, S.Set State)
determine_node n g fss_old fss s_max =
    let f :: (SignNum, State) -> M.HashMap Char ([SignNum], [State]) -> Char -> M.HashMap Char ([SignNum], [State])
        f (k, s) n c = M.insertWith (\ _ (ks, ss) -> (k : ks, s : ss)) c ([k], [s]) n
        n' = foldl'
            (\ n (l, k, s) -> case l of
                LabelChar c  -> f (k, s) n c
                LabelRange r -> foldl' (f (k, s)) n r
            ) M.empty n
        (xs, ys) = M.foldlWithKey'
            (\ (xs, ys) c (ks@(k : ks'), ss@(s : ss')) -> if ss' == []
                then (xs, M.insertWith (\ _ (r, ks'') -> (c : r, S.insert k ks'')) s ([c], S.insert k S.empty) ys)
                else
                    let ks' = S.fromList ks
                    in  (M.insertWith (\ _ (r, ks'') -> (c : r, S.union ks' ks'')) (S.fromList ss) ([c], ks') xs, ys)
            ) (M.empty, M.empty) n'
        n'' = M.foldlWithKey' (\ n s (r, ks) -> M.insert (to_label r) (ks, s) n) M.empty ys
        (n''', sss, s_max') = M.foldlWithKey' (\ (n, sss, i) ss (r, ks) -> (M.insert (to_label r) (ks, i) n, M.insert i ss sss, i + 1)) (n'', M.empty, s_max) xs
        g' = M.foldlWithKey'
            (\ g s ss ->
                let n = S.foldl' (\ n s' -> M.lookupDefault [] s' g ++ n) [] ss
                in  M.insert s n g
            ) g sss
        ss = M.foldl' (\ ss (_, s) -> S.insert s ss) S.empty n'''
        fss' = M.foldlWithKey'
            (\ fss i ss ->
                let ks = S.foldl' (\ ks s -> S.union (M.lookupDefault S.empty s fss_old) ks) S.empty ss
                in  if ks == S.empty then fss else M.insert i ks fss
            ) fss sss
    in  (n''', g', fss', s_max', ss)


instance NFData Label where
    rnf (LabelChar c)  = rnf c
    rnf (LabelRange r) = rnf r


f :: (NCFAGraph, DCFAGraph, M.HashMap State SignSet, M.HashMap State SignSet, State, S.Set State) -> (DCFAGraph, M.HashMap State SignSet)
f (_, dg, fss_old, fss,  _, ss) | ss == S.empty = (dg, fss)
f (ng, dg, fss_old, fss, s_max, ss)             = f $ S.foldl'
    (\ (ng, dg, fss_old, fss, s_max, ss) s ->
        let n = M.lookupDefault [] s ng
        in  if n /= []
                then
                    let (n', ng', fss', s_max', ss') = determine_node n ng fss_old fss s_max
                    in  (M.delete s ng', M.insert s n' dg, fss_old, fss', s_max', S.union ss' ss)
                else (ng, dg, fss_old, case M.lookup s fss_old of {Just ss -> M.insert s ss fss; _ -> fss}, s_max, ss)
    ) (ng, dg, fss_old, fss, s_max, S.empty) ss


determine :: NCFA -> DCFA
determine ncfa@(NCFA s0 sl g fss) =
    let n0                          = M.lookupDefault (error "init node absent in ncfa") s0 g
        (n0', g', fss', s_max', ss) = determine_node n0 g fss M.empty sl
        g''                         = M.delete s0 g'
        (dg, fss'')                 = f (g'', M.empty, fss, fss', s_max', ss)
    in  DCFA s0 n0' dg fss''


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
            \ (l, (ks, s')) -> appendFile fp $ concat
                [ "\t"
                , show s
                , " -> "
                , show s'
                , " [label=\""
                , show l
                , case l of
                    LabelRange _ -> "\", style=bold, color=red]\n"
                    _            -> "\"]\n"
                , show $ S.toList ks
                ]
    appendFile fp "}\n"
