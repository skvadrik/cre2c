{-# LANGUAGE BangPatterns #-}

module CFA
    ( ncfa_empty

    , dcfa_is_final
    , dcfa_accepted

    , ncfa_set_final
    , ncfa_add_transition

    , determine

    , ncfa_to_dot
    , dcfa_to_dot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl')
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust)

import           Types


ncfa_empty :: NCFA
ncfa_empty = NCFA 0 1 M.empty M.empty


dcfa_is_final :: State -> DCFA -> Bool
dcfa_is_final s (DCFA _ _ fss) = isJust $ M.lookup s fss


dcfa_accepted :: State -> DCFA -> S.Set Id
dcfa_accepted s (DCFA _ _ fss) = M.lookupDefault S.empty s fss


ncfa_set_final :: State -> Id -> NCFA -> NCFA
ncfa_set_final s k (NCFA s0 sl g fss) = NCFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


ncfa_add_transition :: NCFA -> (State, Label, Id, State) -> (NCFA, State)
ncfa_add_transition ncfa@(NCFA s0 sl g fss) (s1, l, k, s2) =
    let g' = M.insertWith (\ _ node -> (l, k, s2) : node) s1 [(l, k, s2)] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


------------------------------------------------------------------------------------------------


to_label :: String -> Label
to_label [c] = LabelChar c
to_label r   = LabelRange r


determine_node :: NCFANode -> NCFAGraph -> M.HashMap State (S.Set Id) -> M.HashMap State (S.Set Id) -> State -> (DCFANode, NCFAGraph, M.HashMap State (S.Set Id), State, S.Set State)
determine_node n g fss_old fss s_max =
    let f :: (Id, State) -> M.HashMap Char ([Id], [State]) -> Char -> M.HashMap Char ([Id], [State])
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


f :: (NCFAGraph, DCFAGraph, M.HashMap State (S.Set Id), M.HashMap State (S.Set Id), State, S.Set State) -> (DCFAGraph, M.HashMap State (S.Set Id))
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
determine ncfa@(NCFA s0 s_max g fss) =
    let (dg, fss') = f (g, M.empty, fss, M.empty, s_max, S.insert s0 S.empty)
    in  DCFA s0 dg fss'


ncfa_to_dot :: NCFA -> FilePath -> IO ()
ncfa_to_dot (NCFA _ _ g fss) fp = do
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


dcfa_to_dot :: DCFA -> FilePath -> IO ()
dcfa_to_dot (DCFA s0 g fss) fp = do
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
