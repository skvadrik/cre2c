{-# LANGUAGE BangPatterns #-}

module CFA
    ( new_ncfa

    , dcfa_is_final
    , dcfa_accepted

    , ncfa_set_final
    , ncfa_set_cyclic
    , ncfa_add_transition
    , ncfa_tie_states
    , ncfa_union

    , determine

    , ncfa_to_dot
    , dcfa_to_dot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl', nub)
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust, fromJust)
import Control.DeepSeq

import           Types               hiding (err)


new_ncfa :: IStateID -> NCFA a
new_ncfa k = NCFA k (k + 1) M.empty M.empty


dcfa_is_final :: IStateID -> DCFA a -> Bool
dcfa_is_final s (DCFA _ _ fss) = isJust $ M.lookup s fss


dcfa_accepted :: IStateID -> DCFA a -> S.Set IRegID
dcfa_accepted s (DCFA _ _ fss) = M.lookupDefault S.empty s fss


ncfa_set_final :: IStateID -> IRegID -> NCFA a -> NCFA a
ncfa_set_final s k (NCFA s0 sl g fss) = NCFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


ncfa_set_cyclic :: Labellable a => IStateID -> NCFA a -> NCFA a
ncfa_set_cyclic s1 ncfa@NCFA{ ncfa_graph = g } =
    let f  = map (\ (l, k, _, s) -> (l, k, True, s))
        g' = M.adjust (\ nd -> f nd) s1 g
    in  ncfa{ ncfa_graph = g' }


ncfa_add_transition :: NCFA a -> (IStateID, Label a, IRegID, IStateID) -> (NCFA a, IStateID)
ncfa_add_transition (NCFA s0 sl g fss) (s1, l, k, s2) =
    let g' = M.insertWith (\ _ node -> (l, k, False, s2) : node) s1 [(l, k, False, s2)] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


ncfa_union :: Labellable a => NCFA a -> NCFA a -> NCFA a
ncfa_union (NCFA s0 sl g fss) (NCFA s0' sl' g' fss') =
    let insert_node g s nd = M.insertWith (\ nd1 nd2 -> force (nd1 ++ nd2)) s nd g
        g''   = M.foldlWithKey' insert_node g g'
        fss'' = M.union fss fss'
    in  if s0' < sl
            then err "ncfa_union : initial state of second NCFA is less than final state of the first one."
            else NCFA s0 sl' g'' fss''


ncfa_tie_states :: Labellable a => NCFA a -> S.Set IStateID -> S.Set IStateID -> NCFA a
ncfa_tie_states (NCFA s0 sl g fss) xs ys =
    let f :: NCFANode a -> NCFANode a
        f n = foldl'
            (\ n' (l, k, b, s) -> if S.member s xs
                then map (\ y -> (l, k, b, y)) (S.toList ys) ++ n'
                else (l, k, b, s) : n'
            ) [] n
        g' = M.map f g
        g'' = S.foldl' (\ g x -> M.delete x g) g' xs
    in  (NCFA s0 sl g'' fss)


------------------------------------------------------------------------------------------------


group_by_symbol :: Labellable a => NCFANode a -> M.HashMap a ([IRegID], Bool, [IStateID])
group_by_symbol =
    let f1 (k, b, s) n c  = M.insertWith (\ _ (ks, b', ss) -> (k : ks, b || b', s : ss)) c ([k], b, [s]) n
        f2 n (l, k, b, s) = case l of
            LOne   x  -> f1 (k, b, s) n x
            LRange xs -> foldl' (f1 (k, b, s)) n xs
    in  foldl' f2 M.empty


partition_arcs :: M.HashMap a ([IRegID], Bool, [IStateID]) -> (M.HashMap (IStateID, Bool) ([a], S.Set IRegID), M.HashMap (S.Set IStateID, Bool) ([a], S.Set IRegID))
partition_arcs arcs =
    let f1 (xs, ys) c (ks, b, ss) = case (ks, nub ss) of
            ([],  _  ) -> err "partition_arcs : empty id-list on arc"
            (_,   [] ) -> err "partition_arcs : empty state-list on arc"
            ([k], [s]) ->
                let xs' = M.insertWith (\ _ (r, ks') -> (c : r, S.insert k ks')) (s, b) ([c], S.insert k S.empty) xs
                in  (xs', ys)
            (_,   [s]) ->
                let ks' = S.fromList ks
                    xs' = M.insertWith (\ _ (r, ks'') -> (c : r, S.union ks' ks'')) (s, b) ([c], ks') xs
                in  (xs', ys)
            _          ->
                let ks' = S.fromList ks
                    ys' = M.insertWith (\ _ (r, ks'') -> (c : r, S.union ks' ks'')) (S.fromList ss, b) ([c], ks') ys
                in  (xs, ys')
    in  M.foldlWithKey' f1 (M.empty, M.empty) arcs


tie_multiarcs :: IStateID -> M.HashMap (S.Set IStateID, Bool) ([a], S.Set IRegID)
    -> (M.HashMap (IStateID, Bool) ([a], S.Set IRegID), M.HashMap IStateID (S.Set IStateID), IStateID)
tie_multiarcs max =
    let f (arcs, new2olds, new) (olds, b) node =
            ( M.insert (new, b) node arcs
            , M.insert new olds new2olds
            , new + 1
            )
    in  M.foldlWithKey' f (M.empty, M.empty, max)


to_dcfa_node :: Labellable a => M.HashMap (IStateID, Bool) ([a], S.Set IRegID) -> DCFANode a
to_dcfa_node =
    let to_label r = case r of
            [x] -> LOne x
            xs  -> (LRange . S.toList . S.fromList) xs
        f nd (s, b) (r, ids) = M.insert (to_label r) (ids, b, s) nd
    in  M.foldlWithKey' f M.empty


update_ncfa_graph :: Labellable a => M.HashMap IStateID (S.Set IRegID) -> NCFAGraph a -> NCFAGraph a
update_ncfa_graph new2olds graph =
--    let f1 g s    = M.lookupDefault (err "update_ncfa_graph : missing node") s g
    let f1 nd s ss = foldl' (\ xs (l, id, b, s') -> (if s' `S.member` ss then (l, id, b, s) else (l, id, b, s')) : xs) [] nd
        f2 g s nd =
            let nd' = M.foldlWithKey' f1 nd new2olds
            in  M.adjust (\ _ -> nd') s g
        graph'    = M.foldlWithKey' f2 graph graph
        f3 g s    = M.lookupDefault [] s g
        f4 g s ss =
            let nd = (concatMap (f3 g) . S.toList) ss
            in  M.insert s nd g
    in  M.foldlWithKey' f4 graph' new2olds

-- удалить списки сигнатур
-- сделать двунапрвленным
-- повставлять строгие фолды
-- где можно, работать с подграфами


update_fstates :: M.HashMap IStateID (S.Set IRegID) -> M.HashMap IStateID (S.Set IStateID) -> M.HashMap IStateID (S.Set IRegID) -> M.HashMap IStateID (S.Set IRegID)
update_fstates nfinals new2olds dfinals =
--    let f1 s = M.lookupDefault (err "update_fstates : missing final state") s old_fstates
    let f1 s = M.lookupDefault S.empty s nfinals
        f2 states new olds =
            let ids = (S.unions . map f1 . S.toList) olds
            in  if ids == S.empty
                    then states
                    else M.insert new ids states
    in  M.foldlWithKey' f2 dfinals new2olds


determine_node :: Labellable a => NCFA a -> DCFA a -> IStateID -> (NCFA a, DCFA a, S.Set IStateID)
determine_node ncfa@(NCFA ns0 max ngraph nfinals) dcfa@(DCFA ds0 dgraph dfinals) s =
    let nnode                   = M.lookup s ngraph
        symbol2arcs             = group_by_symbol (fromJust nnode)
        (arcs1, multiarcs)      = partition_arcs symbol2arcs
        (arcs2, new2olds, max') = tie_multiarcs max multiarcs
        dnode                   = to_dcfa_node (M.union arcs1 arcs2)
-- это неправильно. что если были ссылки на этот узел
        ngraph'                 = (M.delete s . update_ncfa_graph new2olds) ngraph
        dgraph'                 = M.insert s dnode dgraph
        dfinals'                = update_fstates nfinals new2olds dfinals
        ncfa'                   = NCFA ns0 max' ngraph' nfinals
        dcfa'                   = DCFA ds0 dgraph' dfinals'
        states_to_determine     = (S.fromList . (\ (_, _, x) -> x) . unzip3 . M.elems) dnode
        (ncfa'', dcfa''@DCFA{dcfa_final_states = finals}, states_to_determine') = case nnode of
            Just _  -> (ncfa', dcfa', states_to_determine)
            Nothing -> (ncfa,  dcfa,  S.empty)
    in  case M.lookup s nfinals of
            Just ids -> (ncfa'', dcfa''{dcfa_final_states = M.insert s ids finals}, states_to_determine')
            Nothing  -> (ncfa'', dcfa'', states_to_determine')


determine :: (Labellable a) => NCFA a -> DCFA a
determine ncfa@(NCFA s0 _ _ _) =
    let f1 (ncfa1, dcfa1, states1) s1 =
            let (ncfa2, dcfa2, states2) = determine_node ncfa1 dcfa1 s1
            in  (ncfa2, dcfa2, S.union states1 states2)
        f2 (ncfa1, dcfa1, states1) = if states1 == S.empty
            then dcfa1
            else f2 $ S.foldl' f1 (ncfa1, dcfa1, S.empty) states1
        dcfa   = DCFA s0 M.empty M.empty
        states = S.insert s0 S.empty
    in  f2 (ncfa, dcfa, states)


ncfa_to_dot :: (Labellable a) => NCFA a -> FilePath -> IO ()
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
            \ (l, k, b, s') -> appendFile fp $ concat
                [ "\t"
                , show s
                , " -> "
                , show s'
                , " [label=\""
                , show l
                , ", "
                , show k
                , "\""
                , case b of
                    True  -> ", style=bold, color=blue"
                    False -> ""
                , "]\n"
                ]
    appendFile fp "}\n"


dcfa_to_dot :: (Labellable a) => DCFA a -> FilePath -> IO ()
dcfa_to_dot (DCFA _ g fss) fp = do
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
            \ (l, (ks, b, s')) -> appendFile fp $ concat
                [ "\t"
                , show s
                , " -> "
                , show s'
                , " [label=\""
                , show l
                , ", "
                , show $ S.toList ks
                , "\""
{-
                , case l of
                    LRange _ -> ", style=bold, color=red"
                    _        -> ""
-}
                , case b of
                    True  -> ", style=bold, color=blue"
                    False -> ""
                , "]\n"
                ]
    appendFile fp "}\n"


err :: String -> a
err s = error $ "*** CFA : " ++ s


