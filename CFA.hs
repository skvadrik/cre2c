{-# LANGUAGE BangPatterns #-}

module CFA
    ( new_ncfa

    , dcfa_is_final
    , dcfa_accepted

    , ncfa_set_final
    , ncfa_set_cyclic
    , ncfa_set_all_cyclic
    , ncfa_add_transition
    , ncfa_tie_states
    , ncfa_union

    , determine

    , ncfa_to_dot
    , dcfa_to_dot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl', intercalate, partition)
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust)
import Control.DeepSeq
import System.IO.Unsafe
import System.Cmd

import           Types               hiding (err)
import           Helpers


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


ncfa_set_all_cyclic :: Labellable a => NCFA a -> NCFA a
ncfa_set_all_cyclic ncfa@NCFA{ ncfa_graph = g } =
    let f  = map (\ (l, id, _, s) -> (l, id, True, s))
        g' = M.map f g
    in  ncfa{ ncfa_graph = g' }


ncfa_add_transition :: NCFA a -> (IStateID, Label a, IRegID, IStateID) -> (NCFA a, IStateID)
ncfa_add_transition (NCFA s0 sl g fss) (s1, l, k, s2) =
    let g' = M.insertWith (\ _ node -> (l, k, False, s2) : node) s1 [(l, k, False, s2)] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


ncfa_union :: Labellable a => NCFA a -> NCFA a -> NCFA a
ncfa_union (NCFA s0 _ g fss) (NCFA _ sl' g' fss') =
    let insert_node g s nd = M.insertWith (\ nd1 nd2 -> force (nd1 ++ nd2)) s nd g
        g''   = M.foldlWithKey' insert_node g g'
        fss'' = M.union fss fss'
    in  NCFA s0 sl' g'' fss''


ncfa_tie_states :: Labellable a => NCFA a -> S.Set IStateID -> IStateID -> NCFA a
ncfa_tie_states (NCFA s0 sl g fss) xs y =
    let f n = foldl'
            (\ n' (l, k, b, s) -> if S.member s xs
                then (l, k, b, y) : n'
                else (l, k, b, s) : n'
            ) [] n
        g' = M.map f g
    in  (NCFA s0 sl g' fss)


------------------------------------------------------------------------------------------------


group_by_symbol :: Labellable a => NCFANode a -> M.HashMap a (S.Set IRegID, Bool, S.Set IStateID)
group_by_symbol =
    let f1 (k, b, s) n c  = M.insertWith (\ _ (ks, b', ss) -> (S.insert k ks, b || b', S.insert s ss)) c (S.insert k S.empty, b, S.insert s S.empty) n
        f2 n (l, k, b, s) = case l of
            LOne   x  -> f1 (k, b, s) n x
            LRange xs -> S.foldl' (f1 (k, b, s)) n xs
    in  foldl' f2 M.empty


partition_arcs :: Labellable a => M.HashMap a (S.Set IRegID, Bool, S.Set IStateID)
    -> (M.HashMap (IStateID, Bool) ([a], S.Set IRegID), M.HashMap (S.Set IStateID, Bool) ([a], S.Set IRegID))
partition_arcs arcs =
    let f1 (xs, ys) c (ks, b, ss) = if S.size ss == 1
            then
                let s = (head . S.toList) ss
                    xs' = M.insertWith (\ _ (r, ks') -> (c : r, S.union ks ks')) (s, b) ([c], ks) xs
                in  (xs', ys)
            else
                let ys' = M.insertWith (\ _ (r, ks') -> (c : r, S.union ks ks')) (ss, b) ([c], ks) ys
                in  (xs, ys')
    in  M.foldlWithKey' f1 (M.empty, M.empty) arcs


tie_multiarcs :: Labellable a => IStateID -> M.HashMap (S.Set IStateID, Bool) ([a], S.Set IRegID)
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
            xs  -> (LRange . S.fromList) xs
        f nd (s, b) (r, ids) = M.insert (to_label r) (ids, b, s) nd
    in  M.foldlWithKey' f M.empty


-- 1 не дублировать те циклы, в которые ведут только мержимые дуги, а просто перенаправлять их.
-- 2 не генерить проверки для всех реверсных дуг
duplicate_arcs :: Labellable a => (NCFAGraph a, IStateID) -> M.HashMap IStateID IStateID -> S.Set IStateID -> IStateID -> IStateID -> (NCFAGraph a, IStateID)
duplicate_arcs (g, max) old2new fixed s1 s2 =
    let span_noncyclic g (l, k, b, s) = if s == s1
            then M.insertWith (++) s2 [(l, k, b, s2)] g
            else M.insertWith (++) s2 [(l, k, b, s)] g
        span_cyclic (g, max) (l, k, b, s) = case (M.lookup s old2new, S.member s fixed) of
            (Just s', _    ) ->
                 let g1 = M.insertWith (++) s2 [(l, k, b, s')] g
                 in  (g1, max)
            (Nothing, True ) ->
                 let g1 = M.insertWith (++) s2 [(l, k, b, s)] g
                 in  (g1, max)
            (Nothing, False) ->
                 let g1 = M.insertWith (++) s2 [(l, k, b, max)] g
                 in  duplicate_arcs (g1, max + 1) (M.insert s max old2new) fixed s max
    in  case M.lookup s1 g of
            Just nd ->
                let (rev_arcs, arcs) = partition (\ (_, _, b, _) -> b) nd
                    g1         = foldl' span_noncyclic g arcs
                    (g2, max') = foldl' span_cyclic (g1, max) rev_arcs
                in  (g2, max')
            Nothing -> (g, max)


duplicate_self ::  Labellable a => (NCFAGraph a, IStateID) -> S.Set IStateID -> IStateID -> IStateID -> (NCFAGraph a, IStateID)
duplicate_self (g, max) olds old new =
    let f arcs (l, k, b, s) = case s of
            _ | s == old        -> (l, k, b, new) : arcs
            _ | S.member s olds -> arcs
            _                   -> (l, k, b, s) : arcs
        arcs1 = M.lookupDefault [] old g
        arcs2 = foldl' f [] arcs1
        g1    = M.insertWith (++) new arcs2 g
    in  (g1, max)


update_ncfa_graph :: Labellable a => IStateID -> M.HashMap IStateID (S.Set IRegID) -> S.Set IStateID -> NCFA a -> NCFA a
update_ncfa_graph s new2olds fixed (NCFA init max graph finals) =
    let f1 new olds (g, max) old = if old == s
            then duplicate_self (g, max) olds old new
            else
                let old2new = S.foldl' (\ m old -> M.insert old new m) M.empty olds
                in  duplicate_arcs (g, max) old2new fixed old new
        f2 (g, max) new olds = S.foldl' (f1 new olds) (g, max) olds
        (graph', max') = M.foldlWithKey' f2 (graph, max) new2olds
    in  NCFA init max' graph' finals


update_fstates :: M.HashMap IStateID (S.Set IRegID) -> M.HashMap IStateID (S.Set IStateID) -> M.HashMap IStateID (S.Set IRegID) -> M.HashMap IStateID (S.Set IRegID)
update_fstates nfinals new2olds dfinals =
    let f1 s = M.lookupDefault S.empty s nfinals
        f2 states new olds =
            let ids = (S.unions . map f1 . S.toList) olds
            in  if ids == S.empty
                    then states
                    else M.insert new ids states
    in  M.foldlWithKey' f2 dfinals new2olds


determine_node :: Labellable a => NCFA a -> DCFA a -> IStateID -> (NCFA a, DCFA a, S.Set IStateID)
determine_node ncfa@(NCFA ns0 max ngraph nfinals) dcfa@(DCFA ds0 dgraph dfinals) s =
    let nnode                   = M.lookupDefault [] s ngraph
        symbol2arcs             = group_by_symbol nnode
        (arcs1, multiarcs)      = partition_arcs symbol2arcs
        (arcs2, new2olds, max') = tie_multiarcs max multiarcs
        dnode                   = to_dcfa_node (M.union arcs1 arcs2)
        dgraph'                 = M.insert s dnode dgraph
        fixed                   = (S.insert s . S.fromList . M.keys) dgraph'
        ncfa'                   = update_ncfa_graph s new2olds fixed (NCFA ns0 max' ngraph nfinals)
        nfinals'                = ncfa_final_states ncfa'
        dfinals'                = update_fstates nfinals' new2olds dfinals
        dcfa'                   = DCFA ds0 dgraph' dfinals'
        states_to_determine     = (S.fromList . (\ (_, _, x) -> x) . unzip3 . M.elems) dnode
        (ncfa'', dcfa''@DCFA{dcfa_final_states = finals}, states_to_determine') = case nnode of
            [] -> (ncfa,  dcfa,  S.empty)
            _  -> (ncfa', dcfa', states_to_determine)
    in  case M.lookup s nfinals' of
            Just ids -> (ncfa'', dcfa''{dcfa_final_states = M.insert s ids finals}, states_to_determine')
            Nothing  -> (ncfa'', dcfa'', states_to_determine')


show_cfa :: Labellable a => (NCFA a, DCFA a, S.Set IStateID) -> (NCFA a, DCFA a, S.Set IStateID)
show_cfa (ncfa, dcfa, states) =
    let filename = (intercalate "_" . map show . S.toList) states
        ncfa1    = unsafePerformIO $
            ncfa_to_dot ncfa ("pic/ncfa_" ++ filename ++ ".dot") >>
            system ("dot -Tpng -opic/ncfa_" ++ filename ++ ".png pic/ncfa_" ++ filename ++ ".dot") >>
            return ncfa
        dcfa1    = unsafePerformIO $
            dcfa_to_dot dcfa ("pic/dcfa_" ++ filename ++ ".dot") >>
            system ("dot -Tpng -opic/dcfa_" ++ filename ++ ".png pic/dcfa_" ++ filename ++ ".dot") >>
            return dcfa
    in  (ncfa1, dcfa1, states)


determine :: (Labellable a) => NCFA a -> DCFA a
determine ncfa@(NCFA s0 _ _ _) =
    let f1 (ncfa1, dcfa1, states1) s1 =
            let (ncfa2, dcfa2, states2) = determine_node ncfa1 dcfa1 s1
                states3                 = S.difference (S.union states1 states2) ((S.fromList . M.keys) (dcfa_graph dcfa2))
--            in  show_cfa (ncfa2, dcfa2, states3)
            in  (ncfa2, dcfa2, states3)
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


