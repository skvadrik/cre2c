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

    , show_cfa
    , ncfa_to_dot
    , dcfa_to_dot
    ) where


import qualified Data.HashMap.Strict as M hiding (lookupDefault)
import qualified Data.HashMap.Lazy   as M        (lookupDefault)
import qualified Data.Set            as S
import           Data.List                       (foldl')
import           Control.Monad                   (forM_)
import           Control.Applicative             ((<$>))
import           Data.Maybe                      (isJust)
import           System.Cmd
import           System.IO.Unsafe
import           Data.Time.Clock

import           Types               hiding (err)


new_ncfa :: IStateID -> NCFA a
new_ncfa k = NCFA k (k + 1) M.empty M.empty


dcfa_is_final :: IStateID -> DCFA a -> Bool
dcfa_is_final s (DCFA _ _ _ fss) = isJust $ M.lookup s fss


dcfa_accepted :: IStateID -> DCFA a -> S.Set IRegID
dcfa_accepted s (DCFA _ _ _ fss) = M.lookupDefault S.empty s fss


ncfa_set_final :: IStateID -> IRegID -> NCFA a -> NCFA a
ncfa_set_final s k (NCFA s0 sl g fss) = NCFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


ncfa_set_cyclic :: Labellable a => IStateID -> NCFA a -> NCFA a
ncfa_set_cyclic s1 ncfa@NCFA{ ncfa_graph = g } =
    let f  = map (\ (l, k, _, s) -> (l, k, True, s))
        g' = M.adjust f s1 g
    in  ncfa{ ncfa_graph = g' }


ncfa_add_transition :: NCFA a -> (IStateID, Label a, IRegID, IStateID) -> (NCFA a, IStateID)
ncfa_add_transition (NCFA s0 sl g fss) (s1, l, k, s2) =
    let def_arc = (l, k, False, s2)
        g'      = M.insertWith (++) s1 [def_arc] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


ncfa_union :: Labellable a => NCFA a -> NCFA a -> NCFA a
ncfa_union (NCFA s0 _ g fss) (NCFA _ sl' g' fss') =
    let insert_node g s nd = M.insertWith (++) s nd g
        g''   = M.foldlWithKey' insert_node g g'
        fss'' = M.union fss fss'
    in  NCFA s0 sl' g'' fss''


ncfa_tie_states :: Labellable a => NCFA a -> S.Set IStateID -> IStateID -> NCFA a
ncfa_tie_states (NCFA s0 sl g fss) xs y =
    let f arcs = foldl'
            (\ arcs1 (l, k, b, s') -> if S.member s' xs
                then (l, k, b, y) : arcs1
                else (l, k, b, s') : arcs1
            ) [] arcs
        g'  = M.map f g
        g'' = S.foldl' (\ g s -> M.delete s g) g' xs
    in  (NCFA s0 sl g'' fss)

------------------------------------------------------------------------------------------------

group_by_symbol :: Labellable a => NCFANode a -> M.HashMap a (S.Set IRegID, Bool, S.Set IStateID)
group_by_symbol =
    let f1 (k, b, s) n c  = M.insertWith (\ _ (ks, b', ss) -> (S.insert k ks, b || b', S.insert s ss)) c (S.singleton k, b, S.singleton s) n
        f2 n (l, k, b, s) = case l of
            LOne   x  -> f1 (k, b, s) n x
            LRange xs -> S.foldl' (f1 (k, b, s)) n xs
    in  foldl' f2 M.empty


group_by_states :: Labellable a => M.HashMap a (S.Set IRegID, Bool, S.Set IStateID) -> M.HashMap (S.Set IStateID) (Label a, S.Set IRegID, Bool)
group_by_states arcs =
    let f1 xs c (ks, b, ss) = M.insertWith (\ _ (r, ks', b') -> (c : r, S.union ks ks', b || b')) ss ([c], ks, b) xs
        arcs1 = M.foldlWithKey' f1 M.empty arcs
        arcs2 = M.map (\ (r, ks, b) -> (to_label r, ks, b)) arcs1
    in  arcs2


to_label :: Labellable a => [a] -> Label a
to_label r = case r of
    [x] -> LOne x
    xs  -> (LRange . S.fromList) xs


update_dcfa :: Labellable a => M.HashMap IStateID (S.Set IRegID) -> IStateID -> (DCFA a, M.HashMap (S.Set IStateID) IStateID, S.Set (S.Set IStateID)) -> S.Set IStateID -> (Label a, S.Set IRegID, Bool)
    -> (DCFA a, M.HashMap (S.Set IStateID) IStateID, S.Set (S.Set IStateID))
update_dcfa nfinals s (DCFA init max g dfinals, set2state, sets) set (l, ks, b) = case M.lookup set set2state of
    Just s' ->
        let g1    = M.insertWith (\ _ arcs -> M.insert l (ks, b, s') arcs) s (M.insert l (ks, b, s') M.empty) g
            dcfa1 = DCFA init max g1 dfinals
        in  (dcfa1, set2state, sets)
    Nothing ->
        let g1         = M.insertWith (\ _ arcs -> M.insert l (ks, b, max + 1) arcs) s (M.insert l (ks, b, max + 1) M.empty) g
            dfinals1   = S.foldl'
                (\ dfinals1 s -> case M.lookup s nfinals of
                    Just ks -> M.insertWith S.union (max + 1) ks dfinals1
                    Nothing -> dfinals1
                ) dfinals set
            dcfa1      = DCFA init (max + 1) g1 dfinals1
            set2state1 = M.insert set (max + 1) set2state
            sets1      = S.insert set sets
        in  (dcfa1, set2state1, sets1)


extend :: Labellable a => NCFA a -> (DCFA a, M.HashMap (S.Set IStateID) IStateID, S.Set (S.Set IStateID)) -> S.Set IStateID
    -> (DCFA a, M.HashMap (S.Set IStateID) IStateID, S.Set (S.Set IStateID))
extend (NCFA _ _ ngraph nfinals) (dcfa, set2state, sets) set =
    let s         = M.lookupDefault (err "can't find new state") set set2state
        arcs      = S.foldl (\ arcs s -> M.lookupDefault [] s ngraph ++ arcs) [] set
        multiarcs = (group_by_states . group_by_symbol) arcs
    in  M.foldlWithKey' (update_dcfa nfinals s) (dcfa, set2state, sets) multiarcs


determine_ :: Labellable a => NCFA a -> DCFA a -> M.HashMap (S.Set IStateID) IStateID -> S.Set (S.Set IStateID) -> DCFA a
determine_ _    dcfa _         sets | sets == S.empty = dcfa
determine_ ncfa dcfa set2state sets =
    let (dcfa', set2state', sets') = S.foldl' (extend ncfa) (dcfa, set2state, S.empty) sets
    in  determine_ ncfa dcfa' set2state' sets'


determine :: Labellable a => NCFA a -> DCFA a
determine ncfa@(NCFA init _ _ nfinals) =
    let init_set  = S.singleton init
        dfinals   = case M.lookup init nfinals of
            Just ks -> M.insert 0 ks M.empty
            Nothing -> M.empty
        set2state = M.insert init_set 0 M.empty
        sets      = S.singleton init_set
        dcfa      = DCFA 0 0 M.empty dfinals
    in  determine_ ncfa dcfa set2state sets

------------------------------------------------------------------------------------------------

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
        \ (s, arcs) -> do
            forM_ arcs $
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
dcfa_to_dot (DCFA _ _ g fss) fp = do
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


show_cfa :: Labellable a => NCFA a -> NCFA a
show_cfa ncfa =
    let io = unsafePerformIO $
            show . utctDayTime <$> getCurrentTime >>= \ filename ->
            ncfa_to_dot ncfa ("pic/ncfa_" ++ filename ++ ".dot") >>
            system ("dot -Tpng -opic/ncfa_" ++ filename ++ ".png pic/ncfa_" ++ filename ++ ".dot") >>
            return ()
    in  io `seq` ncfa



