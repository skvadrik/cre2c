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
import           Data.List                (foldl', intersect, find, partition, delete, nub)
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust, fromJust)

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


-- тутачки нада схитрить с диапазонами, иначе жопа
-- надо вычислять наибольшие пересекающиеся диапазоны
get_multiarcs :: Char -> NCFANode -> (Node, NCFANode)
get_multiarcs c ys =
    let (xs', ys') = partition
            (\ (l, _, _) -> case l of
                LabelChar c' -> c == c'
                LabelRange r -> c `elem` r
            ) ys
        (xs'', ys'') = foldl'
            (\ (xs, ys) (l, k, s) -> case l of
                LabelChar _     -> ((c, k, s) : xs, ys)
                LabelRange [c'] -> ((c, k, s) : xs, ys)
                LabelRange r    -> ((c, k, s) : xs, (LabelRange (delete c r), k, s) : ys)
            ) ([], []) xs'
    in  (xs'', ys'' ++ ys')


type Node = [(Char, SignNum, State)]


-- либо тут пытаться грамотно вычислять разбиение на непересекающиеся подмножества
-- либо дальше потом группировать
group_by_label :: [Node] -> NCFANode -> [Node]
group_by_label xss [] = xss
group_by_label xss ((l, k, s) : ys) = case l of
    LabelChar c  ->
        let (xs', ys') = get_multiarcs c ys
        in  group_by_label (((c, k, s) : xs') : xss) ys'
    LabelRange r ->
        let (xss', ys') = foldl'
                (\ (xss'', ys'') c ->
                    let (xs', ys') = get_multiarcs c ys''
                    in  (((c, k, s) : xs') : xss'', ys')
                ) (xss, ys) r
        in  group_by_label xss' ys'


type MultiArc = (Label, SignSet, S.Set State)

group_multiarcs_by_state :: [Node] -> [MultiArc]
group_multiarcs_by_state xss =
    let xss' = map (\ xs -> ((S.fromList . (\ (_, _, ss) -> ss) . unzip3) xs, xs)) xss

        f :: [[Node]] -> [(S.Set State, Node)] -> [[Node]]
        f xsss []                 = xsss
        f xsss ((ss, ys) : yss) =
            let (xss, yss') = partition (\ (ss', _) -> ss' == ss) yss
                xss'        = (snd . unzip) xss
            in  f ((ys : xss') : xsss) yss'

        g :: [Node] -> MultiArc
        g (xs@((c, _, _) : _) : []) =
            let (ks, ss) = foldl' (\ (ks, ss) (_, k, s) -> (S.insert k ks, S.insert s ss)) (S.empty, S.empty) xs
            in  (LabelChar c, ks, ss)
        g xss =
            let r        = map (\ ((c, _, _) : _) -> c) xss
                (ks, ss) = foldl'
                    (\ (ks, ss) xs -> foldl' (\ (ks, ss) (_, k, s) -> (S.insert k ks, S.insert s ss)) (ks, ss) xs
                    ) (S.empty, S.empty) xss
            in  (LabelRange r, ks, ss)

    in  (map g . trace'' "*" . f [] . trace'' "***") xss'


group_by_state :: [Node] -> Node -> [Node]
group_by_state xss [] = xss
group_by_state xss (y@(_, _, s) : ys) =
    let (xs', ys') = partition (\ (_, _, s') -> s' == s) ys
    in  group_by_state ((y : xs') : xss) ys'


group_by_sign :: [Node] -> Node -> [Node]
group_by_sign xss [] = xss
group_by_sign xss (y@(_, k, _) : ys) =
    let (xs', ys') = partition (\ (_, k', _) -> k' == k) ys
    in  group_by_sign ((y : xs') : xss) ys'


determine_init_node :: NCFANode -> NCFAGraph -> State -> (DCFAInitNode, NCFAGraph, State, S.Set State)
determine_init_node n g s_max =
    let (multiarcs, ranges, arcs) =
            ( (\ (xss, (uss, vss), zss) -> (xss, concat uss, (concat . concat) vss ++ zss))
            . (\ (xss, (yss, zss))      -> (xss, (unzip . (map (partition ((> 1) . length) . group_by_sign []))) yss, concat zss))
            . (\ (xss, yss)             -> (group_multiarcs_by_state xss, (partition ((> 1) . length) . group_by_state [] . concat) yss))
            . partition ((> 1) . length)
            . group_by_label []
            ) n
        n'   = foldl' (\ n (c, k, s) -> M.insert (LabelChar c) (S.insert k S.empty, s) n) M.empty arcs
        n''  = foldl'
            (\ n r ->
                let (cs, ks, ss) = unzip3 r
                in  M.insert (LabelRange cs) (S.fromList ks, case nub ss of { [s] -> s; _ -> error "multiple end states"}) n
            ) n' ranges
        (n''', sss, s_max') = foldl'
            (\ (n, sss, i) (l, ks, ss) -> (M.insert l (ks, i) n, M.insert i ss sss, i + 1)
            ) (n'', M.empty, s_max) multiarcs
{-
        (n''', sss, s_max') = foldl'
            (\ (n, sss, i) xs@((c, _, _) : _) ->
                let (ks, ss) = foldl' (\ (ks, ss) (_, k, s) -> (S.insert k ks, S.insert s ss)) (S.empty, S.empty) xs
                in  (M.insert (LabelChar c) (ks, i) n, M.insert i ss sss, i + 1)
            ) (n'', M.empty, s_max) multiarcs
-}
        g' = M.foldlWithKey'
            (\ g s ss ->
                let n = S.foldl' (\ n s' -> M.lookupDefault [] s' g ++ n) [] ss
                in  M.insert s n g
            ) g sss
        ss   = M.foldl' (\ ss (_, s) -> S.insert s ss) S.empty n'''
    in  (n''', g', s_max', ss)


determine_node :: NCFANode -> NCFAGraph -> State -> (DCFANode, NCFAGraph, State, S.Set State)
determine_node n g s_max =
    let (multiarcs, ranges, arcs) =
            ( (\ (xss, (uss, vss), zss) -> (xss, concat uss, (concat . concat) vss ++ zss))
            . (\ (xss, (yss, zss))      -> (xss, (unzip . (map (partition ((> 1) . length) . group_by_sign []))) yss, concat zss))
            . (\ (xss, yss)             -> (group_multiarcs_by_state xss, (partition ((> 1) . length) . group_by_state [] . concat) yss))
            . partition ((> 1) . length)
            . group_by_label []
            ) n
        n'   = foldl' (\ n (c, k, s) -> M.insert (LabelChar c) s n) M.empty arcs
        n''  = foldl'
            (\ n r ->
                let (cs, ks, ss) = unzip3 r
                in  M.insert (LabelRange cs) (case nub ss of { [s] -> s; _ -> error "multiple end states"}) n
            ) n' ranges
        (n''', sss, s_max') = foldl'
            (\ (n, sss, i) (l, _, ss) -> (M.insert l i n, M.insert i ss sss, i + 1)
            ) (n'', M.empty, s_max) multiarcs
{-
        (n''', sss, s_max') = foldl'
            (\ (n, sss, i) xs@((c, _, _) : _) ->
                let ss = foldl' (\ ss (_, _, s) -> S.insert s ss) S.empty xs
                in  (M.insert (LabelChar c) i n, M.insert i ss sss, i + 1)
            ) (n'', M.empty, s_max) multiarcs
-}
        g' = M.foldlWithKey'
            (\ g s ss ->
                let n = S.foldl' (\ n s' -> M.lookupDefault [] s' g ++ n) [] ss
                in  M.insert s n g
            ) g sss
        ss   = M.foldl' (\ ss s -> S.insert s ss) S.empty n'''
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
