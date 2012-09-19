module CFA
    ( NCFA
    , DCFA
--    , CFANode
    , NCFALabel
    , DCFALabel
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
    , acceptedSignatures

    , setFinal
    , addTransitionNCFA
    , determine

    , toDotNCFA
    , toDotDCFA
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl', intersect, find)
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


acceptedSignatures :: State -> DCFA -> SignSet
acceptedSignatures s (DCFA _ _ _ fss) = M.lookupDefault (S.empty) s fss


setFinal :: State -> SignNum -> NCFA -> NCFA
setFinal s k (NCFA s0 sl g fss) = NCFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


addTransitionNCFA :: NCFA -> (State, NCFALabel, SignNum, State) -> (NCFA, State)
addTransitionNCFA ncfa@(NCFA s0 sl g fss) (s1, l, k, s2) =
    let g'  = M.insertWith (\ _ node -> (l, k, s2) : node) s1 [(l, k, s2)] g
    in  (NCFA s0 (max (sl + 1) s2) g' fss, s2)


add_monoarc :: (DCFANode, NCFAGraph) -> (NCFALabel, SignNum, State) -> (DCFANode, NCFAGraph)
add_monoarc (n', g') (LabelChar c, k, s) -> case M.lookup c n' of
    Nothing       -> (M.insert c (S.insert k S.empty, s) n', g')
    Just (ks, s') ->
        ( M.adjust (\ _ -> (S.insert k ks, s')) с n'
        , M.delete s $ if s == s' then g' else M.adjust (++ M.lookupDefault (error "node absent in ncfa") s g') s' g'
        )


g :: (DCFANode, NCFAGraph) -> (Char, SignNum, State) -> (DCFANode, NCFAGraph)
g n (c, k, s) -> case M.lookup c n of
    Nothing       -> n
    Just (ks, s') ->
        ( M.adjust (\ _ -> (S.insert k ks, s')) с n'
        , M.delete s $ if s == s' then g' else M.adjust (++ M.lookupDefault (error "node absent in ncfa") s g') s' g'
        )


f :: DCFANode -> [(NCFALabel, SignNum, State)] -> DCFANode
f (n', g') (c, k, s) -> case M.lookup c n' of
    Nothing       -> (M.insert c (S.insert k S.empty, s) n', g')
    Just (ks, s') ->
        ( M.adjust (\ _ -> (S.insert k ks, s')) с n'
        , M.delete s $ if s == s' then g' else M.adjust (++ M.lookupDefault (error "node absent in ncfa") s g') s' g'
        )


add_polyarcs :: (DCFANode, NCFAGraph) -> [(NCFALabel, SignNum, State)] -> (DCFANode, NCFAGraph)
add_polyarcs (n', g') polyarcs ->


multiarcs :: NCFANode -> NCFANode -> NCFANode
multiarcs xs []       = xs
multiarcs xs ((LabelChar c, k, s) : ys) =
    let f (xs, (LabelChar c, k, s) : ys) = foldl'
            (\ (l, _, _) -> case l of
                LabelChar c' -> c == c'
                LabelRange r -> c `elem` r
            ) ys
        (xs'', ys'') = 

 multiarcs (if foldl' ()  then else xs) ys


determine_node :: (NCFANode, NCFAGraph) -> (DCFANode, DCFAGraph)
determine_node n g =
    let multiarcs = foldl'
            (\ (xs, ys) (l, k, s) -> case l of
                LabelChar c  -> (foldl' () [] ys ++ xs, 
                LabelRange r ->
            ) ([], n) n
        n' = foldl'
        (\ n'' (l, k, s) -> 
        ) M.empty n


determine :: NCFA -> DCFA
determine ncfa@(NCFA s0 _ g fss) =
    let n0                   = M.lookupDefault (error "init node absent in ncfa") s0 g
        (monoarcs, polyarcs) = partition (\ (l, _, _) -> case l of { LabelChar _ -> True; _ -> False }) n0
        (n0', g')            = foldl' add_monoarc (M.empty, g) monoarcs
        (n0'', g'')          = add_polyarcs (n0', g') polyarcs
        g'' = M.delete s0 g'
    in  DCFA s0 n0 g' fss
{-
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
        in  (CFA s0 sl g' fss, S.insert ((snd . fromJust) mb_node) S.empty)
    Just node -> case l of
        LabelChar c  ->
            let s'' = case filter (\ (l', (_, s')) -> case l' of { LabelRange r | c `elem` r -> True; _ -> False }) (M.toList node) of
                    [(l', (_, s'))] -> s'
                    []              -> s2
                    _               -> error "multiple ranges"
                g'  = M.adjust (\ node -> M.insert l (S.insert k S.empty, s'') node) s1 g
                g'' = M.insertWith (\ _ node -> node) s'' M.empty g'
            in  (CFA s0 (max (sl + 1) s'') g'' fss, S.insert s'' S.empty)
        LabelRange r ->
            let (g', ss, r') = M.foldlWithKey'
                    (\ (g', ss, r') l' (_, s') -> case l' of
                        LabelChar c | c `elem` r ->
                            ( M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
                            , S.insert s' ss
                            , filter (/= c) r'
                            )
                        LabelRange r'' | let r''' = intersect r' r'', r''' /= [] ->
                            ( M.adjust (\ node -> M.insert (LabelRange r''') (S.insert k S.empty, s') node) s1 g'
                            , S.insert s' ss
                            , filter (`notElem` r'') r'
                            )
                        _ -> (g', ss, r')
                    ) (g, S.empty, r) node
                (g'', ss') = if r' == [] then (g', ss) else
                    let g''  = M.adjust (\ node -> M.insert (LabelRange r') (S.insert k S.empty, s2) node) s1 g'
                        g''' = M.insertWith (\ _ node -> node) s2 M.empty g''
                    in  (g''', S.insert s2 ss)
            in  (CFA s0 (max (sl + 1) s2) g'' fss, ss')
    Nothing ->
        let g'  = M.insert s1 (M.insert l (S.insert k S.empty, s2) M.empty) g
            g'' = M.insert s2 M.empty g'
        in  (CFA s0 (max (sl + 1) s2) g'' fss, S.insert s2 S.empty)
-}


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
            , "\"]\n"
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
                , "\"]\n"
                ]
    appendFile fp "}\n"
