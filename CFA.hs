module CFA
    ( CFA
    , CFANode
    , Label
    , State
    , SignNum
    , SignSet

    , emptyCFA

    , initialState
    , initialNode
    , maxStateNumber
    , cfaGraph
    , isFinal
    , acceptedSignatures

    , setFinal
    , addTransition

    , toDot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl', intersect)
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust, fromJust)

import           Types


emptyCFA :: CFA
emptyCFA = CFA 0 1 M.empty M.empty

initialState :: CFA -> State
initialState = initial_state

initialNode :: CFA -> CFANode
initialNode (CFA st0 _ g _) = M.lookupDefault undefined st0 g

maxStateNumber :: CFA -> State
maxStateNumber = max_state_number

cfaGraph :: CFA -> CFAGraph
cfaGraph = cfa_graph

isFinal :: State -> CFA -> Bool
isFinal st cfa = isJust $ M.lookup st (final_states cfa)

acceptedSignatures :: State -> CFA -> SignSet
acceptedSignatures s (CFA _ _ _ fss) = M.lookupDefault (S.empty) s fss




setFinal :: State -> SignNum -> CFA -> CFA
setFinal s k (CFA s0 sl g fss) = CFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)

addTransition :: CFA -> (State, Label, SignNum, State) -> (CFA, S.Set State)
addTransition cfa@(CFA s0 sl g fss) (s1, l@(LabelRange r), k, s2) = case M.lookup s1 g of
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
        in  (CFA s0 sl g' fss, S.insert ((snd . fromJust) mb_node) S.empty)
    Just node ->
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
            (g'', ss') = case r' of
                "" -> (g', ss)
                r' ->
                    ( M.adjust (\ node -> M.insert (LabelRange r') (S.insert k S.empty, s2) node) s1 g'
                    , S.insert s2 ss
                    )
            g''' = M.insertWith (\ _ node -> node) s2 M.empty g''
-- чиво это тут s2?
        in  (CFA s0 (max (sl + 1) s2) g''' fss, ss')
    Nothing ->
        let g'  = M.insert s1 (M.insert l (S.insert k S.empty, s2) M.empty) g
            g'' = M.insert s2 M.empty g'
        in  (CFA s0 (max (sl + 1) s2) g'' fss, S.insert s2 S.empty)

-- ещё ж везде повставлять эту хрень

addTransition cfa@(CFA s0 sl g fss) (s1, l@(LabelChar c), k, s2) = case M.lookup s1 g of
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
        in  (CFA s0 sl g' fss, S.insert ((snd . fromJust) mb_node) S.empty)
    Just node ->
        let s'' = case filter (\ (l', (_, s')) -> case l' of { LabelRange r | c `elem` r -> True; _ -> False }) (M.toList node) of
                [(l', (_, s'))] -> s'
                []              -> s2
                _               -> error "multiple ranges"
            g'  = M.adjust (\ node -> M.insert l (S.insert k S.empty, s'') node) s1 g
            g'' = M.insertWith (\ _ node -> node) s'' M.empty g'
        in  (CFA s0 (max (sl + 1) s'') g'' fss, S.insert s'' S.empty)
    Nothing ->
        let g'  = M.insert s1 (M.insert l (S.insert k S.empty, s2) M.empty) g
            g'' = M.insert s2 M.empty g'
        in  (CFA s0 (max (sl + 1) s2) g'' fss, S.insert s2 S.empty)
{-
        let ks = S.insert k S.empty
            g'  = M.insertWith (\ _ node -> M.insert l (ks, s2) node) s1 (M.insert l (ks, s2) M.empty) g
            g'' = M.insertWith (\ _ node -> node) s2 M.empty g'
        in  (CFA s0 (max (sl + 1) s2) g'' fss, S.insert s2 S.empty)
-}




toDot :: CFA -> FilePath -> IO ()
toDot (CFA _ _ g fss) fp = do
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
            \ (l, (signs, s')) -> appendFile fp $ concat
                [ "\t"
                , show s
                , " -> "
                , show s'
                , " [label=\""
                , show l
                , show $ S.toList signs
                , "\"]\n"
                ]
    appendFile fp "}\n"
