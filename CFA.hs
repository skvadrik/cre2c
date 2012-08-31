{-# LANGUAGE BangPatterns #-}

module CFA
    ( CFA
    , CFANode
    , Label
    , State
    , SignNum
    , SignSet

    , emptyCFA

    , initialState
    , maxStateNumber
    , bindableState
    , isFinal
    , finalStates
    , cfaGraph
    , acceptedSignatures
    , initialNode

    , setFinal
    , setBindable
    , addTransition

    , toDot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl', nub)
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust, fromJust)

import Debug.Trace


type State    = Int
type Label    = Char
type SignNum  = Int
type SignSet  = S.Set SignNum

type CFANode  = M.HashMap Label (SignSet, State)
type CFAGraph = M.HashMap State CFANode
data CFA      = CFA
    { initial_state    :: State
    , max_state_number :: State
    , bindable_state   :: Maybe State
    , cfa_graph        :: CFAGraph
    , final_states     :: M.HashMap State SignSet
    } deriving (Show)




emptyCFA :: CFA
emptyCFA = CFA 0 1 Nothing M.empty M.empty

initialState :: CFA -> State
initialState = initial_state

maxStateNumber :: CFA -> State
maxStateNumber = max_state_number

bindableState :: CFA -> Maybe State
bindableState = bindable_state

acceptedSignatures :: State -> CFA -> SignSet
acceptedSignatures s (CFA _ _ _ _ fss) = M.lookupDefault S.empty s fss

finalStates :: CFA -> M.HashMap State SignSet
finalStates = final_states

cfaGraph :: CFA -> CFAGraph
cfaGraph = cfa_graph

initialNode :: CFA -> CFANode
initialNode (CFA st0 _ _ g _) = M.lookupDefault undefined st0 g




isFinal :: State -> CFA -> Bool
isFinal st cfa = isJust $ M.lookup st (final_states cfa)




setFinal :: State -> SignNum -> CFA -> CFA
setFinal s k (CFA s0 sl st g fss) = CFA s0 sl st g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)

setBindable :: Maybe State -> CFA -> CFA
setBindable sb (CFA s0 sl _ g fss) = CFA s0 sl sb g fss

addTransition :: CFA -> (State, Label, SignNum, State) -> (CFA, State)
addTransition cfa@(CFA s0 sl sb g fss) (s1, l, k, s2) = case M.lookup s1 g of
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
        in  (CFA s0 sl sb g' fss, (snd . fromJust) mb_node)
    _ ->
        let ks = S.insert k S.empty
            g'  = M.insertWith (\ _ node -> M.insert l (ks, s2) node) s1 (M.insert l (ks, s2) M.empty) g
            g'' = M.insertWith (\ _ node -> node) s2 (M.empty) g'
        in  (CFA s0 (max (sl + 1) s2) sb g'' fss, s2)




toDot :: CFA -> FilePath -> IO ()
toDot (CFA _ _ _ g fss) fp = do
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




trace' :: (Show a) => a -> a
trace' a = trace (show a) a

trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a
