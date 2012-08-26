{-# LANGUAGE BangPatterns #-}

module CFA
    ( CFA(..)
    , Label
    , State
    , SignNum
    , SignSet
    , emptyCFA
    , lastState
    , addTransition
    , neighbourhood
    , initialState
    , setFinal
    , isFinal
    , acceptedSignatures
    , toDot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (partition, foldl')
import           Control.Monad            (forM_)
import           Data.Char                (ord)
import           Data.Maybe               (isJust, isNothing, fromJust)

import Debug.Trace


type State     = Int
type Label     = Char
type SignNum   = Int
type SignSet   = S.Set SignNum

type CFANode  = M.HashMap Label (SignSet, State)
type CFAGraph = M.HashMap State CFANode
data CFA      = CFA
    { initial_state :: State
    , last_state    :: State
    , cfa_graph     :: CFAGraph
    , final_states  :: M.HashMap State SignSet
    } deriving (Show)


emptyCFA :: CFA
emptyCFA = CFA 0 1 M.empty M.empty


initialState :: CFA -> State
initialState = initial_state


lastState :: CFA -> State
lastState = last_state


isFinal :: State -> CFA -> Bool
isFinal st cfa = isJust $ M.lookup st (final_states cfa)


setFinal :: CFA -> SignNum -> State -> CFA
setFinal (CFA s0 sl g fss) k s = CFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)


acceptedSignatures :: CFA -> State -> SignSet
acceptedSignatures (CFA _ _ _ fss) s = M.lookupDefault S.empty s fss


neighbourhood :: CFA -> State -> CFANode
neighbourhood (CFA _ _ g _) s = M.lookupDefault M.empty s g





trace' :: (Show a) => a -> a
trace' a = trace (show a) a


trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a





addTransition :: CFA -> (State, Label, SignNum, State) -> (CFA, State)
addTransition cfa@(CFA s0 sl g fss) (s1, l, k, s2) = case M.lookup s1 g of
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
        in  (CFA s0 sl g' fss, (snd . fromJust) mb_node)
    _ ->
        let ks = S.insert k S.empty
            g' = M.insertWith (\ _ node -> M.insert l (ks, s2) node) s1 (M.insert l (ks, s2) M.empty) g
        in  (CFA s0 (max (sl + 1) s2) g' fss, s2)


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
