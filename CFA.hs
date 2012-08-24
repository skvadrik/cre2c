{-# LANGUAGE BangPatterns #-}

module CFA
    ( CFA(..)
    , Label
    , Label
    , State
    , SignNum
    , SignSet
    , StateMap
    , emptyCFA
    , addTransition
    , neighbourhood
    , initialStateCFA
    , setFinal
    , isFinal
    , acceptedSignaturesCFA
    , toDotCFA
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (partition, foldl')
import           Control.Monad            (forM_)
import           Data.Char                (ord)
import           Data.Maybe               (isJust, isNothing, fromJust)

import Debug.Trace


type State     = Int
type SignNum   = Int
type SignSet   = S.Set SignNum
type StateMap  = M.HashMap State (S.Set State)

type Label    = Char
type CFANode  = M.HashMap Label (SignSet, State)
type CFAGraph = M.HashMap State CFANode
data CFA      = CFA
    { initial_state :: State
    , last_states   :: S.Set State
    , cfa_graph     :: DCFAGraph
    , final_states  :: M.HashMap State SignSet
    } deriving (Show)


emptyCFA :: CFA
emptyCFA = CFA 0 1 M.empty M.empty


initialStateCFA :: CFA -> State
initialStateCFA = initial_state


isFinalCFA :: State -> CFA -> Bool
isFinalCFA st cfa = isJust $ M.lookup st (final_states cfa)


setFinal :: CFA -> SignNum -> State -> CFA
setFinal (CFA st0 stl dg f_sts) st sign_num = CFA
    st0
    stl
    dg
    ( M.insertWith
        (\ _ sign_nums -> S.insert sign_num sign_nums)
        st
        (S.insert sign_num S.empty)
        f_sts
    )


acceptedSignaturesCFA :: CFA -> State -> SignSet
acceptedSignaturesCFA cfa st = case M.lookup st (final_states cfa) of
    Just signs -> signs
    Nothing    -> S.empty


neighbourhood :: CFA -> State -> CFANode
neighbourhood (CFA _ _ g _) s = M.lookupDefault M.empty s g


trace' :: (Show a) => a -> a
trace' a = trace (show a) a


trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a


addTransition :: CFA -> (State, NLabel, SignNum, State) -> (CFA, State)
addTransition cfa@(CFA s0 l_ss g f_ss) (s1, l, sign, s2) = case M.lookup s1 g of
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (signs, s') -> (S.insert sign signs, s')) l node) s1 g
        in  (CFA s0 sl g' f_ss, fromJust mb_node)
    _ ->
        let signs = S.insert sign S.empty
            g' = M.insertWith (\ _ node -> M.insert l (signs, s2) node) s1 (M.insert l (signs, s2) M.empty) g
        in  (CFA s0 (max (sl + 1) s2) g' f_ss, s2)


toDot :: CFA -> FilePath -> IO ()
toDot (CFA _ g f_ss) fp = do
    writeFile  fp "digraph CFA {\n"
    appendFile fp "\trankdir = LR\n\tnode [shape=\"circle\"]\n"
    forM_ (M.keys f_ss) $
        \ i -> appendFile fp $ concat
            [ "\t"
            , show i
            , " [shape=\"doublecircle\" label=\""
            , show i
            , show (M.lookupDefault undefined i f_ss)
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
