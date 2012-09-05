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
    , finalStates
    , acceptedSignatures

    , setFinal
    , addTransition

    , toDot
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.List                (foldl')
import           Control.Monad            (forM_)
import           Data.Maybe               (isJust, fromJust)


type State    = Int
type Label    = Char
type SignNum  = Int
type SignSet  = S.Set SignNum

type CFANode  = M.HashMap Label (SignSet, State)
type CFAGraph = M.HashMap State CFANode
data CFA      = CFA
    { initial_state    :: State
    , max_state_number :: State
    , cfa_graph        :: CFAGraph
    , final_states     :: M.HashMap State SignSet
    } deriving (Show)




emptyCFA :: CFA
emptyCFA = CFA 0 0 M.empty M.empty

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

finalStates :: CFA -> M.HashMap State SignSet
finalStates = final_states

acceptedSignatures :: State -> CFA -> SignSet
acceptedSignatures s (CFA _ _ _ fss) = M.lookupDefault (S.empty) s fss




setFinal :: State -> SignNum -> CFA -> CFA
setFinal s k (CFA s0 sl g fss) = CFA s0 sl g (M.insertWith (\ _ ks -> S.insert k ks) s (S.insert k S.empty) fss)

addTransition :: CFA -> (State, Label, SignNum, State) -> (CFA, State)
addTransition cfa@(CFA s0 sl g fss) (s1, l, k, s2) = case M.lookup s1 g of
    Just node | let mb_node = M.lookup l node, isJust mb_node ->
        let g' = M.adjust (\ node -> M.adjust (\ (ks, s') -> (S.insert k ks, s')) l node) s1 g
        in  (CFA s0 sl g' fss, (snd . fromJust) mb_node)
    _ ->
        let ks = S.insert k S.empty
            g'  = M.insertWith (\ _ node -> M.insert l (ks, s2) node) s1 (M.insert l (ks, s2) M.empty) g
            g'' = M.insertWith (\ _ node -> node) s2 (M.empty) g'
        in  (CFA s0 (max (sl + 1) s2) g'' fss, s2)




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
