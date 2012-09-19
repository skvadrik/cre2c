module Types where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable

import Debug.Trace
trace' a = trace (show a) a
trace'' s a = trace (s ++ show a) a


---------------- regexp types
data Regexp
    = Regexp RegexpAlt
    deriving (Show, Eq, Ord)
data RegexpAlt
    = AltFromCat   RegexpCat
    | Alt          RegexpCat RegexpAlt
    deriving (Show, Eq, Ord)
data RegexpCat
    = CatFromIter    RegexpIter
    | Cat RegexpIter RegexpCat
    deriving (Show, Eq, Ord)
data RegexpIter
    = IterFromPrim RegexpPrim
    | IterMaybe    RegexpPrim
    | IterRepeat   RegexpPrim Int
    | IterRange    RegexpPrim Int Int
    deriving (Show, Eq, Ord)
data RegexpPrim
    = Elementary String
    | Name String
    | Wrapped RegexpAlt
    | Any
    | Range String
    deriving (Show, Eq, Ord)
data RegexpDefs
    = Def  RegexpDef
    | Defs RegexpDef RegexpDefs
    deriving (Show)
data RegexpDef
    = RegexpDef String Regexp
    deriving (Show)
type RegexpTable = M.HashMap String Regexp


type State     = Int
type SignNum   = Int
type SignSet   = S.Set SignNum

type DCFANode      = M.HashMap DCFALabel State
type DCFAInitNode  = M.HashMap DCFALabel (SignSet, State)
type DCFAGraph     = M.HashMap State DCFANode
data DCFA          = DCFA
    { dcfa_init_state   :: State
    , dcfa_init_node    :: DCFAInitNode
    , dcfa_graph        :: DCFAGraph
    , dcfa_final_states :: M.HashMap State SignSet
    } deriving (Show)

type NCFANode  = [(NCFALabel, SignNum, State)]
type NCFAGraph = M.HashMap State NCFANode
data NCFA      = NCFA
    { ncfa_init_state   :: State
    , ncfa_max_state    :: State
    , ncfa_graph        :: NCFAGraph
    , ncfa_final_states :: M.HashMap State SignSet
    } deriving (Show)

type DCFALabel = Char
data NCFALabel
    = LabelChar Char
    | LabelRange String

instance Eq NCFALabel where
    LabelChar c1  == LabelChar c2  = c1 == c2
    LabelChar c   == LabelRange s  = False -- c `elem` s
    LabelRange s  == LabelChar c   = False -- c `elem` s
    LabelRange s1 == LabelRange s2 = s1 == s2

instance Show NCFALabel where
    show (LabelChar c)  = tail $ init $ show c
    show (LabelRange s) = tail $ init $ show $ head s : '-' : [last s]

---------------- Common types
data Rules
    = ManyRules Rule Rules
    | OneRule   Rule
    deriving (Show)
data Rule
    = SimpleRule  RegexpName Code
    | ComplexRule CondList RegexpName Code
    deriving (Show)
data CondList
    = ManyConds Cond CondList
    | OneCond   Cond
    deriving (Show)
type Cond
    = String
type RegexpName
    = String
type Code
    = BS.ByteString
type RuleTable
    = M.HashMap Int ([Cond], RegexpName, Code)
type SignTable
    = M.HashMap String [BS.ByteString]
