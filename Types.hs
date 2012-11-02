module Types where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable
import           Data.Char                   (ord)
import           Numeric                     (showHex)


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


type State = Int
type Id    = Int

type DCFANode      = M.HashMap Label (S.Set Id, State)
type DCFAGraph     = M.HashMap State DCFANode
data DCFA          = DCFA
    { dcfa_init_state   :: State
    , dcfa_graph        :: DCFAGraph
    , dcfa_final_states :: M.HashMap State (S.Set Id)
    } deriving (Show)

type NCFANode  = [(Label, Id, State)]
type NCFAGraph = M.HashMap State NCFANode
data NCFA      = NCFA
    { ncfa_init_state   :: State
    , ncfa_max_state    :: State
    , ncfa_graph        :: NCFAGraph
    , ncfa_final_states :: M.HashMap State (S.Set Id)
    } deriving (Show)

data Label
    = LabelChar Char
    | LabelRange String

instance Eq Label where
    LabelChar c1  == LabelChar c2  = c1 == c2
    LabelChar c   == LabelRange s  = c `elem` s
    LabelRange s  == LabelChar c   = c `elem` s
    LabelRange s1 == LabelRange s2 = s1 == s2

instance Hashable Label where
    hash (LabelChar c)  = hash [c]
    hash (LabelRange r) = hash r

instance Show Label where
    show (LabelChar c)  = showHex (ord c) ""
    show (LabelRange s) = tail $ init $ show $ head s : '-' : [last s]

---------------- Common types
data ChunkList
    = LastChunk Code
    | Chunk Code RuleTable ChunkList
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
    = M.HashMap RegexpName (M.HashMap (S.Set Cond) Code)
type SignTable
    = M.HashMap String [BS.ByteString]


type Node = [(Char, Id, State)]
type MultiArc = (Label, S.Set Id, S.Set State)

hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0
