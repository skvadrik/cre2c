module Types where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable


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
data Token
    = TokenChain String
    | TokenName String
    | TokenInt Int
    | TokenOBracket
    | TokenCBracket
    | TokenOSqBracket
    | TokenCSqBracket
    | TokenOParenthesis
    | TokenCParenthesis
    | TokenComma
    | TokenVSlash
    | TokenDQuote
    | TokenDot
    | TokenQueryMark
    deriving (Show)
type RegexpTable = M.HashMap String Regexp


---------------- CFA types
type State    = Int
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


data Label
    = LabelChar Char
    | LabelRange String
    | LabelAny
    deriving (Show)

instance Eq Label where
    LabelAny      == _             = True
    _             == LabelAny      = True
    LabelChar c1  == LabelChar c2  = c1 == c2
    LabelChar c   == LabelRange s  = c `elem` s
    LabelRange s  == LabelChar c   = c `elem` s
    LabelRange s1 == LabelRange s2 = s1 == s2

instance Hashable Label where
    hash LabelAny       = hash ""
    hash (LabelChar c)  = hash [c]
    hash (LabelRange s) = hash s


---------------- Common types
type Code        = BS.ByteString
type Condition   = String
type Rule        = ([Condition], Regexp, Code)
type SignTable   = M.HashMap String [BS.ByteString]
