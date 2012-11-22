module Types where

import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable
import           Data.Char                   (isAlphaNum, toLower, toUpper)
import           Text.Printf

---------------- regexp types
data Regexp a
    = Regexp (RegexpAlt a)
    deriving (Show)
data RegexpAlt a
    = AltFromCat   (RegexpCat a)
    | Alt          (RegexpCat a) (RegexpAlt a)
    deriving (Show)
data RegexpCat a
    = CatFromIter  (RegexpIter a)
    | Cat          (RegexpIter a) (RegexpCat a)
    deriving (Show)
data RegexpIter a
    = IterFromPrim (RegexpPrim a)
    | IterMaybe    (RegexpPrim a)
    | IterRepeat   (RegexpPrim a) Int
    | IterRange    (RegexpPrim a) Int Int
    deriving (Show)
data RegexpPrim a
    = Elementary [a]
    | Name       RegexpName
    | Wrapped    (RegexpAlt a)
    | Any
    | Range      [a]
    deriving (Show)
type RegexpTable a = M.HashMap String (Regexp a)


type State = Int
type RegexpId = Int

type DCFANode a      = M.HashMap (Label a) (S.Set RegexpId, State)
type DCFAGraph a     = M.HashMap State (DCFANode a)
data DCFA a          = DCFA
    { dcfa_init_state   :: State
    , dcfa_graph        :: DCFAGraph a
    , dcfa_final_states :: M.HashMap State (S.Set RegexpId)
    } deriving (Show)

type NCFANode a  = [(Label a, RegexpId, State)]
type NCFAGraph a = M.HashMap State (NCFANode a)
data NCFA a      = NCFA
    { ncfa_init_state   :: State
    , ncfa_max_state    :: State
    , ncfa_graph        :: NCFAGraph a
    , ncfa_final_states :: M.HashMap State (S.Set RegexpId)
    } deriving (Show)

data Label a
    = LOne a
    | LRange [a]
    deriving (Eq, Show)

instance (Eq a, Show a, Hashable a) => Hashable (Label a) where
    hash (LOne c)   = hash c
    hash (LRange r) = hash r



type TokenTable a = M.HashMap String a



class (Eq a, Ord a, Show a, Hashable a) => Labellable a where
    read' :: String -> TokenTable a -> (a, String)

    reads' :: TokenTable a -> String -> [a]
    reads' _    "" = []
    reads' ttbl s  =
        let (t, r) = read' s ttbl
        in  t : reads' ttbl r

    full_range :: [a]

    is_full_range :: [a] -> Bool
    is_full_range r = S.fromList r == S.fromList full_range

    span_range :: [a] -> [a]

    span_case :: a -> [a]



instance Labellable Char where
    read' (c : cs) _ = (c, cs)
    read' ""       _ = error "*** Types : read (Char) : trying to read from empty string"

    reads' _ cs = cs

    full_range = ['\x00' .. '\xFF']

    span_range s =
        let span_range' :: [Char] -> String -> [Char]
            span_range' cs ""                = cs
            span_range' cs (a : '-' : b : s) = span_range' ([a .. b] ++ cs) s
            span_range' cs (a : s)           = span_range' (a : cs) s
        in  span_range' "" s

    span_case c =
        let is_alpha :: Char -> Bool
            is_alpha c = (c > '\x40' && c <= '\x5A') || (c > '\x60' && c <= '\x7A')
        in  if is_alpha c then [toLower c, toUpper c] else [c]




instance Labellable Int where
    read' s ttbl =
        let (t, r) = span (\ c -> isAlphaNum c || c == '_') s
        in  case M.lookup t ttbl of
                Just i  -> (i, r)
                Nothing -> error $ printf "*** Types : read (Int) : unknown token : %s" t

    full_range = [0x00 .. 0xFFFFffff]

    span_range = id

    span_case i = [i]



{-
instance Eq (Label a) where
    LOne   x  == LOne   y  = x == y
    LRange xs == LRange ys = xs == ys
    LOne   x  == LRange xs = x `elem` xs
    LRange xs == LOne   x  = x `elem` xs

instance Show Label where
    show (LOne   x ) = show x
    show (LRange xs) = show xs
-}

---------------- Common types
data Chunk
    = Ch1 Code
    | Ch2 Code Options RuleTable
data Options
    = Opts
        { mode       :: Mode
        , token_type :: Type
        , match      :: Match
        }
    | OptsBlock
        { block      :: BlockName
        , token_type :: Type
        }
    deriving (Show)
data Mode
    = Single
    | Normal
    deriving (Show)
data Type
    = U8
    | U32
    deriving (Show)
data Match
    = Longest
    | All
    deriving (Show)
data CondList
    = ManyConds Cond CondList
    | OneCond   Cond
    deriving (Show)
type Cond
    = String
type RegexpName
    = String
type BlockName
    = String
type Code
    = BS.ByteString
type RuleTable
    = M.HashMap RegexpName (Maybe BlockName, M.HashMap (S.Set Cond) Code)
type SignTable
    = M.HashMap String [BS.ByteString]
type Conds2Code
    = M.HashMap (S.Set Cond) Code



type Node = [(Type, RegexpId, State)]
type MultiArc a = (Label a, S.Set RegexpId, S.Set State)

hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0
