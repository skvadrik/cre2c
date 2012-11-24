module Types where

import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable
import           Data.Char                   (isAlphaNum, toLower, toUpper, isSpace)
import           Data.List                   (isPrefixOf)
import           Text.Printf
import           Debug.Trace


trace' :: Show a => a -> a
trace' a = trace (show a) a

trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a

trace''' :: Show a => String -> a -> a
trace''' s a = trace (show a ++ s) a

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
type TokenTable a  = M.HashMap String a




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

instance Labellable a => Hashable (Label a) where
    hash (LOne c)   = hash c
    hash (LRange r) = hash r

instance Labellable a => Eq (Label a) where
    LOne   x  == LOne   y  = x == y
    LRange xs == LRange ys = xs == ys
    LOne   x  == LRange xs = x `elem` xs
    LRange xs == LOne   x  = x `elem` xs

instance Labellable a => Ord (Label a) where
    LOne   x       `compare` LOne   y       = x  `compare` y
    LRange xs      `compare` LRange ys      = xs `compare` ys
    LOne   x       `compare` LRange (y : _) = x  `compare` y
    LRange (y : _) `compare` LOne   x       = y  `compare` x
    LOne   _       `compare` LRange []      = error "*** Types : empty range"
    LRange []      `compare` LOne   _       = error "*** Types : empty range"

instance Labellable a => Show (Label a) where
    show (LOne   x ) = show_hex  x
    show (LRange xs) = shows_hex xs



class (Eq a, Ord a, PrintfArg a, Show a, Hashable a) => Labellable a where
    read' :: String -> Maybe (TokenTable a) -> (a, String)

    reads' :: Maybe (TokenTable a) -> String -> [a]
    reads' _    "" = []
    reads' ttbl s  =
        let (t, r) = read' s ttbl
        in  t : reads' ttbl r

    full_range :: [a]

    is_full_range :: [a] -> Bool
    is_full_range r = S.fromList r == S.fromList full_range

    span_range :: [a] -> [a]

    span_case :: a -> [a]

    lex_token_table :: Labellable a => String -> (Maybe (TokenTable a), String)

    show_hex :: a -> String
    show_hex a = printf "0x%02X" a

    shows_hex :: [a] -> String
    shows_hex [] = "EMPTY_RANGE"
    shows_hex r = printf "%s-%s" ((show_hex . head) r) ((show_hex . last) r)




instance Labellable Char where
    read' _        (Just _) = error "*** Types : read' (Char) : non-empty token table for char-based scanner ?"
    read' (c : cs) _        = (c, cs)
    read' ""       _        = error "*** Types : read' (Char) : trying to read from empty string"

    reads' (Just _) _ = error "*** Types : reads' (Char) : non-empty token table for char-based scanner ?"
    reads' _ cs       = cs

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

    lex_token_table s = (Nothing, s)




instance Labellable Int where
    read' _ Nothing     = error "*** Types : read' (Int) : empty token table for int-based scanner ?"
    read' s (Just ttbl) =
        let (t, r) = span (\ c -> isAlphaNum c || c == '_') s
        in  case M.lookup t ttbl of
                Just i  -> (i, r)
                Nothing -> error $ printf "*** Types : read (Int) : unknown token : %s" t

    full_range = [0x00 .. 0xFFFFffff]

    span_range = id

    span_case i = [i]

    lex_token_table s =
        let s1       = "!cre2c_token_table:"
            s2       = ( dropWhile isSpace . drop (length s1) ) s
            (s3, s4) = case break (== '}') s2 of
                ('{':s5@(_:_), '}':s6) -> (s5, s6)
                _                      -> error "*** RegexpParser : lex_token_table : bad token table"
            check_token_name s =
                let s' = takeWhile (\ c -> isAlphaNum c || c == '_') s
                in  if s' == s then s else error "*** RegexpParser : check_token_name : mailformed token"
            tokens   = (map check_token_name . words) s3
            ttbl     = M.fromList $ zip tokens [1 .. length tokens]
        in  if s1 `isPrefixOf` s
                then (Just ttbl, s4)
                else (Nothing, s)




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
