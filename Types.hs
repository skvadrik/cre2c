module Types where

import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.Hashable
import           Data.Char                   (toLower, toUpper)
import           Text.Printf
import Control.DeepSeq

import           Helpers


type SBlkname  = String
type SRegname  = String
type STokname  = String
type SCond     = String
type SCode     = String


type IRegID    = Int
type IStateID  = Int
type ITokID    = Int
type IBlkID    = Int


type MRegID2RegInfo       = M.HashMap IRegID        (Maybe SBlkname, MCondset2Code)
type MRegname2RegInfo     = M.HashMap SRegname      (Maybe SBlkname, MCondset2Code)
type MCondset2Code        = M.HashMap (S.Set SCond) SCode
type MRegname2Regexp a    = M.HashMap SRegname      (Regexp a)
type MTokname2TokID       = M.HashMap STokname      ITokID


type DCFANode a    = M.HashMap (Label a) (S.Set IRegID, Bool, IStateID)
type DCFAGraph a   = M.HashMap IStateID (DCFANode a)
type NCFANode a    = [(Label a, IRegID, Bool, IStateID)]
type NCFAGraph a   = M.HashMap IStateID (NCFANode a)


data StateInfo a
    = SI IStateID Bool Bool (DCFANode a) (Maybe (S.Set IRegID))
data BlockInfo a
    = BI Int Options MRegID2RegInfo (Maybe MTokname2TokID)


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
    | IterZeroMany (RegexpPrim a)
    | IterOneMany  (RegexpPrim a)
    | IterMaybe    (RegexpPrim a)
    | IterRepeat   (RegexpPrim a) Int
    | IterRange    (RegexpPrim a) Int Int
    deriving (Show)
data RegexpPrim a
    = Elementary [a]
    | Name       SRegname
    | Wrapped    (RegexpAlt a)
    | Any
    | Range      [a]
    deriving (Show)


data DCFA a = DCFA
    { dcfa_init_state   :: IStateID
    , dcfa_graph        :: DCFAGraph a
    , dcfa_final_states :: M.HashMap IStateID (S.Set IRegID)
    } deriving (Show)
data NCFA a = NCFA
    { ncfa_init_state   :: IStateID
    , ncfa_max_state    :: IStateID
    , ncfa_graph        :: NCFAGraph a
    , ncfa_final_states :: M.HashMap IStateID (S.Set IRegID)
    } deriving (Show)


data Chunk
    = Ch1 SCode
    | Ch2 SCode Options MRegname2RegInfo
data Options
    = Opts
        { mode           :: Mode
        , match          :: Match
        , token_type     :: TokenType
        , prelexer       :: Maybe String
        , default_action :: Maybe SCode
        }
    | OptsBlock
        { block          :: SBlkname
        , token_type     :: TokenType
        , prelexer       :: Maybe String
        , default_action :: Maybe SCode
        }
    deriving (Show)
data Mode
    = Single
    | Normal
    deriving (Show)
data TokenType
    = TTChar
    | TTEnum STokname
    deriving (Show)
data Match
    = Longest
    | All
    deriving (Show)


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
    LOne   _       `compare` LRange []      = err "empty range"
    LRange []      `compare` LOne   _       = err "empty range"
instance Labellable a => Show (Label a) where
    show (LOne   x ) = show_hex  x
    show (LRange xs) = shows_hex xs
instance Labellable a => NFData (Label a) where
    rnf (LOne x) = rnf x
    rnf (LRange xs) = rnf xs


class (Eq a, Ord a, PrintfArg a, NFData a, Show a, Hashable a) => Labellable a where
    read' :: String -> Maybe MTokname2TokID -> (a, String)

    reads' :: Maybe MTokname2TokID -> String -> [a]
    reads' _    "" = []
    reads' ttbl s  =
        let (t, r) = read' s ttbl
        in  t : reads' ttbl r

    full_range :: Maybe MTokname2TokID -> [a]

    is_full_range :: [a] -> Maybe MTokname2TokID -> Bool
    is_full_range r ttbl = S.fromList r == S.fromList (full_range ttbl)

    span_range :: [a] -> [a]

    span_case :: a -> [a]

    show_hex :: a -> String
    show_hex a = printf "0x%02X" a

    shows_hex :: [a] -> String
    shows_hex [] = "EMPTY_RANGE"
    shows_hex r = printf "%s-%s" ((show_hex . head) r) ((show_hex . last) r)


instance Labellable Char where
    read' _        (Just _) = err "read' (Char) : non-empty token table for char-based scanner ?"
    read' (c : cs) _        = (c, cs)
    read' ""       _        = err "read' (Char) : trying to read from empty string"

    reads' (Just _) _ = err "reads' (Char) : non-empty token table for char-based scanner ?"
    reads' _ cs       = cs

    full_range Nothing  = ['\x00' .. '\xFF']
    full_range (Just _) = err "full_range (Char) : non-empty token table"

    span_range s =
        let span_range' :: [Char] -> String -> [Char]
            span_range' cs ""                = cs
            span_range' cs (a : '-' : b : s) = span_range' ([a .. b] ++ cs) s
            span_range' cs (a : s)           = span_range' (a : cs) s
        in  span_range' "" s

    span_case c = if is_alpha c then [toLower c, toUpper c] else [c]


instance Labellable Int where
    read' _ Nothing     = err "read' (Int) : empty token table for int-based scanner ?"
    read' s (Just ttbl) =
        let (t, r) = lex_name s
        in  case M.lookup t ttbl of
                Just i  -> (i, r)
                Nothing -> err $ printf "read (Int) : unknown token : %s" (show t)

    full_range (Just ttbl) = M.elems ttbl
    full_range Nothing     = err "full_range (Int) : empty token table"

    span_range = id

    span_case i = [i]


hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance (Hashable a) => Hashable (S.Set a) where
    hash = S.foldl' hashAndCombine 0


err :: String -> a
err s = error $ "*** Types : " ++ s