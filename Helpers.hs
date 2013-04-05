module Helpers
    ( trace'
    , trace''
    , trace'''

    , ($$$)
    , wrap_in_braces

    , is_alpha
    , is_alpha_num_
    , skip_spaces
    , lex_name
    , lex_int
    , break_escaped
    ) where


import           Data.Char                       (isAlphaNum, isSpace, isDigit)
import           Debug.Trace
import           Text.PrettyPrint.HughesPJ       (($$), ($+$), Doc)
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Data.DList                as DL


-------------------------------------------------------------------

trace' :: Show a => a -> a
trace' a = trace (show a) a


trace'' :: Show a => String -> a -> a
trace'' s a = trace (s ++ show a) a


trace''' :: Show a => String -> a -> a
trace''' s a = trace (show a ++ s) a

-------------------------------------------------------------------

instance Eq Doc where
    d1 == d2 = PP.render d1 == PP.render d2


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                  = d1 $$ PP.text "" $$ d2
infixl 5 $$$


wrap_in_braces :: Doc -> Doc
wrap_in_braces d = PP.text "{" $+$ PP.nest 4 d $$ PP.text "}"

-------------------------------------------------------------------

is_alpha :: Char -> Bool
is_alpha c = (c > '\x40' && c <= '\x5A') || (c > '\x60' && c <= '\x7A')


is_alpha_num_ :: Char -> Bool
is_alpha_num_ c = isAlphaNum c || c == '_'


skip_spaces :: String -> String
skip_spaces = dropWhile isSpace


lex_name :: String -> (String, String)
lex_name s =
    let v@(nm, _) = (span is_alpha_num_ . skip_spaces) s
    in  case nm of
            "" -> err "lex_name : empty name"
            _  -> v


lex_int :: String -> (Int, String)
lex_int s =
    let (n, s') = (span isDigit . skip_spaces) s
    in  (read n, s')


break_escaped :: Char -> String -> (String, String)
break_escaped c s =
    let f :: DL.DList Char -> String -> (DL.DList Char, String)
        f _   ""                        = error "break_escaped: not enough symbols"
        f tok ('\\' : '"'  : xs) = f (DL.snoc (DL.snoc tok '\\') '"') xs
        f tok ('\\' : '\'' : xs) = f (DL.snoc (DL.snoc tok '\\') '\'') xs
        f tok ('\\' : ']'  : xs) = f (DL.snoc tok ']') xs
--        f tok ('\\' : x : xs) | x == c  = f (DL.snoc tok x) xs
--        f tok ('\\' : x : xs) | x == c  = f (DL.snoc (DL.snoc tok '\\') x) xs
        f tok (x : xs) | x == c         = (DL.snoc tok '"', xs)
        f tok (x : xs)                  = f (DL.snoc tok x) xs
        (tok, rest) = f (DL.fromList ['"']) s
    in  ((read . DL.toList) tok, rest)

{-
-- inside of an X-wrapped string you should escape X's and standard escapes only
break_escaped :: Char -> String -> (String, String)
break_escaped c s =
    let f :: DL.DList Char -> String -> (DL.DList Char, String)
        f _   ""                        = err "break_escaped: not enough symbols"
--        f tok ('\\' : x : xs) | x == c  = f (DL.snoc tok x) xs
        f tok ('\\' : x : xs) | x == c  = f (DL.snoc (DL.snoc tok '\\') x) xs
        f tok (x : xs) | x == c         = (DL.snoc tok '"', xs)
        f tok (x : xs)                  = f (DL.snoc tok x) xs
        (tok, rest) = f (DL.fromList ['"']) s
    in  ((read . DL.toList) tok, rest)
-}
-------------------------------------------------------------------

err :: String -> a
err s = error $ "*** Helpers : " ++ s


