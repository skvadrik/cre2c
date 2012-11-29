{

module SourceParser
    ( parse_source
    ) where

import qualified Data.HashMap.Strict   as M
import           Data.Char
import           Data.List                   (foldl', isPrefixOf)
import qualified Data.DList            as DL
import qualified Data.Set              as S
import           Text.Printf

import           Types                 hiding (err)
import           Helpers

}

%name      parser
%tokentype { Token }
%error     { parseError }

%token
    name          { TName    $$ }
    code          { TCode    $$ }
    options       { TOptions $$ }
    ':'           { TColon      }
    '>'           { TAngle      }
    '='           { TEq         }
    start         { TStart      }
    end           { TEnd        }

%%

Source :: { [Chunk] }
    : code                                           { [Ch1 $1] }
    | code start options Rules end Source            { (Ch2 $1 $3 (foldl' insert_rule M.empty $4)) : $6 }
    | code start options       end Source            { err "empty rule list; useless scanner block." }

Rules :: { [Rule] }
    : Rules1                                         { $1 }
    | Rules2                                         { $1 }

Rules1 :: { [Rule] }
    : Rule1                                          { [$1]    }
    | Rule1 Rules1                                   { $1 : $2 }

Rule1 :: { Rule }
    : name '=' code                                  { ($1, Nothing, [], $3) }
    | Conds '>' name '=' code                        { ($3, Nothing, $1, $5) }

Rules2 :: { [Rule] }
    : Rule2                                          { [$1]    }
    | Rule2 Rules2                                   { $1 : $2 }

Rule2 :: { Rule }
    : name ':' name '=' code                         { ($1, Just $3, [], $5) }
    | Conds '>' name ':' name '=' code               { ($3, Just $5, $1, $7) }

Conds :: { [SCond] }
    : name                                           { [$1]    }
    | name Conds                                     { $1 : $2 }


{

type Rule
    = (SRegname, Maybe SBlkname, [SCond], SCode)

data Token
    = TEq
    | TAngle
    | TColon
    | TStart
    | TEnd
    | TCode    SCode
    | TName    String
    | TOptions Options
    deriving (Show)

data OptionSet = OptionSet
    { opt_mode        :: Mode
    , opt_match       :: Match
    , opt_block       :: Maybe SBlkname
    , opt_token_type  :: TokenType
    , opt_prelexer    :: Maybe String
    , opt_default     :: Maybe SCode
    }


parseError :: [Token] -> a
parseError e = err $ "Parse error: " ++ show e


lexer :: String -> [Token]
lexer s =
    let (entry, s') = lex_entry s
    in  TCode entry : case s' of
            "" -> []
            _  -> TStart : lex_options s'


lex_entry :: SCode -> (SCode, String)
lex_entry s =
    let s1 = "/*start:"
        lex_entry' entry rest = case rest of
            ""                    -> (DL.toList entry, "")
            r | s1 `isPrefixOf` r -> (DL.toList entry, drop (length s1) r)
            c : cs                -> lex_entry' (DL.snoc entry c) cs
    in  lex_entry' DL.empty s


lex_options :: String -> [Token]
lex_options s =
    let s1 = "!cre2c_mode:"
        s2 = "!cre2c_match:"
        s3 = "!cre2c_type:"
        s4 = "!cre2c_prelexer:"
        s5 = "!cre2c_default:"
        lex_options' (opts, rest) = case skip_spaces rest of
            r | s1 `isPrefixOf` r -> (lex_options' . lex_mode       opts . drop (length s1)) r
            r | s2 `isPrefixOf` r -> (lex_options' . lex_match      opts . drop (length s2)) r
            r | s3 `isPrefixOf` r -> (lex_options' . lex_token_type opts . drop (length s3)) r
            r | s4 `isPrefixOf` r -> (lex_options' . lex_prelexer   opts . drop (length s4)) r
            r | s5 `isPrefixOf` r -> (lex_options' . lex_default    opts . drop (length s5)) r
            _                     -> (opts, rest)
        def_opts    = OptionSet Normal Longest Nothing TTChar Nothing Nothing
        (opts', s') = lex_options' (def_opts, s)
    in  case opts' of
            OptionSet _    Longest (Just block) ttype@(TTEnum _) pl df -> TOptions (OptsBlock block ttype  pl df) : lex_rules_b s'
            OptionSet _    Longest (Just block) TTChar           pl df -> TOptions (OptsBlock block TTChar pl df) : lex_rules_b s'
            OptionSet mode match   Nothing      ttype@(TTEnum _) pl df -> TOptions (Opts mode match ttype  pl df) : lex_rules   s'
            OptionSet mode match   Nothing      TTChar           pl df -> TOptions (Opts mode match TTChar pl df) : lex_rules   s'
            _                                                             -> err "conflicting options"


lex_mode :: OptionSet -> String -> (OptionSet, String)
lex_mode opts s =
    let (m, s') = lex_name s
    in  case m of
            "single" -> (opts{opt_mode = Single}, s')
            "normal" -> (opts{opt_mode = Normal}, s')
            "block"  ->
                let (b, s'') = lex_blockname s'
                in  (opts{opt_block = Just b}, s'')
            _        -> err $ printf "unknown mode: %s" m


lex_match :: OptionSet -> String -> (OptionSet, String)
lex_match opts s =
    let (m, s') = lex_name s
    in  case m of
            "longest"  -> (opts{opt_match = Longest}, s')
            "all"      -> (opts{opt_match = All},     s')
            _          -> err $ printf "unknown match: %s" m


lex_token_type :: OptionSet -> String -> (OptionSet, String)
lex_token_type opts s =
    let (t, s') = lex_name s
    in  case t of
            "char" -> (opts{opt_token_type = TTChar},   s')
            _      -> (opts{opt_token_type = TTEnum t}, s')


lex_prelexer :: OptionSet -> String -> (OptionSet, String)
lex_prelexer opts s =
    let (pl, s') = lex_name s
    in  (opts{opt_prelexer = Just pl}, s')


lex_default :: OptionSet -> String -> (OptionSet, String)
lex_default opts s =
    let (df, s') = (lex_code . skip_spaces) s
    in  (opts{opt_default = Just df}, s')


lex_blockname :: String -> (SBlkname, String)
lex_blockname ('(':s) =
    case span is_alpha_num_ s of
        ("", _     ) -> err "block name not specified."
        (b,  ')':s') -> (b, s')
        (b,  _     ) -> err $ printf "missing ')' after \"cre2c: block(%s\"" b
lex_block s           = err "missing '(' after \"block\" in \"cre2c_mode:\" directive"


lex_rules :: String -> [Token]
lex_rules s =
    let s1 = "end*/"
        s2 = skip_spaces s
        s3 = drop (length s1) s2
    in  case s2 of
            s'       | s1 `isPrefixOf` s' -> TEnd : lexer s3
            s'@(c:_) | isAlpha c          -> let (nm, s'') = lex_name s' in TName nm : lex_rules s''
            '>' : s'                      -> let (nm, s'') = lex_name s' in TAngle : TName nm : lex_rules s''
            '=' : s'                      -> TEq : lex_rules s'
            '{' : s'                      -> let (cd, s'') = lex_code s2 in TCode cd : lex_rules s''
            s'                            -> err $ printf "lex_rules : can't parse rules at %s" $ (take 50 . show) s'


lex_rules_b :: String -> [Token]
lex_rules_b s =
    let s1 = "end*/"
        s2 = skip_spaces s
        s3 = drop (length s1) s2
    in  case s2 of
            s'       | s1 `isPrefixOf` s' -> TEnd : lexer s3
            s'@(c:_) | isAlpha c          -> let (nm, s'') = lex_name s' in TName nm : lex_rules_b s''
            ':' : s'                      -> let (nm, s'') = lex_name s' in TColon : TName nm : lex_rules_b s''
            '>' : s'                      -> let (nm, s'') = lex_name s' in TAngle : TName nm : lex_rules_b s''
            '=' : s'                      -> TEq : lex_rules_b s'
            '{' : s'                      -> let (cd, s'') = lex_code s2 in TCode cd : lex_rules_b s''
            s'                            -> err $ printf "lex_rules : can't parse rules at %s" $ (take 50 . show) s'


lex_code :: String -> (SCode, String)
lex_code ('{' : s) =
    let lex_code' 0 tok rest         = ((DL.toList . DL.cons '{') tok, rest)
        lex_code' i tok ""           = err "lex_code: invalid code block"
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (DL.snoc tok '{') rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (DL.snoc tok '}') rest
        lex_code' i tok (c   : rest) = lex_code' i       (DL.snoc tok c)   rest
    in  lex_code' 1 DL.empty s
lex_code _         = err "lex_code : missing '{' before code block"


err :: String -> a
err s = error $ "*** SourceParser : " ++ s


--------------------------------------------------------------------------------


combine_rules :: (Maybe SBlkname, MCondset2Code) -> (Maybe SBlkname, MCondset2Code) -> (Maybe SBlkname, MCondset2Code)
combine_rules (Just b, _) (Just b', _) | b /= b' = err "rules for one regexp lead to different blocks"
combine_rules (Just b, _) (Nothing, _)           = err "combine_rules: dark magic..."
combine_rules (Nothing, _) (Just b, _)           = err "combine_rules: dark magic..."
combine_rules (b, conds2code) (b', conds2code')  =
    let conds2code'' = M.foldlWithKey'
            (\ c2c conds code -> M.insertWith
                (\ _ code' -> concat ["{ ", code, code', " }"])
                conds
                code
                conds2code
            ) conds2code' conds2code
    in  (b, conds2code'')


insert_rule :: MRegname2RegInfo -> Rule -> MRegname2RegInfo
insert_rule rules (name, block, conds, code) = M.insertWith
    combine_rules
    name
    (block, M.insert (S.fromList conds) code M.empty)
    rules


parse_source :: String -> [Chunk]
parse_source = parser . lexer

}










