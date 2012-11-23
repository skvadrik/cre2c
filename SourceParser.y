{

module SourceParser
    ( parse_source
    ) where

import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList            as DL
import           Control.Applicative         ((<$>))
import           Data.Char
import           Data.List                   (foldl')
import qualified Data.Set              as S
import           Text.Printf

import           Types

}

%name      parser
%tokentype { Token }
%error     { parseError }

%token
    name          { TokenName $$ }
    code          { TokenCode $$ }
    '>'           { TokenAngle }
    ':'           { TokenColon }
    '{'           { TokenOParenthesis }
    '}'           { TokenCParenthesis }
    '='           { TokenEq }
    start         { TokenStart }
    end           { TokenEnd }
    mode          { TokenMode $$ }
    token_type    { TokenType $$ }
    match         { TokenMatch $$ }
    block         { TokenBlock $$ }

%%

Source :: { [Chunk] }
    : code                                           { [Ch1 $1] }
    | code start Options Rules end Source            { (Ch2 $1 $3 (foldl' insert_rule M.empty $4)) : $6 }
    | code start Options end Source                  { error "*** SourceParser : empty rule list; useless scanner block." }

Options
    : mode                                           { Opts      $1     U8  Longest }
    | token_type                                     { Opts      Normal $1  Longest }
    | match                                          { Opts      Normal U8  $1      }
    | mode token_type                                { Opts      $1     $2  Longest }
    | mode match                                     { Opts      $1     U8  $2      }
    | token_type match                               { Opts      Normal $1  $2      }
    | mode token_type match                          { Opts      $1     $2  $3      }
    | mode match token_type                          { Opts      $1     $3  $2      }
    | token_type mode match                          { Opts      $2     $1  $3      }
    | token_type match mode                          { Opts      $3     $1  $2      }
    | match token_type mode                          { Opts      $3     $2  $1      }
    | match mode token_type                          { Opts      $2     $3  $1      }
    | block                                          { OptsBlock $1     U8          }
    | block token_type                               { OptsBlock $1     $2          }

Rules :: { [Rule] }
    : Rules1                                         { $1 }
    | Rules2                                         { $1 }

Rules1 :: { [Rule] }
    : Rule1                                          { [$1]    }
    | Rule1 Rules1                                   { $1 : $2 }

Rule1 :: { Rule }
    : name '=' '{' code '}'                          { ($1, Nothing, [], $4) }
    | Conds '>' name '=' '{' code '}'                { ($3, Nothing, $1, $6) }

Rules2 :: { [Rule] }
    : Rule2                                          { [$1]    }
    | Rule2 Rules2                                   { $1 : $2 }

Rule2 :: { Rule }
    : name ':' name '=' '{' code '}'                 { ($1, Just $3, [], $6) }
    | Conds '>' name ':' name '=' '{' code '}'       { ($3, Just $5, $1, $8) }

Conds :: { [Cond] }
    : name                                           { [$1]    }
    | name Conds                                     { $1 : $2 }


{

type Rule
    = (RegexpName, Maybe BlockName, [Cond], Code)

data Source
    = SourceEnd  Code
    | Source     Code Options [Rule] Source

data Token
    = TokenEq
    | TokenCode BS.ByteString
    | TokenName String
    | TokenOParenthesis
    | TokenCParenthesis
    | TokenAngle
    | TokenColon
    | TokenStart
    | TokenEnd
    | TokenMode  Mode
    | TokenType  Type
    | TokenMatch Match
    | TokenBlock BlockName
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer ::  String -> [Token]
lexer [] = []
lexer ('/':'*':'s':'t':'a':'r':'t':':': cs) =
    let default_opts = Opts Normal U8 Longest
    in  TokenStart : lex_options cs default_opts
lexer cs =
    let lex_entry_code :: DL.DList Char -> String -> (DL.DList Char, String)
        lex_entry_code code "" = (code, "")
        lex_entry_code code ('\n':'/':'*':'s':'t':'a':'r':'t':':':cs) = (code, cs)
        lex_entry_code code (c : cs) = lex_entry_code (DL.snoc code c) cs
        (code, rest) = lex_entry_code DL.empty cs
        default_opts = Opts Normal U8 Longest
    in  TokenCode ((BS.pack . DL.toList) code) : (if rest == "" then [] else TokenStart : lex_options rest default_opts)


lex_options :: String -> Options -> [Token]
lex_options ('!':'c':'r':'e':'2':'c':'_':'t':'y':'p':'e':':':cs)     opts = lex_type cs  opts
lex_options ('!':'c':'r':'e':'2':'c':'_':'m':'o':'d':'e':':':cs)     opts = lex_mode cs  opts
lex_options ('!':'c':'r':'e':'2':'c':'_':'m':'a':'t':'c':'h':':':cs) opts = lex_match cs opts
lex_options (c:cs)  opts | isSpace c = lex_options cs opts
lex_options s@(c:_) opts | isAlpha c = case opts of
    OptsBlock _ _ -> lex_rules_b s
    _             -> lex_rules   s
lex_options s o = error $ "+++++++++++++++++" ++ show s ++ "----------------------" ++ show o


lex_type :: String -> Options -> [Token]
lex_type cs opts =
    let (ttype, cs') = (span isAlphaNum . dropWhile isSpace) cs
    in  case ttype of
            t | t == "U8B"  -> TokenType U8  : lex_options cs' (opts{ token_type = U8 })
            t | t == "U32B" -> TokenType U32 : lex_options cs' (opts{ token_type = U32 })
            _               -> error $ printf "*** SourceParser: unknown token_type: %s" ttype


lex_mode :: String -> Options -> [Token]
lex_mode cs opts =
    let (mode, cs') = (span isAlpha . dropWhile isSpace) cs
    in  case mode of
            m | m == "single" -> TokenMode Single : lex_options cs' (opts{ mode = Single })
            m | m == "normal" -> TokenMode Normal : lex_options cs' (opts{ mode = Normal })
            m | m == "block"  ->
                let (block, cs'') = lex_blockname cs'
                in  TokenBlock block : lex_options cs'' (OptsBlock block (token_type opts))
            _                 -> error $ printf "*** SourceParser: unknown mode: %s" mode


lex_match :: String -> Options -> [Token]
lex_match cs opts =
    let (match, cs') = (span isAlphaNum . dropWhile isSpace) cs
    in  case match of
            m | m == "longest"  -> TokenMatch Longest : lex_options cs' (opts{ match = Longest })
            m | m == "all"      -> TokenMatch All     : lex_options cs' (opts{ match = All })
            _                   -> error $ printf "*** SourceParser: unknown match: %s" match


lex_blockname :: String -> (BlockName, String)
lex_blockname ('(':cs) =
    let (block, rest) = span (\ c -> isAlpha c || c == '_') cs
    in  case (block, rest) of
            ("", _)                            -> error "*** SourceParser: block name not specified."
            (_,  r) | r == "" || head r /= ')' -> error $ printf "*** SourceParser: missing ')' after \"cre2c: block(%s\"" block
            _                                  -> (block, tail rest)
lex_block cs = error "*** SourceParser: missing '(' after \"block\" in \"cre2c_mode:\" directive"


lex_rules :: String -> [Token]
lex_rules [] = []
lex_rules ('\n' : 'e' : 'n' : 'd' : '*' : '/' : cs) = TokenEnd : lexer cs
lex_rules (c : cs)
    | isSpace c = lex_rules cs
    | isAlpha c = lex_name (c : cs) lex_rules
lex_rules ('>'  : cs) = TokenAngle        : lex_name cs lex_rules
lex_rules ('='  : cs) = TokenEq           : lex_rules cs
lex_rules ('{'  : cs) = TokenOParenthesis : lex_code cs lex_rules
lex_rules cs          = error $ printf "*** SourceParser : lex_rules : can't parse rules at %s" $ (show . take 50) cs


lex_rules_b :: String -> [Token]
lex_rules_b [] = []
lex_rules_b ('\n' : 'e' : 'n' : 'd' : '*' : '/' : cs) = TokenEnd : lexer cs
lex_rules_b (c : cs)
    | isSpace c = lex_rules_b cs
    | isAlpha c = lex_name (c : cs) lex_rules_b
lex_rules_b (':'  : cs) = TokenColon : lex_name cs lex_rules_b
lex_rules_b ('>'  : cs) = TokenAngle        : lex_name cs lex_rules_b
lex_rules_b ('='  : cs) = TokenEq           : lex_rules_b cs
lex_rules_b ('{'  : cs) = TokenOParenthesis : lex_code cs lex_rules_b
lex_rules_b cs          = error $ printf "*** SourceParser : lex_rules_b : can't parse rules at %s" $ (show . take 50) cs


lex_code :: String -> (String -> [Token]) -> [Token]
lex_code cs lex =
    let lex_code' :: Int -> DL.DList Char -> String -> (DL.DList Char, String)
        lex_code' 0 tok rest         = (DL.cons '{' tok, rest)
        lex_code' i tok ""           = error "*** SourceParser: invalid code block in regexp definition"
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (DL.snoc tok '{') rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (DL.snoc tok '}') rest
        lex_code' i tok (c   : rest) = lex_code' i       (DL.snoc tok c)   rest
        (code, rest) = lex_code' 1 DL.empty cs
    in  TokenCode ((BS.pack . DL.toList) code) : TokenCParenthesis : lex rest


lex_name :: String -> (String -> [Token]) -> [Token]
lex_name cs lex =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = lex rest
    in  if nm /= "" then TokenName nm : rest' else rest'


--------------------------------------------------------------------------------


combine_rules :: (Maybe BlockName, Conds2Code) -> (Maybe BlockName, Conds2Code) -> (Maybe BlockName, Conds2Code)
combine_rules (Just b, _) (Just b', _) | b /= b' = error "*** SourceParser: rules for one regexp lead to different blocks"
combine_rules (Just b, _) (Nothing, _)           = error "*** SourceParser: combine_rules: dark magic..."
combine_rules (Nothing, _) (Just b, _)           = error "*** SourceParser: combine_rules: dark magic..."
combine_rules (b, conds2code) (b', conds2code')  =
    let conds2code'' = M.foldlWithKey'
            (\ c2c conds code -> M.insertWith
                (\ _ code' -> BS.concat [BS.pack "{ ", code, code', BS.pack " }"])
                conds
                code
                conds2code
            ) conds2code' conds2code
    in  (b, conds2code'')


insert_rule :: RuleTable -> Rule -> RuleTable
insert_rule rules (name, block, conds, code) = M.insertWith
    combine_rules
    name
    (block, M.insert (S.fromList conds) code M.empty)
    rules


parse_source :: FilePath -> IO [Chunk]
parse_source fp = (parser . lexer) <$> readFile fp

}










