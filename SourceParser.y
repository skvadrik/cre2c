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
    match         { TokenMatch $$ }
    block         { TokenBlock $$ }

%%

Source
    : code                                           { SourceEnd $1          }
    | code start Options Rules end Source            { Source    $1 $3 $4 $6 }
    | code start Options end Source                  { Empty     $1 $3 $5    }

Options
    : mode match                                     { Opts      $1 $2 }
    | block                                          { OptsBlock $1    }

Rules
    : Rules1                                         { R1 $1 }
    | Rules2                                         { R2 $1 }

Rules1
    : Rule1                                          { ROne1  $1    }
    | Rule1 Rules1                                   { RMany1 $1 $2 }

Rule1
    : name '=' '{' code '}'                          { RSimple1  $1 $4    }
    | CondList '>' name '=' '{' code '}'             { RComplex1 $1 $3 $6 }

Rules2
    : Rule2                                          { ROne2  $1    }
    | Rule2 Rules2                                   { RMany2 $1 $2 }

Rule2
    : name ':' name '=' '{' code '}'                 { RSimple2  $1 $3 $6    }
    | CondList '>' name ':' name '=' '{' code '}'    { RComplex2 $1 $3 $5 $8 }

CondList
    : name                                           { OneCond   $1    }
    | name CondList                                  { ManyConds $1 $2 }


{

data Source
    = SourceEnd  Code
    | Source     Code Options Rules Source
    | Empty      Code Options       Source

data Rules
    = R1 Rules1
    | R2 Rules2

data Rules1
    = RMany1 Rule1 Rules1
    | ROne1  Rule1

data Rule1
    = RSimple1  RegexpName Code
    | RComplex1 CondList RegexpName Code

data Rules2
    = RMany2 Rule2 Rules2
    | ROne2  Rule2

data Rule2
    = RSimple2  RegexpName BlockName Code
    | RComplex2 CondList RegexpName BlockName Code

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
    | TokenMode Mode
    | TokenMatch Match
    | TokenBlock BlockName
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer ::  String -> [Token]
lexer [] = []
lexer ('/':'*':'s':'t':'a':'r':'t':':': cs) = TokenStart : lex_options cs
lexer cs =
    let lex_entry_code :: DL.DList Char -> String -> (DL.DList Char, String)
        lex_entry_code code "" = (code, "")
        lex_entry_code code ('\n':'/':'*':'s':'t':'a':'r':'t':':':cs) = (code, cs)
        lex_entry_code code (c : cs) = lex_entry_code (DL.snoc code c) cs
        (code, rest) = lex_entry_code DL.empty cs
    in  TokenCode ((BS.pack . DL.toList) code) : (if rest == "" then [] else TokenStart : lex_options rest)


lex_options :: String -> [Token]
lex_options cs =
    let (mode, cs')   = (lex_mode . dropWhile isSpace) cs
        (match, cs'') = (lex_match . dropWhile isSpace) cs'
    in  case mode of
            Left m  -> TokenMode  m : TokenMatch match : lex_rules cs''
            Right b -> TokenBlock b : lex_rules_b cs'


lex_mode :: String -> (Either Mode BlockName, String)
lex_mode ('!':'c':'r':'e':'2':'c':'_':'m':'o':'d':'e':':':cs) =
    let (mode, rest) = (span (\ c -> isAlpha c || c == '_') . dropWhile isSpace) cs
    in  case mode of
            m | m == "single" -> (Left Single, rest)
            m | m == "normal" -> (Left Normal, rest)
            m | m == "block"  ->
                let (block, rest') = lex_blockname rest
                in  (Right block, rest')
            _                    -> error $ printf "*** SourceParser: unknown mode: %s" mode
lex_mode _ = error "*** SourceParser: missing \"!cre2c_mode:\" directive"


lex_match :: String -> (Match, String)
lex_match ('!':'c':'r':'e':'2':'c':'_':'m':'a':'t':'c':'h':':':cs) =
    let (match, rest) = (span (\ c -> isAlpha c || c == '_') . dropWhile isSpace) cs
    in  case match of
            m | m == "longest"  -> (Longest, rest)
            m | m == "all"      -> (All, rest)
            _                   -> error $ printf "*** SourceParser: unknown match: %s" match
lex_match _ = error "*** SourceParser: missing \"!cre2c_match:\" directive"


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


conds2list :: CondList -> [Cond]
conds2list (OneCond c)      = [c]
conds2list (ManyConds c cs) = c : conds2list cs


-- 2 types (Rules1 and Rules2) are needed to make ALL rules in one block be either "block" rules or simple rules

rules2table1 :: Rules1 -> [(RegexpName, Maybe BlockName, [Cond], Code)]
rules2table1 (ROne1  (RSimple1           name code))       = [(name, Nothing, [], code)]
rules2table1 (ROne1  (RComplex1 condlist name code))       = [(name, Nothing, conds2list condlist, code)]
rules2table1 (RMany1 (RSimple1           name code) rules) = (name, Nothing, [], code) : rules2table1 rules
rules2table1 (RMany1 (RComplex1 condlist name code) rules) = (name, Nothing, conds2list condlist, code) : rules2table1 rules


rules2table2 :: Rules2 -> [(RegexpName, Maybe BlockName, [Cond], Code)]
rules2table2 (ROne2  (RSimple2           name block code))       = [(name, Just block, [], code)]
rules2table2 (ROne2  (RComplex2 condlist name block code))       = [(name, Just block, conds2list condlist, code)]
rules2table2 (RMany2 (RSimple2           name block code) rules) = (name, Just block, [], code) : rules2table2 rules
rules2table2 (RMany2 (RComplex2 condlist name block code) rules) = (name, Just block, conds2list condlist, code) : rules2table2 rules


combine_rules :: (Maybe BlockName, M.HashMap (S.Set Cond) Code) -> (Maybe BlockName, M.HashMap (S.Set Cond) Code) -> (Maybe BlockName, M.HashMap (S.Set Cond) Code)
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


insert_rule :: RuleTable -> (RegexpName, Maybe BlockName, [Cond], Code) -> RuleTable
insert_rule rules (name, block, conds, code) = M.insertWith
    combine_rules
    name
    (block, M.insert (S.fromList conds) code M.empty)
    rules


source2chunk_list :: Source -> ChunkList
source2chunk_list (SourceEnd code)                = LastChunk code
source2chunk_list (Empty _ _ _)                   = error "*** SourceParser : empty rule list; useless scanner block."
source2chunk_list (Source code opts rules source) =
    let rules'  = case rules of
            R1 rs -> rules2table1 rs
            R2 rs -> rules2table2 rs
        rules'' = foldl' insert_rule M.empty rules'
        chunks  = source2chunk_list source
    in  Chunk code opts rules'' chunks


parse_source :: FilePath -> IO ChunkList
parse_source fp =
    ( source2chunk_list
    . parser
    . lexer
    ) <$> readFile fp

}










