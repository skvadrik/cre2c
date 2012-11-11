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

import           Types

}

%name      parser
%tokentype { Token }
%error     { parseError }

%token
    name          { TokenName $$ }
    code          { TokenCode $$ }
    '>'           { TokenAngle }
    '{'           { TokenOParenthesis }
    '}'           { TokenCParenthesis }
    '='           { TokenEq }
    start         { TokenStart }
    end           { TokenEnd }
    mode          { TokenMode $$ }
    match         { TokenMatch $$ }

%%

Source
    : code start Options Rules end Source { Source    $1 $3 $4 $6 }
    | code                                { SourceEnd $1          }

Options
    : mode match                          { Options $1 $2 }

Rules
    : Rule                                { OneRule   $1    }
    | Rule Rules                          { ManyRules $1 $2 }

Rule
    : name '=' '{' code '}'               { SimpleRule  $1 $4    }
    | CondList '>' name '=' '{' code '}'  { ComplexRule $1 $3 $6 }

CondList
    : name                                { OneCond   $1    }
    | name CondList                       { ManyConds $1 $2 }


{

data Source
    = Source     Code Options Rules Source
    | SourceEnd  Code

data Token
    = TokenEq
    | TokenCode BS.ByteString
    | TokenName String
    | TokenOParenthesis
    | TokenCParenthesis
    | TokenAngle
    | TokenStart
    | TokenEnd
    | TokenMode Mode
    | TokenMatch Match
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
    in  TokenMode mode : TokenMatch match : lex_rules cs''


lex_mode :: String -> (Mode, String)
lex_mode ('!':'c':'r':'e':'2':'c':'_':'m':'o':'d':'e':':':cs) =
    let (mode, rest) = (span (\ c -> isAlpha c || c == '-') . dropWhile isSpace) cs
    in  case mode of
            m | m == "scanner"   -> (Scanner, rest)
            m | m == "matcher"   -> (Matcher, rest)
            m | m == "tokenizer" -> (Tokenizer, rest)
            _                    -> error $ "unknown mode: " ++ mode
lex_mode _ = error " *** missing \"!cre2c_mode:\" directive"


lex_match :: String -> (Match, String)
lex_match ('!':'c':'r':'e':'2':'c':'_':'m':'a':'t':'c':'h':':':cs) =
    let (match, rest) = (span (\ c -> isAlpha c) . dropWhile isSpace) cs
    in  case match of
            m | m == "longest"  -> (Longest, rest)
            m | m == "shortest" -> (Shortest, rest)
            m | m == "all"      -> (All, rest)
            _                   -> error $ "unknown match: " ++ match
lex_match _ = error " *** missing \"!cre2c_match:\" directive"


lex_rules :: String -> [Token]
lex_rules [] = []
lex_rules ('\n' : 'e' : 'n' : 'd' : '*' : '/' : cs) = TokenEnd : lexer cs
lex_rules (c : cs)
    | isSpace c = lex_rules cs
    | isAlpha c = lex_name (c : cs)
lex_rules ('>'  : cs) = TokenAngle        : lex_name cs
lex_rules ('='  : cs) = TokenEq           : lex_rules cs
lex_rules ('{'  : cs) = TokenOParenthesis : lex_code cs


lex_code :: String -> [Token]
lex_code cs =
    let lex_code' :: Int -> DL.DList Char -> String -> (DL.DList Char, String)
        lex_code' 0 tok rest         = (DL.cons '{' tok, rest)
        lex_code' i tok ""           = error "invalid code block in regexp definition"
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (DL.snoc tok '{') rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (DL.snoc tok '}') rest
        lex_code' i tok (c   : rest) = lex_code' i       (DL.snoc tok c)   rest
        (code, rest) = lex_code' 1 DL.empty cs
    in  TokenCode ((BS.pack . DL.toList) code) : TokenCParenthesis : lex_rules rest


lex_name :: String -> [Token]
lex_name cs =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = lex_rules rest
    in  if nm /= "" then TokenName nm : rest' else rest'


--------------------------------------------------------------------------------
parse_source :: FilePath -> IO ChunkList
parse_source fp =
    let conds2list :: CondList -> [Cond]
        conds2list (OneCond c)      = [c]
        conds2list (ManyConds c cs) = c : conds2list cs

        rules2table :: Rules -> [(RegexpName, ([Cond], Code))]
        rules2table (OneRule   (SimpleRule           name code))       = [(name, ([], code))]
        rules2table (OneRule   (ComplexRule condlist name code))       = [(name, (conds2list condlist, code))]
        rules2table (ManyRules (SimpleRule           name code) rules) = (name, ([], code)) : rules2table rules
        rules2table (ManyRules (ComplexRule condlist name code) rules) = (name, (conds2list condlist, code)) : rules2table rules

        source2chunk_list :: Source -> ChunkList
        source2chunk_list (SourceEnd code)                = LastChunk code
        source2chunk_list (Source code opts rules source) = Chunk
            code
            opts
            ((foldl'
                (\ rules (name, (conds, code)) -> M.insertWith
                    (\ _ xs -> M.insertWith (\ _ code' -> BS.concat [BS.pack "{ ", code, code', BS.pack " }"]) (S.fromList conds) code xs)
                    name
                    (M.insert (S.fromList conds) code M.empty)
                    rules
                ) M.empty
            ) (rules2table rules))
            (source2chunk_list source)
    in  ( source2chunk_list
        . parser
        . lexer
        ) <$> readFile fp

}










