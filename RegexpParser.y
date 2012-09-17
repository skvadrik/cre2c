{

module RegexpParser
    ( parse_regexps
    ) where

import qualified Data.HashMap.Strict as M
import           Data.Char
import           Control.Applicative       ((<$>))
import qualified Data.DList          as DL

import           Types

}

%name      parser
%tokentype { Token }
%error     { parseError }

%token
    name          { TokenName $$ }
    chain         { TokenChain $$ }
    int           { TokenInt $$ }
    '('           { TokenOBracket }
    ')'           { TokenCBracket }
    '['           { TokenOSqBracket }
    ']'           { TokenCSqBracket }
    '{'           { TokenOParenthesis }
    '}'           { TokenCParenthesis }
    ','           { TokenComma }
    '|'           { TokenVSlash }
    '\''          { TokenQuote }
    '"'           { TokenDQuote }
    '.'           { TokenDot }
    '?'           { TokenQueryMark }
    '='           { TokenEq }
    ';'           { TokenSemicolon }

%%

RegexpDefs
    : RegexpDef                      { Def  $1    }
    | RegexpDef RegexpDefs           { Defs $1 $2 }

RegexpDef
    : name '=' Regexp ';'            { RegexpDef $1 $3 }

Regexp
    : RegexpAlt                      { Regexp       $1 }

RegexpAlt
    : RegexpCat '|' RegexpAlt        { Alt          $1 $3 }
    | RegexpCat                      { AltFromCat   $1    }

RegexpCat
    : RegexpIter RegexpCat           { Cat          $1 $2 }
    | RegexpIter                     { CatFromIter  $1    }

RegexpIter
    : RegexpPrim '?'                 { IterMaybe    $1       }
    | RegexpPrim '{' int '}'         { IterRepeat   $1 $3    }
    | RegexpPrim '{' int ',' int '}' { IterRange    $1 $3 $5 }
    | RegexpPrim                     { IterFromPrim $1       }

RegexpPrim
    : name                           { Name         $1 }
    | '"' chain '"'                  { Elementary   $2 }
    | '\'' chain '\''                { Elementary   $2 }
    | '(' RegexpAlt ')'              { Wrapped      $2 }
    | '[' chain ']'                  { Range        $2 }
    | '.'                            { Any             }


{

data Token
    = TokenSemicolon
    | TokenEq
    | TokenChain String
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
    | TokenQuote
    | TokenDQuote
    | TokenDot
    | TokenQueryMark
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
      | isSpace c = lexer cs
      | isAlpha c = lex_name lexer (c : cs)
lexer ('='  : cs)        = TokenEq : lex_regexp cs
lexer ('-'  : '-'  : cs) = lex_comment cs


lex_name :: (String -> [Token]) -> String -> [Token]
lex_name f cs =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = f rest
    in  if nm /= "" then TokenName nm : rest' else rest'


lex_comment cs = lexer $ dropWhile (/= '\n') cs


lex_regexp :: String -> [Token]
lex_regexp [] = []
lex_regexp (c : cs)
      | isSpace c = lex_regexp cs
      | isAlpha c = lex_name lex_regexp (c : cs)
lex_regexp ('.'  : cs) = TokenDot          : lex_regexp cs
lex_regexp ('?'  : cs) = TokenQueryMark    : lex_regexp cs
lex_regexp ('('  : cs) = TokenOBracket     : lex_regexp cs
lex_regexp (')'  : cs) = TokenCBracket     : lex_regexp cs
lex_regexp ('{'  : cs) = TokenOParenthesis : lex_int cs
lex_regexp ('}'  : cs) = TokenCParenthesis : lex_regexp cs
lex_regexp ('"'  : cs) = TokenDQuote       : lex_dqchain cs
lex_regexp ('\'' : cs) = TokenQuote        : lex_qchain cs
lex_regexp ('['  : cs) = TokenOSqBracket   : lex_chain cs
lex_regexp ('|'  : cs) = TokenVSlash       : lex_regexp cs
lex_regexp (';'  : cs) = TokenSemicolon    : lexer cs


lex_int cs =
    let (num, rest) = span isDigit cs
    in  case rest of
            ',' : cs' -> TokenInt (read num) : TokenComma : lex_int cs'
            '}' : cs' -> TokenInt (read num) : TokenCParenthesis : lex_regexp cs'


take_escaped_till :: Char -> String -> (String, String)
take_escaped_till c s =
    let f :: DL.DList Char -> String -> (DL.DList Char, String)
        f tok ""                = (tok, "")
        f tok ('\\' : x : xs)   = f (DL.snoc (DL.snoc tok '\\') x) xs
        f tok (x : xs) | x == c = (DL.snoc tok '"', xs)
        f tok (x : xs)          = f (DL.snoc tok x) xs
        (tok, rest) = f (DL.fromList ['"']) s
    in  ((read . DL.toList) tok, rest)


lex_dqchain cs =
    let (ch, rest) = take_escaped_till '"' cs
    in  TokenChain ch : TokenDQuote : lex_regexp rest


lex_qchain cs =
    let (ch, rest) = take_escaped_till '\'' cs
    in  TokenChain ch : TokenQuote : lex_regexp rest


lex_chain cs =
    let (ch, rest) = take_escaped_till ']' cs
    in  TokenChain ch : TokenCSqBracket : lex_regexp rest


span_chain :: String -> String
span_chain s =
    let span_chain' :: String -> String -> String
        span_chain' s1 ""                 = s1
        span_chain' s1 (a : '-' : b : s2) = span_chain' ([a .. b] ++ s1) s2
        span_chain' s1 (a : s2)           = span_chain' (a : s1) s2
    in  span_chain' "" s


lex_range cs =
    let (ch, rest) = take_escaped_till ']' cs
    in  TokenChain (span_chain ch) : TokenCSqBracket : lex_regexp rest


--------------------------------------------------------------------------------
parse_regexps :: FilePath -> IO RegexpTable
parse_regexps fp =
    let regexps2table :: RegexpDefs -> [(String, Regexp)]
        regexps2table (Def  (RegexpDef name regexp))      = [(name, regexp)]
        regexps2table (Defs (RegexpDef name regexp) defs) = (name, regexp) : regexps2table defs
    in  M.fromList . regexps2table . parser . lexer <$> readFile fp

}










