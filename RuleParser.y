{

module RuleParser
    ( parse_rules
    ) where

import qualified Data.HashMap.Strict as M
import           Data.Char
import           Control.Applicative      ((<$>))

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

%%

Rules
    : Rule                                     { OneRule   $1    }
    | Rule Rules                               { ManyRules $1 $2 }

Rule
    : RegexpName '=' '{' Code '}'              { SimpleRule  $1 $4    }
    | CondList '>' RegexpName '=' '{' Code '}' { ComplexRule $1 $3 $6 }

CondList
    : Cond                                     { OneCond   $1    }
    | Cond CondList                            { ManyConds $1 $2 }


{

data Rules
    = ManyRules Rule Rules
    | OneRule   Rule
    deriving (Show)

data Rule
    = SimpleRule  RegexpName Code
    | ComplexRule CondList RegexpName Code
    deriving (Show)

data CondList
    = ManyConds Cond CondList
    | OneCond   Cond
    deriving (Show)

data Token
    = TokenEq
    | TokenCode String
    | TokenName String
    | TokenOParenthesis
    | TokenCParenthesis
    | TokenAngle
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
      | isSpace c = lexer cs
      | isAlpha c = lex_name lexer (c : cs)
lexer ('='  : cs)        = TokenEq : lex_regexp cs
lexer ('\n' : cs)        = lexer cs
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


lex_dqchain cs = TokenChain ch : TokenDQuote : lex_regexp (tail rest)
    where (ch, rest) = span (/= '"') cs


lex_qchain cs = TokenChain ch : TokenQuote : lex_regexp (tail rest)
    where (ch, rest) = span (/= '\'') cs


lex_chain cs = TokenChain ch : TokenCSqBracket : lex_regexp (tail rest)
    where (ch, rest) = span (/= ']') cs


--------------------------------------------------------------------------------
parse_rules :: String -> RuleTable
parse_rules =
    let regexps2table :: RegexpDefs -> [(String, Regexp)]
        regexps2table (Def  (RegexpDef name regexp))      = [(name, regexp)]
        regexps2table (Defs (RegexpDef name regexp) defs) = (name, regexp) : regexps2table defs
    in  M.fromList . regexps2table . parser . lexer

}










