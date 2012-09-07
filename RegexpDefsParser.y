{

module RegexpDefsParser (parseRegexpDefs) where

import Data.Char
import Types

}

%name      parse_regexp_defs
%tokentype { Token }
%error     { parseError }

%token
    name   { TokenName $$ }
    regexp { TokenRegexp $$ }
    '='    { TokenEq }
    ';'    { TokenSemicolon }

%%

RegexpDefs
    : RegexpDef            { Def  $1    }
    | RegexpDef RegexpDefs { Defs $1 $2 }

RegexpDef
    : name '=' regexp ';'  { RegexpDef $1 $3 }


{
data Token
    = TokenRegexp    String
    | TokenName      String
    | TokenSemicolon
    | TokenEq
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer [] = []
lexer (c : cs)
      | isSpace c = lexer cs
      | isAlpha c = lexName (c : cs)
lexer ('='  : cs) = TokenEq : lexRegexp cs
lexer ('\n' : cs) = lexer cs
lexer ('-' : '-'  : cs) = lexComment cs

lexName cs =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = lexer rest
    in  if nm /= "" then TokenName nm : rest' else rest'

lexRegexp cs =
    let (r, rest) = span (/= ';') cs
    in  TokenRegexp r : if rest /= "" then TokenSemicolon : lexer (tail rest) else []

lexComment cs = lexer $ dropWhile (/= '\n') cs


parseRegexpDefs = parse_regexp_defs . lexer
}

