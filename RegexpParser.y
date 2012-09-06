{

module RegexpParser where

import Data.Char
import Types

}

%name parse_tokenized_regexp
%tokentype { Token }
%error { parseError }

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
    '"'           { TokenDQuote }
    '.'           { TokenDot }
    '?'           { TokenQueryMark }

%%

Regexp     : RegexpAlt                      { Regexp       $1 }

RegexpAlt  : RegexpCat '|' RegexpAlt        { Alt          $1 $3 }
           | RegexpCat                      { AltFromCat   $1    }

RegexpCat  : RegexpIter RegexpCat           { Cat          $1 $2 }
           | RegexpIter                     { CatFromIter  $1    }

RegexpIter : RegexpPrim '?'                 { IterMaybe    $1       }
           | RegexpPrim '{' int '}'         { IterRepeat   $1 $3    }
           | RegexpPrim '{' int ',' int '}' { IterRange    $1 $3 $5 }
           | RegexpPrim                     { IterFromPrim $1       }

RegexpPrim : name                           { Name         $1 }
           | '"' chain '"'                  { Elementary   $2 }
           | '(' RegexpAlt ')'              { Wrapped      $2 }
           | '[' chain ']'                  { Range        $2 }
           | '.'                            { Any             }

{

parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexName (c:cs)
lexer ('.':cs) = TokenDot : lexer cs
lexer ('?':cs) = TokenQueryMark : lexer cs
lexer ('(':cs) = TokenOBracket : lexer cs
lexer (')':cs) = TokenCBracket : lexer cs
lexer ('{':cs) = TokenOParenthesis : lexInt cs
lexer ('}':cs) = TokenCParenthesis : lexer cs
lexer ('"':cs) = TokenDQuote : lexQuotedChain cs
lexer ('[':cs) = TokenOSqBracket : lexChain cs
lexer ('|':cs) = TokenVSlash : lexer cs

lexInt cs =
    let (num, rest) = span isDigit cs
    in  case rest of
            ',' : cs' -> TokenInt (read num) : TokenComma : lexInt cs'
            '}' : cs' -> TokenInt (read num) : TokenCParenthesis : lexer cs'

lexName cs = TokenName nm : lexer rest
    where (nm, rest) = span isAlphaNum cs

lexQuotedChain cs = TokenChain ch : TokenDQuote : lexer (tail rest)
    where (ch, rest) = span (/= '"') cs

lexChain cs = TokenChain ch : TokenCSqBracket : lexer (tail rest)
    where (ch, rest) = span (/= ']') cs

parseRegexp = parse_tokenized_regexp . lexer

}

