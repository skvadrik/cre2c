{

module RegexpParser where

import           Data.Char

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
    '{'           { TokenOParenthesis }
    '}'           { TokenCParenthesis }
    ','           { TokenComma }
    '|'           { TokenVSlash }
    '"'           { TokenDQuote }

%%

Regexp     : RegexpAlt                  { Regexp       $1 }

RegexpAlt  : RegexpCat '|' RegexpAlt    { Alt          $1 $3 }
           | RegexpCat                  { AltFromCat   $1    }

RegexpCat  : RegexpIter RegexpCat       { Cat          $1 $2 }
           | RegexpIter                 { CatFromIter  $1    }

RegexpIter : RegexpPrim '{' ',' int '}' { Iter         $1 $4 }
           | RegexpPrim                 { IterFromPrim $1    }

RegexpPrim : name                       { Name         $1 }
           | '"' chain '"'              { Elementary   $2 }
           | '(' RegexpAlt ')'          { Wrapped      $2 }

{

parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


data Regexp     = Regexp RegexpAlt
                deriving (Show, Eq, Ord)

data RegexpAlt  = AltFromCat RegexpCat
                | Alt        RegexpCat RegexpAlt
                deriving (Show, Eq, Ord)

data RegexpCat  = CatFromIter    RegexpIter
                | Cat RegexpIter RegexpCat
                deriving (Show, Eq, Ord)

data RegexpIter = IterFromPrim RegexpPrim
                | Iter         RegexpPrim Int
                deriving (Show, Eq, Ord)

data RegexpPrim = Elementary String
                | Name String
                | Wrapped RegexpAlt
                deriving (Show, Eq, Ord)

data Token = TokenChain String
           | TokenName String
           | TokenInt Int
           | TokenOBracket
           | TokenCBracket
           | TokenOParenthesis
           | TokenCParenthesis
           | TokenComma
           | TokenVSlash
           | TokenDQuote
           deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexName (c:cs)
lexer ('(':cs) = TokenOBracket : lexer cs
lexer (')':cs) = TokenCBracket : lexer cs
lexer ('{':cs) = TokenOParenthesis : lexer cs
lexer ('}':cs) = TokenCParenthesis : lexer cs
lexer ('"':cs) = TokenDQuote : lexChain cs
lexer (',':cs) = TokenComma : lexInt cs
lexer ('|':cs) = TokenVSlash : lexer cs

lexInt cs = TokenInt (read num) : lexer rest
    where (num, rest) = span isDigit cs

lexName cs = TokenName nm : lexer rest
    where (nm, rest) = span isAlphaNum cs

lexChain cs = TokenChain ch : TokenDQuote : lexer (tail rest)
    where (ch, rest) = span (/= '"') cs

parseRegexp = parse_tokenized_regexp . lexer

}

