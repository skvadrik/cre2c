{
{-# OPTIONS -cpp #-}

module RegexpParser
    ( parse_regexps
    ) where

import qualified Data.HashMap.Strict as M
import           Data.Char
import           Control.Applicative       ((<$>))
import qualified Data.DList          as DL
import           Data.List                 (foldl')

import           Types

#define HappyAbsSyn (HappyAbsSyn_ ta)

}

%name      parser
%tokentype { Token ta }
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

RDefs :: { [(String, Regexp ta)] }
    : RDef                            { [$1]    }
    | RDef RDefs                      { $1 : $2 }

RDef :: { (String, Regexp ta) }
    : name '=' R ';'                  { ($1, $3) }

R :: { Regexp ta }
    : RAlt                            { Regexp       $1 }

RAlt :: { RegexpAlt ta }
    : RCat '|' RAlt                   { Alt          $1 $3 }
    | RCat                            { AltFromCat   $1    }

RCat :: { RegexpCat ta }
    : RIter RCat                      { Cat          $1 $2 }
    | RIter                           { CatFromIter  $1    }

RIter :: { RegexpIter ta }
    : RPrim '?'                       { IterMaybe    $1       }
    | RPrim '{' int '}'               { IterRepeat   $1 $3    }
    | RPrim '{' int ',' int '}'       { IterRange    $1 $3 $5 }
    | RPrim                           { IterFromPrim $1       }

RPrim :: { RegexpPrim ta }
    : name                            { Name         $1 }
    | '"' chain '"'                   { Elementary   $2 }
    | '\'' chain '\''                 { Elementary   $2 }
    | '(' RAlt ')'                    { Wrapped      $2 }
    | '[' chain ']'                   { Range        $2 }
    | '.'                             { Any             }


{

data Token ta
    = TokenSemicolon
    | TokenEq
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
    | TokenChain       [ta]
    | TokenName        String
    | TokenInt         Int
    deriving (Show)


parseError :: [Token ta] -> tb
parseError e = error "Parse error"


lexer :: Labellable ta => String -> [Token ta]
lexer [] = []
lexer (c : cs)
      | isSpace c = lexer cs
      | isAlpha c = lex_name lexer (c : cs)
lexer ('='  : cs)        = TokenEq : lex_regexp cs
lexer ('-'  : '-'  : cs) = lex_comment cs


lex_name :: Labellable ta => (String -> [Token ta]) -> String -> [Token ta]
lex_name f cs =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = f rest
    in  if nm /= "" then TokenName nm : rest' else rest'


lex_comment :: Labellable ta => String -> [Token ta]
lex_comment cs = lexer $ dropWhile (/= '\n') cs


lex_regexp :: Labellable ta => String -> [Token ta]
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
lex_regexp ('\'' : cs) = lex_qchain cs
lex_regexp ('['  : cs) = TokenOSqBracket   : lex_range cs
lex_regexp ('|'  : cs) = TokenVSlash       : lex_regexp cs
lex_regexp (';'  : cs) = TokenSemicolon    : lexer cs


lex_int :: Labellable ta => String -> [Token ta]
lex_int cs =
    let (num, rest) = span isDigit cs
    in  case rest of
            ',' : cs' -> TokenInt (read num) : TokenComma : lex_int cs'
            '}' : cs' -> TokenInt (read num) : TokenCParenthesis : lex_regexp cs'


break_escaped :: Char -> String -> (String, String)
break_escaped c s =
    let f :: DL.DList Char -> String -> (DL.DList Char, String)
        f tok ""                        = (tok, "")
        f tok ('\\' : x : xs) | x == c  = f (DL.snoc (DL.snoc tok '\\') x) xs
        f tok (x : xs) | x == c         = (DL.snoc tok '"', xs)
        f tok (x : xs)                  = f (DL.snoc tok x) xs
        (tok, rest) = f (DL.fromList ['"']) s
    in  ((read . DL.toList) tok, rest)


lex_qchain :: Labellable ta => String -> [Token ta]
lex_qchain cs =
    let (ch, rest) = break_escaped '\'' cs
        ch' = reads' M.empty ch
        f :: Labellable ta => [ta] -> [Token ta]
        f []       = lex_regexp rest
        f (x : xs) = case span_case x of
            [y] -> TokenQuote : TokenChain [y] : TokenQuote : f xs
            ys  -> TokenOSqBracket : TokenChain ys : TokenCSqBracket : f xs
    in f ch'


lex_dqchain :: Labellable ta => String -> [Token ta]
lex_dqchain cs =
    let (ch, rest) = break_escaped '"' cs
    in  TokenChain (reads' M.empty ch) : TokenDQuote : lex_regexp rest


lex_range :: Labellable ta => String -> [Token ta]
lex_range cs =
    let (ch, rest) = break_escaped ']' cs
        ch'        = (span_range . reads' M.empty) cs
    in  TokenChain ch' : TokenCSqBracket : lex_regexp rest


--------------------------------------------------------------------------------
parse_regexps :: Labellable ta => FilePath -> IO (RegexpTable ta)
parse_regexps fp = M.fromList . parser . lexer <$> readFile fp

}










