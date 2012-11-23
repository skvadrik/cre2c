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
lexer s =
    let (ttbl, s') = (lex_token_table . dropWhile isSpace) s
    in  lex_rules ttbl s'


lex_rules :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_rules _    [] = []
lex_rules ttbl (c : cs)
    | isSpace c = lex_rules ttbl cs
    | isAlpha c =
        let (nm, rest) = lex_name (c : cs)
        in  TokenName nm : lex_rules ttbl rest
lex_rules ttbl ('='  : cs)        = TokenEq : lex_regexp ttbl cs
lex_rules ttbl ('-'  : '-'  : cs) = lex_comment ttbl cs


lex_name :: String -> (String, String)
lex_name cs =
    let v@(nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
    in  case nm of
            "" -> error "*** RegexpParser : lex_name : empty name"
            _  -> v


lex_comment :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_comment ttbl cs = lex_rules ttbl $ dropWhile (/= '\n') cs


lex_regexp :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_regexp _    [] = []
lex_regexp ttbl (c : cs)
    | isSpace c = lex_regexp ttbl cs
    | isAlpha c =
        let (nm, rest) = lex_name (c : cs)
        in  TokenName nm : lex_regexp ttbl rest
lex_regexp ttbl ('.'  : cs) = TokenDot          : lex_regexp  ttbl cs
lex_regexp ttbl ('?'  : cs) = TokenQueryMark    : lex_regexp  ttbl cs
lex_regexp ttbl ('('  : cs) = TokenOBracket     : lex_regexp  ttbl cs
lex_regexp ttbl (')'  : cs) = TokenCBracket     : lex_regexp  ttbl cs
lex_regexp ttbl ('{'  : cs) = TokenOParenthesis : lex_int     ttbl cs
lex_regexp ttbl ('}'  : cs) = TokenCParenthesis : lex_regexp  ttbl cs
lex_regexp ttbl ('"'  : cs) = TokenDQuote       : lex_dqchain ttbl cs
lex_regexp ttbl ('\'' : cs) =                     lex_qchain  ttbl cs
lex_regexp ttbl ('['  : cs) = TokenOSqBracket   : lex_range   ttbl cs
lex_regexp ttbl ('|'  : cs) = TokenVSlash       : lex_regexp  ttbl cs
lex_regexp ttbl (';'  : cs) = TokenSemicolon    : lex_rules       ttbl cs


lex_int :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_int ttbl cs =
    let (num, rest) = span isDigit cs
    in  case rest of
            ',' : cs' -> TokenInt (read num) : TokenComma        : lex_int    ttbl cs'
            '}' : cs' -> TokenInt (read num) : TokenCParenthesis : lex_regexp ttbl cs'


break_escaped :: Char -> String -> (String, String)
break_escaped c s =
    let f :: DL.DList Char -> String -> (DL.DList Char, String)
        f tok ""                        = (tok, "")
        f tok ('\\' : x : xs) | x == c  = f (DL.snoc (DL.snoc tok '\\') x) xs
        f tok (x : xs) | x == c         = (DL.snoc tok '"', xs)
        f tok (x : xs)                  = f (DL.snoc tok x) xs
        (tok, rest) = f (DL.fromList ['"']) s
    in  ((read . DL.toList) tok, rest)


lex_qchain' :: Labellable ta => [ta] -> [Token ta]
lex_qchain' []       = []
lex_qchain' (x : xs) = case span_case x of
    [y] -> TokenQuote      : TokenChain [y] : TokenQuote      : lex_qchain' xs
    ys  -> TokenOSqBracket : TokenChain ys  : TokenCSqBracket : lex_qchain' xs


lex_qchain :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_qchain ttbl cs =
    let (ch, rest) = break_escaped '\'' cs
        ch'        = reads' ttbl ch
    in lex_qchain' ch' ++ lex_regexp ttbl rest


lex_dqchain :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_dqchain ttbl cs =
    let (ch, rest) = break_escaped '"' cs
    in  TokenChain (reads' ttbl ch) : TokenDQuote : lex_regexp ttbl rest


lex_range :: Labellable ta => Maybe (TokenTable ta) -> String -> [Token ta]
lex_range ttbl cs =
    let (ch, rest) = break_escaped ']' cs
        ch'        = (span_range . reads' ttbl) ch
    in  TokenChain ch' : TokenCSqBracket : lex_regexp ttbl rest


--------------------------------------------------------------------------------
parse_regexps :: Labellable ta => FilePath -> IO (RegexpTable ta)
parse_regexps fp = M.fromList . parser . lexer <$> readFile fp

}










