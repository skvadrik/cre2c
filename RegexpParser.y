{
{-# OPTIONS -cpp #-}

module RegexpParser
    ( parse_def_file
    ) where

import qualified Data.HashMap.Strict as M hiding (lookupDefault)
import qualified Data.HashMap.Lazy   as M        (lookupDefault)
import           Data.Char
import           Control.Applicative             ((<$>))
import           Data.List                       (foldl', isPrefixOf)
import qualified Data.Set            as S
import           Text.Printf

import           Types                    hiding (err)
import           Helpers

#define HappyAbsSyn    (HappyAbsSyn_ ta)

}

%name      parser
%tokentype { Token ta }
%error     { parseError }
%token
    name          { TokenName $$ }
    chain         { TokenChain $$ }
    range         { TokenRange $$ }
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
    '*'           { TokenStar }
    '+'           { TokenPlus }

%%

RDefs :: { [(SRegname, Regexp ta)] }
    : RDef                            { [$1]    }
    | RDef RDefs                      { $1 : $2 }

RDef :: { (SRegname, Regexp ta) }
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
    : RPrim '*'                       { IterZeroMany $1       }
    | RPrim '+'                       { IterOneMany  $1       }
    | RPrim '?'                       { IterMaybe    $1       }
    | RPrim '{' int '}'               { IterRepeat   $1 $3    }
    | RPrim '{' int ',' int '}'       { IterRange    $1 $3 $5 }
    | RPrim                           { IterFromPrim $1       }

RPrim :: { RegexpPrim ta }
    : name                            { Name         $1 }
    | '"' chain '"'                   { Elementary   $2 }
    | '\'' chain '\''                 { Elementary   $2 }
    | '(' RAlt ')'                    { Wrapped      $2 }
    | '[' range ']'                   { Range        $2 }
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
    | TokenStar
    | TokenPlus
    | TokenChain       [ta]
    | TokenRange       (S.Set ta)
    | TokenName        String
    | TokenInt         Int
    deriving (Show)


parseError :: [Token ta] -> tb
parseError _ = err "Parse error"


analyze :: SCode -> M.HashMap STokname MTokname2TokID -> (STokname, Maybe MTokname2TokID, SCode)
analyze s ttbls =
    let s1 = "!cre2c_type:"
        s2 = (skip_spaces . drop (length s1)) s
    in  case s of
            s' | s1 `isPrefixOf` s' ->
                let (t, s3) = span is_alpha_num_ s2
                    e       = err $ "token table missing for token type " ++ t
                    ttbl    = case t of
                        "char" -> Nothing
                        _      -> Just (M.lookupDefault e t ttbls)
                in  (t, ttbl, s3)
            _                       -> err $ printf "missing %s directive in .def file" (show s1)


lexer :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lexer _    "" = []
lexer ttbl (c : cs)
    | isSpace c = lexer ttbl cs
    | isAlpha c =
        let (nm, rest) = lex_name (c : cs)
        in  TokenName nm : lexer ttbl rest
lexer ttbl ('='  : cs)        = TokenEq : lex_regexp ttbl cs
lexer ttbl ('-'  : '-'  : cs) = lex_comment ttbl cs
lexer _    s = err $ "cant't parse " ++ s


lex_comment :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lex_comment ttbl cs = lexer ttbl $ dropWhile (/= '\n') cs


lex_regexp :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lex_regexp _    [] = []
lex_regexp ttbl (c : cs)
    | isSpace c = lex_regexp ttbl cs
    | isAlpha c =
        let (nm, rest) = lex_name (c : cs)
        in  TokenName nm : lex_regexp ttbl rest
lex_regexp ttbl ('.'  : cs) = TokenDot          : lex_regexp  ttbl cs
lex_regexp ttbl ('?'  : cs) = TokenQueryMark    : lex_regexp  ttbl cs
lex_regexp ttbl ('*'  : cs) = TokenStar         : lex_regexp  ttbl cs
lex_regexp ttbl ('+'  : cs) = TokenPlus         : lex_regexp  ttbl cs
lex_regexp ttbl ('('  : cs) = TokenOBracket     : lex_regexp  ttbl cs
lex_regexp ttbl (')'  : cs) = TokenCBracket     : lex_regexp  ttbl cs
lex_regexp ttbl ('}'  : cs) = TokenCParenthesis : lex_regexp  ttbl cs
lex_regexp ttbl ('|'  : cs) = TokenVSlash       : lex_regexp  ttbl cs
lex_regexp ttbl ('{'  : cs) = TokenOParenthesis : lex_iters   ttbl cs
lex_regexp ttbl ('"'  : cs) = TokenDQuote       : lex_dqchain ttbl cs
lex_regexp ttbl ('\'' : cs) =                     lex_qchain  ttbl cs
lex_regexp ttbl ('['  : cs) =                     lex_range   ttbl cs
lex_regexp ttbl (';'  : cs) = TokenSemicolon    : lexer       ttbl cs


lex_iters :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lex_iters ttbl s =
    let (n, s1)  = lex_int s
    in  case skip_spaces s1 of
            ',' : s2 ->
                let (m, s3) = lex_int s2
                in  TokenInt n : TokenComma : TokenInt m : lex_regexp ttbl s3
            '}' : s2 -> TokenInt n : TokenCParenthesis : lex_regexp ttbl s2


lex_qchain' :: Labellable ta => [ta] -> [Token ta]
lex_qchain' []       = []
lex_qchain' (x : xs) = case span_case x of
    [y] -> TokenQuote      : TokenChain [y]             : TokenQuote      : lex_qchain' xs
    ys  -> TokenOSqBracket : TokenRange (S.fromList ys) : TokenCSqBracket : lex_qchain' xs


lex_qchain :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lex_qchain ttbl cs =
    let (ch, rest) = break_escaped '\'' cs
        ch'        = reads' ttbl ch
    in TokenOBracket : lex_qchain' ch' ++ TokenCBracket : lex_regexp ttbl rest


lex_dqchain :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lex_dqchain ttbl cs =
    let (ch, rest) = break_escaped '"' cs
    in  TokenChain (reads' ttbl ch) : TokenDQuote : lex_regexp ttbl rest


lex_range :: Labellable ta => Maybe MTokname2TokID -> SCode -> [Token ta]
lex_range ttbl cs =
    let (r1, cs') = break_escaped ']' cs
        r2        = case r1 of
            '^' : r3 -> (span_negative_range . reads' ttbl) r3
            _        -> (span_range . reads' ttbl) r1
    in  case r2 of
            [x] -> TokenDQuote : TokenChain [x] : TokenDQuote : lex_regexp ttbl cs'
            xs  -> TokenOSqBracket : TokenRange (S.fromList xs) : TokenCSqBracket : lex_regexp ttbl cs'


err :: String -> a
err s = error $ "*** RegexpParser : " ++ s


--------------------------------------------------------------------------------

parse_regexps :: Labellable ta => Maybe MTokname2TokID -> SCode -> MRegname2Regexp ta
parse_regexps ttbl = M.fromList . parser . lexer ttbl

parse_def_file :: M.HashMap STokname MTokname2TokID -> SCode -> Either (MRegname2Regexp Char) (STokname, MRegname2Regexp Int)
parse_def_file ttbls s =
    let (ttype, ttbl, s') = analyze s ttbls
    in  case ttbl of
            Nothing -> Left  (parse_regexps ttbl s')
            Just _  -> Right (ttype, parse_regexps ttbl s')

}


