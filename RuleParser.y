{

module RuleParser
    ( parse_rules
    ) where

import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import           Data.Char

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
    : Rule                               { OneRule   $1    }
    | Rule Rules                         { ManyRules $1 $2 }

Rule
    : name '=' '{' code '}'              { SimpleRule  $1 $4    }
    | CondList '>' name '=' '{' code '}' { ComplexRule $1 $3 $6 }

CondList
    : name                               { OneCond   $1    }
    | name CondList                      { ManyConds $1 $2 }


{

data Token
    = TokenEq
    | TokenCode BS.ByteString
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
      | isAlpha c = lex_name (c : cs)
lexer ('>'  : cs) = TokenAngle        : lex_name cs
lexer ('='  : cs) = TokenEq           : lexer cs
lexer ('{'  : cs) = TokenOParenthesis : lex_code cs


lex_code :: String -> [Token]
lex_code cs =
    let lex_code' :: Int -> String -> String -> (String, String)
        lex_code' 0 tok rest         = (init tok, rest)
        lex_code' i tok ""           = undefined
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (tok ++ "{") rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (tok ++ "}") rest
        lex_code' i tok (r : est)    = lex_code' i (tok ++ [r]) est
        (code, rest) =  lex_code' 1 "" cs
    in  TokenOParenthesis : TokenCode (BS.pack code) : TokenCParenthesis : lexer rest


lex_name :: String -> [Token]
lex_name cs =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = lexer rest
    in  if nm /= "" then TokenName nm : rest' else rest'


--------------------------------------------------------------------------------
parse_rules :: String -> RuleTable
parse_rules =
    let conds2list :: CondList -> [Cond]
        conds2list (OneCond c)      = [c]
        conds2list (ManyConds c cs) = c : conds2list cs

        rules2table :: Rules -> [([Cond], RegexpName, Code)]
        rules2table (OneRule   (SimpleRule           name code))       = [([],                  name, code)]
        rules2table (OneRule   (ComplexRule condlist name code))       = [(conds2list condlist, name, code)]
        rules2table (ManyRules (SimpleRule           name code) rules) = ([],                  name, code) : rules2table rules
        rules2table (ManyRules (ComplexRule condlist name code) rules) = (conds2list condlist, name, code) : rules2table rules

    in  M.fromList . (\ rt -> zip [0 .. length rt] rt) . rules2table . parser . lexer

}










