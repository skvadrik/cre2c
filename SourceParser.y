{

module SourceParser
    ( parse_source
    ) where

import qualified Data.HashMap.Strict   as M hiding (lookupDefault)
import qualified Data.HashMap.Lazy     as M        (lookupDefault)
import           Data.Char
import           Data.List                         (foldl', isPrefixOf)
import qualified Data.DList            as DL
import qualified Data.Set              as S
import           Text.Printf

import           Types                      hiding (err)
import           Helpers

}

%name      parser
%tokentype { Token }
%error     { parseError }

%token
    name           { TName    $$    }
    code           { TCode    $$    }
    start          { TStart         }
    end            { TEnd           }
    option_name    { TOptionName    }
    option_type    { TOptionType    }
    option_default { TOptionDefault }
    type_char      { TypeChar       }
    ':'            { TColon         }
    '>'            { TAngle         }
    '='            { TEq            }

%%

Source :: { [Chunk] }
    : code                                             { [Ch1 $1] }
    | code start Rules end Source
        {
            let {
                chunks  = $5;
                options = Options (show $ length chunks) TTChar Nothing;
                rules   = fst $ foldl' insert_rule (M.empty, 0) $3;
                code    = $1
            }
            in  Ch2 code options rules : chunks
        }
    | code start Opts Rules end Source
        {
            let {
                chunks  = $6;
                options = case block_name $3 of {
                    "" -> $3{block_name = show $ length chunks};
                    _  -> $3
                };
                rules   = fst $ foldl' insert_rule (M.empty, 0) $4;
                code    = $1
            }
            in  Ch2 code options rules : chunks
        }

Opts :: { Options }
    :      OptName                                     { Options $1 TTChar Nothing  }
    |      OptType                                     { Options "" $1     Nothing  }
    |      OptDefault                                  { Options "" TTChar (Just $1)}
    | Opts OptName                                     { $1{block_name = $2}     }
    | Opts OptType                                     { $1{token_type = $2}     }
    | Opts OptDefault                                  { $1{default_action = Just $2} }

OptName :: { SBlkname }
    : option_name '=' name                             { $3 }

OptType :: { TokenType }
    : option_type '=' type_char                        { TTChar    }
    | option_type '=' name                             { TTEnum $3 }

OptDefault :: { SCode }
    : option_default '=' code                          { $3 }

Rules :: { [Rule] }
    : Rule                                             { [$1]    }
    | Rule Rules                                       { $1 : $2 }

Rule :: { Rule }
    :           name          code                     { ($1, Nothing, [], $2) }
    | Conds '>' name          code                     { ($3, Nothing, $1, $4) }
    |           name ':' name code                     { ($1, Just $3, [], $4) }
    | Conds '>' name ':' name code                     { ($3, Just $5, $1, $6) }

Conds :: { [SCond] }
    : name                                             { [$1]    }
    | name Conds                                       { $1 : $2 }


{
{-
data ParserState = ParserState { parser_name :: String }

then_ps :: ParserState a -> (a -> ParserState b) -> ParserState b
then_ps =

return_ps = a -> ParserState a
-}
type Rule
    = (SRegname, Maybe SBlkname, [SCond], SCode)

data Token
    = TEq
    | TAngle
    | TColon
    | TStart
    | TEnd
    | TOptionName
    | TOptionType
    | TOptionDefault
    | TypeChar
    | TCode    SCode
    | TName    String
    deriving (Show)


parseError :: [Token] -> a
parseError e = err $ "Parse error: " ++ show e


lexer :: String -> String -> [Token]
lexer s1 "" = [TCode s1]
lexer s1 s2@(x : xs) =
    let s_start = "/*cre2c"
    in  case s2 of
        _ | s_start `isPrefixOf` s2 -> TCode s1 : TStart : lex_block (drop (length s_start) s2)
        _                           -> lexer (s1 ++ [x]) xs


lex_block :: String -> [Token]
lex_block s =
    let s_end            = "cre2c*/"
        s_option_name    = "cre2c:name"
        s_option_type    = "cre2c:type"
        s_option_default = "cre2c:default"
    in  case s of
            _ | s_end            `isPrefixOf` s -> TEnd           : lexer "" (drop (length s_end) s)
            _ | s_option_name    `isPrefixOf` s -> TOptionName    : lex_block (drop (length s_option_name) s)
            _ | s_option_type    `isPrefixOf` s -> TOptionType    : lex_block (drop (length s_option_type) s)
            _ | s_option_default `isPrefixOf` s -> TOptionDefault : lex_block (drop (length s_option_default) s)
            '>' : xs                            -> TAngle    : lex_block xs
            ':' : xs                            -> TColon    : lex_block xs
            '=' : xs                            -> TEq       : lex_block xs
            xs@('{' : _)                        ->
                let (c, xs') = lex_code xs
                in  TCode c : lex_block xs'
            x : xs | x `elem` "\n\r\t "         -> lex_block xs
            xs                                  ->
                let (n, xs') = lex_name xs
                in  TName n : lex_block xs'


lex_code :: String -> (SCode, String)
lex_code ('{' : s) =
    let lex_code' 0 tok rest         = ((DL.toList . DL.cons '{') tok, rest)
        lex_code' i tok ""           = err "lex_code: invalid code block"
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (DL.snoc tok '{') rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (DL.snoc tok '}') rest
        lex_code' i tok (c   : rest) = lex_code' i       (DL.snoc tok c)   rest
    in  lex_code' 1 DL.empty s
lex_code _         = err "lex_code : missing '{' before code block"


err :: String -> a
err s = error $ "*** SourceParser : " ++ s


--------------------------------------------------------------------------------


combine_rules :: (IRegID, (SBlkname, MCondset2Code)) -> (IRegID, (SBlkname, MCondset2Code)) -> (IRegID, (SBlkname, MCondset2Code))
combine_rules (_, (b,  _         )) (_,  (b', _          )) | b /= b' = err "rules for one regexp lead to different blocks"
combine_rules (k, (b,  conds2code)) (k', (_,  conds2code'))           =
    let conds2code'' = M.foldlWithKey'
            (\ c2c conds code -> M.insertWith
                (\ _ code' -> concat ["{ ", code, code', " }"])
                conds
                code
                conds2code
            ) conds2code' conds2code
    in  (min k k', (b, conds2code''))


insert_rule :: (MRegname2RegInfo, Int) -> Rule -> (MRegname2RegInfo, Int)
insert_rule (rules, k) (name, block, conds, code) =
    let block' = case block of
            Just b  -> b
            Nothing -> "main"
        rules' = M.insertWith
            combine_rules
            name
            (k, (block', M.insert (S.fromList conds) code M.empty))
            rules
    in  (rules', k + 1)


parse_source :: String -> [Chunk]
parse_source = parser . lexer ""

}










