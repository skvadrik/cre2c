{-# OPTIONS_GHC -w #-}
module SourceParser
    ( parse_source
    ) where

import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList            as DL
import           Control.Applicative         ((<$>))
import           Data.Char
import           Data.List                   (foldl')
import qualified Data.Set              as S
import           Text.Printf

import           Types

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Chunk])
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 ([Rule])
	| HappyAbsSyn8 (Rule)
	| HappyAbsSyn11 ([Cond])

action_0 (13) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (13) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (25) = happyAccept
action_3 _ = happyFail

action_4 (19) = happyShift action_5
action_4 _ = happyReduce_1

action_5 (21) = happyShift action_7
action_5 (22) = happyShift action_8
action_5 (23) = happyShift action_9
action_5 (24) = happyShift action_10
action_5 (5) = happyGoto action_6
action_5 _ = happyFail

action_6 (12) = happyShift action_21
action_6 (20) = happyShift action_22
action_6 (6) = happyGoto action_15
action_6 (7) = happyGoto action_16
action_6 (8) = happyGoto action_17
action_6 (9) = happyGoto action_18
action_6 (10) = happyGoto action_19
action_6 (11) = happyGoto action_20
action_6 _ = happyFail

action_7 (22) = happyShift action_13
action_7 (23) = happyShift action_14
action_7 _ = happyReduce_4

action_8 (23) = happyShift action_12
action_8 _ = happyReduce_5

action_9 _ = happyReduce_6

action_10 (22) = happyShift action_11
action_10 _ = happyReduce_11

action_11 _ = happyReduce_12

action_12 _ = happyReduce_9

action_13 (23) = happyShift action_36
action_13 _ = happyReduce_7

action_14 _ = happyReduce_8

action_15 (20) = happyShift action_35
action_15 _ = happyFail

action_16 _ = happyReduce_13

action_17 (12) = happyShift action_34
action_17 (7) = happyGoto action_32
action_17 (8) = happyGoto action_17
action_17 (11) = happyGoto action_33
action_17 _ = happyReduce_15

action_18 _ = happyReduce_14

action_19 (12) = happyShift action_31
action_19 (9) = happyGoto action_29
action_19 (10) = happyGoto action_19
action_19 (11) = happyGoto action_30
action_19 _ = happyReduce_19

action_20 (14) = happyShift action_28
action_20 _ = happyFail

action_21 (12) = happyShift action_25
action_21 (15) = happyShift action_26
action_21 (18) = happyShift action_27
action_21 (11) = happyGoto action_24
action_21 _ = happyReduce_23

action_22 (13) = happyShift action_4
action_22 (4) = happyGoto action_23
action_22 _ = happyFail

action_23 _ = happyReduce_3

action_24 _ = happyReduce_24

action_25 (12) = happyShift action_25
action_25 (11) = happyGoto action_24
action_25 _ = happyReduce_23

action_26 (12) = happyShift action_42
action_26 _ = happyFail

action_27 (16) = happyShift action_41
action_27 _ = happyFail

action_28 (12) = happyShift action_40
action_28 _ = happyFail

action_29 _ = happyReduce_20

action_30 (14) = happyShift action_39
action_30 _ = happyFail

action_31 (12) = happyShift action_25
action_31 (15) = happyShift action_26
action_31 (11) = happyGoto action_24
action_31 _ = happyReduce_23

action_32 _ = happyReduce_16

action_33 (14) = happyShift action_38
action_33 _ = happyFail

action_34 (12) = happyShift action_25
action_34 (18) = happyShift action_27
action_34 (11) = happyGoto action_24
action_34 _ = happyReduce_23

action_35 (13) = happyShift action_4
action_35 (4) = happyGoto action_37
action_35 _ = happyFail

action_36 _ = happyReduce_10

action_37 _ = happyReduce_2

action_38 (12) = happyShift action_48
action_38 _ = happyFail

action_39 (12) = happyShift action_47
action_39 _ = happyFail

action_40 (15) = happyShift action_45
action_40 (18) = happyShift action_46
action_40 _ = happyFail

action_41 (13) = happyShift action_44
action_41 _ = happyFail

action_42 (18) = happyShift action_43
action_42 _ = happyFail

action_43 (16) = happyShift action_52
action_43 _ = happyFail

action_44 (17) = happyShift action_51
action_44 _ = happyFail

action_45 (12) = happyShift action_50
action_45 _ = happyFail

action_46 (16) = happyShift action_49
action_46 _ = happyFail

action_47 (15) = happyShift action_45
action_47 _ = happyFail

action_48 (18) = happyShift action_46
action_48 _ = happyFail

action_49 (13) = happyShift action_55
action_49 _ = happyFail

action_50 (18) = happyShift action_54
action_50 _ = happyFail

action_51 _ = happyReduce_17

action_52 (13) = happyShift action_53
action_52 _ = happyFail

action_53 (17) = happyShift action_58
action_53 _ = happyFail

action_54 (16) = happyShift action_57
action_54 _ = happyFail

action_55 (17) = happyShift action_56
action_55 _ = happyFail

action_56 _ = happyReduce_18

action_57 (13) = happyShift action_59
action_57 _ = happyFail

action_58 _ = happyReduce_21

action_59 (17) = happyShift action_60
action_59 _ = happyFail

action_60 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenCode happy_var_1))
	 =  HappyAbsSyn4
		 ([Ch1 happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((Ch2 happy_var_1 happy_var_3 (foldl' insert_rule M.empty happy_var_4)) : happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (error "*** SourceParser : empty rule list; useless scanner block."
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (TokenMode happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      happy_var_1     U8  Longest
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (TokenType happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      Normal happy_var_1  Longest
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyTerminal (TokenMatch happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      Normal U8  happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyTerminal (TokenType happy_var_2))
	(HappyTerminal (TokenMode happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      happy_var_1     happy_var_2  Longest
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyTerminal (TokenMatch happy_var_2))
	(HappyTerminal (TokenMode happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      happy_var_1     U8  happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyTerminal (TokenMatch happy_var_2))
	(HappyTerminal (TokenType happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      Normal happy_var_1  happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyTerminal (TokenMatch happy_var_3))
	(HappyTerminal (TokenType happy_var_2))
	(HappyTerminal (TokenMode happy_var_1))
	 =  HappyAbsSyn5
		 (Opts      happy_var_1     happy_var_2  happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 (HappyTerminal (TokenBlock happy_var_1))
	 =  HappyAbsSyn5
		 (OptsBlock happy_var_1     U8
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyTerminal (TokenType happy_var_2))
	(HappyTerminal (TokenBlock happy_var_1))
	 =  HappyAbsSyn5
		 (OptsBlock happy_var_1     happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  7 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 5 8 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, Nothing, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 7 8 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_3, Nothing, happy_var_1, happy_var_6)
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  9 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 7 10 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, Just happy_var_3, [], happy_var_6)
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 9 10 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_8)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_3, Just happy_var_5, happy_var_1, happy_var_8)
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  11 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 12;
	TokenCode happy_dollar_dollar -> cont 13;
	TokenAngle -> cont 14;
	TokenColon -> cont 15;
	TokenOParenthesis -> cont 16;
	TokenCParenthesis -> cont 17;
	TokenEq -> cont 18;
	TokenStart -> cont 19;
	TokenEnd -> cont 20;
	TokenMode happy_dollar_dollar -> cont 21;
	TokenType happy_dollar_dollar -> cont 22;
	TokenMatch happy_dollar_dollar -> cont 23;
	TokenBlock happy_dollar_dollar -> cont 24;
	_ -> happyError' (tk:tks)
	}

happyError_ 25 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Rule
    = (RegexpName, Maybe BlockName, [Cond], Code)

data Source
    = SourceEnd  Code
    | Source     Code Options [Rule] Source

data Token
    = TokenEq
    | TokenCode BS.ByteString
    | TokenName String
    | TokenOParenthesis
    | TokenCParenthesis
    | TokenAngle
    | TokenColon
    | TokenStart
    | TokenEnd
    | TokenMode  Mode
    | TokenType  Type
    | TokenMatch Match
    | TokenBlock BlockName
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer ::  String -> [Token]
lexer [] = []
lexer ('/':'*':'s':'t':'a':'r':'t':':': cs) =
    let default_opts = Opts Normal U8 Longest
    in  TokenStart : lex_options cs default_opts
lexer cs =
    let lex_entry_code :: DL.DList Char -> String -> (DL.DList Char, String)
        lex_entry_code code "" = (code, "")
        lex_entry_code code ('\n':'/':'*':'s':'t':'a':'r':'t':':':cs) = (code, cs)
        lex_entry_code code (c : cs) = lex_entry_code (DL.snoc code c) cs
        (code, rest) = lex_entry_code DL.empty cs
        default_opts = Opts Normal U8 Longest
    in  TokenCode ((BS.pack . DL.toList) code) : (if rest == "" then [] else TokenStart : lex_options rest default_opts)


lex_options :: String -> Options -> [Token]
lex_options ('!':'c':'r':'e':'2':'c':'_':'t':'y':'p':'e':':':cs)     opts = lex_type cs  opts
lex_options ('!':'c':'r':'e':'2':'c':'_':'m':'o':'d':'e':':':cs)     opts = lex_mode cs  opts
lex_options ('!':'c':'r':'e':'2':'c':'_':'m':'a':'t':'c':'h':':':cs) opts = lex_match cs opts
lex_options (c:cs)  opts | isSpace c = lex_options cs opts
lex_options s@(c:_) opts | isAlpha c = case opts of
    OptsBlock _ _ -> lex_rules_b s
    _             -> lex_rules   s


lex_type :: String -> Options -> [Token]
lex_type cs opts =
    let (ttype, cs') = (span isAlphaNum . dropWhile isSpace) cs
    in  case ttype of
            t | t == "U8B"  -> TokenType U8  : lex_options cs' (opts{ token_type = U8 })
            t | t == "U32B" -> TokenType U32 : lex_options cs' (opts{ token_type = U32 })
            _               -> error $ printf "*** SourceParser: unknown token_type: %s" ttype


lex_mode :: String -> Options -> [Token]
lex_mode cs opts =
    let (mode, cs') = (span isAlpha . dropWhile isSpace) cs
    in  case mode of
            m | m == "single" -> TokenMode Single : lex_options cs' (opts{ mode = Single })
            m | m == "normal" -> TokenMode Normal : lex_options cs' (opts{ mode = Normal })
            m | m == "block"  ->
                let (block, cs'') = lex_blockname cs'
                in  TokenBlock block : lex_options cs' (OptsBlock block (token_type opts))
            _                 -> error $ printf "*** SourceParser: unknown mode: %s" mode


lex_match :: String -> Options -> [Token]
lex_match cs opts =
    let (match, cs') = (span isAlphaNum . dropWhile isSpace) cs
    in  case match of
            m | m == "longest"  -> TokenMatch Longest : lex_options cs' (opts{ match = Longest })
            m | m == "all"      -> TokenMatch All     : lex_options cs' (opts{ match = All })
            _                   -> error $ printf "*** SourceParser: unknown match: %s" match


lex_blockname :: String -> (BlockName, String)
lex_blockname ('(':cs) =
    let (block, rest) = span (\ c -> isAlpha c || c == '_') cs
    in  case (block, rest) of
            ("", _)                            -> error "*** SourceParser: block name not specified."
            (_,  r) | r == "" || head r /= ')' -> error $ printf "*** SourceParser: missing ')' after \"cre2c: block(%s\"" block
            _                                  -> (block, tail rest)
lex_block cs = error "*** SourceParser: missing '(' after \"block\" in \"cre2c_mode:\" directive"


lex_rules :: String -> [Token]
lex_rules [] = []
lex_rules ('\n' : 'e' : 'n' : 'd' : '*' : '/' : cs) = TokenEnd : lexer cs
lex_rules (c : cs)
    | isSpace c = lex_rules cs
    | isAlpha c = lex_name (c : cs) lex_rules
lex_rules ('>'  : cs) = TokenAngle        : lex_name cs lex_rules
lex_rules ('='  : cs) = TokenEq           : lex_rules cs
lex_rules ('{'  : cs) = TokenOParenthesis : lex_code cs lex_rules
lex_rules cs          = error $ printf "*** SourceParser : lex_rules : can't parse rules at %s" $ (show . take 50) cs


lex_rules_b :: String -> [Token]
lex_rules_b [] = []
lex_rules_b ('\n' : 'e' : 'n' : 'd' : '*' : '/' : cs) = TokenEnd : lexer cs
lex_rules_b (c : cs)
    | isSpace c = lex_rules_b cs
    | isAlpha c = lex_name (c : cs) lex_rules_b
lex_rules_b (':'  : cs) = TokenColon : lex_name cs lex_rules_b
lex_rules_b ('>'  : cs) = TokenAngle        : lex_name cs lex_rules_b
lex_rules_b ('='  : cs) = TokenEq           : lex_rules_b cs
lex_rules_b ('{'  : cs) = TokenOParenthesis : lex_code cs lex_rules_b
lex_rules_b cs          = error $ printf "*** SourceParser : lex_rules_b : can't parse rules at %s" $ (show . take 50) cs


lex_code :: String -> (String -> [Token]) -> [Token]
lex_code cs lex =
    let lex_code' :: Int -> DL.DList Char -> String -> (DL.DList Char, String)
        lex_code' 0 tok rest         = (DL.cons '{' tok, rest)
        lex_code' i tok ""           = error "*** SourceParser: invalid code block in regexp definition"
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (DL.snoc tok '{') rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (DL.snoc tok '}') rest
        lex_code' i tok (c   : rest) = lex_code' i       (DL.snoc tok c)   rest
        (code, rest) = lex_code' 1 DL.empty cs
    in  TokenCode ((BS.pack . DL.toList) code) : TokenCParenthesis : lex rest


lex_name :: String -> (String -> [Token]) -> [Token]
lex_name cs lex =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = lex rest
    in  if nm /= "" then TokenName nm : rest' else rest'


--------------------------------------------------------------------------------


combine_rules :: (Maybe BlockName, Conds2Code) -> (Maybe BlockName, Conds2Code) -> (Maybe BlockName, Conds2Code)
combine_rules (Just b, _) (Just b', _) | b /= b' = error "*** SourceParser: rules for one regexp lead to different blocks"
combine_rules (Just b, _) (Nothing, _)           = error "*** SourceParser: combine_rules: dark magic..."
combine_rules (Nothing, _) (Just b, _)           = error "*** SourceParser: combine_rules: dark magic..."
combine_rules (b, conds2code) (b', conds2code')  =
    let conds2code'' = M.foldlWithKey'
            (\ c2c conds code -> M.insertWith
                (\ _ code' -> BS.concat [BS.pack "{ ", code, code', BS.pack " }"])
                conds
                code
                conds2code
            ) conds2code' conds2code
    in  (b, conds2code'')


insert_rule :: RuleTable -> Rule -> RuleTable
insert_rule rules (name, block, conds, code) = M.insertWith
    combine_rules
    name
    (block, M.insert (S.fromList conds) code M.empty)
    rules


parse_source :: FilePath -> IO [Chunk]
parse_source fp = (parser . lexer) <$> readFile fp
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
