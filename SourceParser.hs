{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Chunk])
	| HappyAbsSyn5 (Options)
	| HappyAbsSyn6 (SBlkname)
	| HappyAbsSyn7 (TokenType)
	| HappyAbsSyn8 (SCode)
	| HappyAbsSyn9 ([Rule])
	| HappyAbsSyn10 (Rule)
	| HappyAbsSyn11 ([SCond])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (13) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (13) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (23) = happyAccept
action_3 _ = happyFail

action_4 (14) = happyShift action_5
action_4 _ = happyReduce_1

action_5 (12) = happyShift action_13
action_5 (16) = happyShift action_14
action_5 (17) = happyShift action_15
action_5 (18) = happyShift action_16
action_5 (5) = happyGoto action_6
action_5 (6) = happyGoto action_7
action_5 (7) = happyGoto action_8
action_5 (8) = happyGoto action_9
action_5 (9) = happyGoto action_10
action_5 (10) = happyGoto action_11
action_5 (11) = happyGoto action_12
action_5 _ = happyFail

action_6 (12) = happyShift action_13
action_6 (16) = happyShift action_14
action_6 (17) = happyShift action_15
action_6 (18) = happyShift action_16
action_6 (6) = happyGoto action_27
action_6 (7) = happyGoto action_28
action_6 (8) = happyGoto action_29
action_6 (9) = happyGoto action_30
action_6 (10) = happyGoto action_11
action_6 (11) = happyGoto action_12
action_6 _ = happyFail

action_7 _ = happyReduce_4

action_8 _ = happyReduce_5

action_9 _ = happyReduce_6

action_10 (15) = happyShift action_26
action_10 _ = happyFail

action_11 (12) = happyShift action_13
action_11 (9) = happyGoto action_25
action_11 (10) = happyGoto action_11
action_11 (11) = happyGoto action_12
action_11 _ = happyReduce_14

action_12 (21) = happyShift action_24
action_12 _ = happyFail

action_13 (12) = happyShift action_21
action_13 (13) = happyShift action_22
action_13 (20) = happyShift action_23
action_13 (11) = happyGoto action_20
action_13 _ = happyReduce_20

action_14 (22) = happyShift action_19
action_14 _ = happyFail

action_15 (22) = happyShift action_18
action_15 _ = happyFail

action_16 (22) = happyShift action_17
action_16 _ = happyFail

action_17 (13) = happyShift action_38
action_17 _ = happyFail

action_18 (12) = happyShift action_36
action_18 (19) = happyShift action_37
action_18 _ = happyFail

action_19 (12) = happyShift action_35
action_19 _ = happyFail

action_20 _ = happyReduce_21

action_21 (12) = happyShift action_21
action_21 (11) = happyGoto action_20
action_21 _ = happyReduce_20

action_22 _ = happyReduce_16

action_23 (12) = happyShift action_34
action_23 _ = happyFail

action_24 (12) = happyShift action_33
action_24 _ = happyFail

action_25 _ = happyReduce_15

action_26 (13) = happyShift action_4
action_26 (4) = happyGoto action_32
action_26 _ = happyFail

action_27 _ = happyReduce_7

action_28 _ = happyReduce_8

action_29 _ = happyReduce_9

action_30 (15) = happyShift action_31
action_30 _ = happyFail

action_31 (13) = happyShift action_4
action_31 (4) = happyGoto action_42
action_31 _ = happyFail

action_32 _ = happyReduce_2

action_33 (13) = happyShift action_40
action_33 (20) = happyShift action_41
action_33 _ = happyFail

action_34 (13) = happyShift action_39
action_34 _ = happyFail

action_35 _ = happyReduce_10

action_36 _ = happyReduce_12

action_37 _ = happyReduce_11

action_38 _ = happyReduce_13

action_39 _ = happyReduce_18

action_40 _ = happyReduce_17

action_41 (12) = happyShift action_43
action_41 _ = happyFail

action_42 _ = happyReduce_3

action_43 (13) = happyShift action_44
action_43 _ = happyFail

action_44 _ = happyReduce_19

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TCode    happy_var_1))
	 =  HappyAbsSyn4
		 ([Ch1 happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 5 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCode    happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let {
                chunks  = happy_var_5;
                options = Options (show $ length chunks) TTChar Nothing;
                rules   = fst $ foldl' insert_rule (M.empty, 0) happy_var_3;
                code    = happy_var_1
            }
            in  Ch2 code options rules : chunks
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 6 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCode    happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let {
                chunks  = happy_var_6;
                options = case block_name happy_var_3 of {
                    "" -> happy_var_3{block_name = show $ length chunks};
                    _  -> happy_var_3
                };
                rules   = fst $ foldl' insert_rule (M.empty, 0) happy_var_4;
                code    = happy_var_1
            }
            in  Ch2 code options rules : chunks
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Options happy_var_1 TTChar Nothing
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (Options "" happy_var_1     Nothing
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (Options "" TTChar (Just happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1{block_name = happy_var_2}
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1{token_type = happy_var_2}
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1{default_action = Just happy_var_2}
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyTerminal (TName    happy_var_3))
	_
	_
	 =  HappyAbsSyn6
		 (happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 _
	_
	_
	 =  HappyAbsSyn7
		 (TTChar
	)

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyTerminal (TName    happy_var_3))
	_
	_
	 =  HappyAbsSyn7
		 (TTEnum happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyTerminal (TCode    happy_var_3))
	_
	_
	 =  HappyAbsSyn8
		 (happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  10 happyReduction_16
happyReduction_16 (HappyTerminal (TCode    happy_var_2))
	(HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn10
		 ((happy_var_1, Nothing, [], happy_var_2)
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 10 happyReduction_17
happyReduction_17 ((HappyTerminal (TCode    happy_var_4)) `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_3, Nothing, happy_var_1, happy_var_4)
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 10 happyReduction_18
happyReduction_18 ((HappyTerminal (TCode    happy_var_4)) `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_1, Just happy_var_3, [], happy_var_4)
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 10 happyReduction_19
happyReduction_19 ((HappyTerminal (TCode    happy_var_6)) `HappyStk`
	(HappyTerminal (TName    happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_3, Just happy_var_5, happy_var_1, happy_var_6)
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 23 23 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TName    happy_dollar_dollar -> cont 12;
	TCode    happy_dollar_dollar -> cont 13;
	TStart -> cont 14;
	TEnd -> cont 15;
	TOptionName -> cont 16;
	TOptionType -> cont 17;
	TOptionDefault -> cont 18;
	TypeChar -> cont 19;
	TColon -> cont 20;
	TAngle -> cont 21;
	TEq -> cont 22;
	_ -> happyError' (tk:tks)
	}

happyError_ 23 tk tks = happyError' tks
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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
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
