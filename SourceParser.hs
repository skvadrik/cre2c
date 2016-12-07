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
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Chunk])
	| HappyAbsSyn5 ([Rule])
	| HappyAbsSyn7 (Rule)
	| HappyAbsSyn10 ([SCond])

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
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_17 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (12) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (12) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (19) = happyAccept
action_3 _ = happyFail

action_4 (17) = happyShift action_5
action_4 _ = happyReduce_1

action_5 (13) = happyShift action_6
action_5 _ = happyFail

action_6 (11) = happyShift action_13
action_6 (18) = happyShift action_14
action_6 (5) = happyGoto action_7
action_6 (6) = happyGoto action_8
action_6 (7) = happyGoto action_9
action_6 (8) = happyGoto action_10
action_6 (9) = happyGoto action_11
action_6 (10) = happyGoto action_12
action_6 _ = happyFail

action_7 (18) = happyShift action_27
action_7 _ = happyFail

action_8 _ = happyReduce_4

action_9 (11) = happyShift action_26
action_9 (6) = happyGoto action_24
action_9 (7) = happyGoto action_9
action_9 (10) = happyGoto action_25
action_9 _ = happyReduce_6

action_10 _ = happyReduce_5

action_11 (11) = happyShift action_23
action_11 (8) = happyGoto action_21
action_11 (9) = happyGoto action_11
action_11 (10) = happyGoto action_22
action_11 _ = happyReduce_10

action_12 (15) = happyShift action_20
action_12 _ = happyFail

action_13 (11) = happyShift action_17
action_13 (14) = happyShift action_18
action_13 (16) = happyShift action_19
action_13 (10) = happyGoto action_16
action_13 _ = happyReduce_16

action_14 (12) = happyShift action_4
action_14 (4) = happyGoto action_15
action_14 _ = happyFail

action_15 _ = happyReduce_3

action_16 _ = happyReduce_17

action_17 (11) = happyShift action_17
action_17 (10) = happyGoto action_16
action_17 _ = happyReduce_16

action_18 (11) = happyShift action_35
action_18 _ = happyFail

action_19 (12) = happyShift action_34
action_19 _ = happyFail

action_20 (11) = happyShift action_33
action_20 _ = happyFail

action_21 _ = happyReduce_11

action_22 (15) = happyShift action_32
action_22 _ = happyFail

action_23 (11) = happyShift action_17
action_23 (14) = happyShift action_18
action_23 (16) = happyShift action_31
action_23 (10) = happyGoto action_16
action_23 _ = happyReduce_16

action_24 _ = happyReduce_7

action_25 (15) = happyShift action_30
action_25 _ = happyFail

action_26 (11) = happyShift action_17
action_26 (16) = happyShift action_29
action_26 (10) = happyGoto action_16
action_26 _ = happyReduce_16

action_27 (12) = happyShift action_4
action_27 (4) = happyGoto action_28
action_27 _ = happyFail

action_28 _ = happyReduce_2

action_29 (12) = happyShift action_42
action_29 _ = happyFail

action_30 (11) = happyShift action_41
action_30 _ = happyFail

action_31 (12) = happyShift action_40
action_31 _ = happyFail

action_32 (11) = happyShift action_39
action_32 _ = happyFail

action_33 (14) = happyShift action_37
action_33 (16) = happyShift action_38
action_33 _ = happyFail

action_34 (11) = happyReduce_12
action_34 (18) = happyReduce_12
action_34 _ = happyReduce_12

action_35 (16) = happyShift action_36
action_35 _ = happyFail

action_36 (12) = happyShift action_47
action_36 _ = happyFail

action_37 (11) = happyShift action_46
action_37 _ = happyFail

action_38 (12) = happyShift action_45
action_38 _ = happyFail

action_39 (14) = happyShift action_37
action_39 (16) = happyShift action_44
action_39 _ = happyFail

action_40 _ = happyReduce_12

action_41 (16) = happyShift action_43
action_41 _ = happyFail

action_42 _ = happyReduce_8

action_43 (12) = happyShift action_50
action_43 _ = happyFail

action_44 (12) = happyShift action_49
action_44 _ = happyFail

action_45 (11) = happyReduce_13
action_45 (18) = happyReduce_13
action_45 _ = happyReduce_13

action_46 (16) = happyShift action_48
action_46 _ = happyFail

action_47 _ = happyReduce_14

action_48 (12) = happyShift action_51
action_48 _ = happyFail

action_49 _ = happyReduce_13

action_50 _ = happyReduce_9

action_51 _ = happyReduce_15

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TCode    happy_var_1))
	 =  HappyAbsSyn4
		 ([Ch1 happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyTerminal (TOptions happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TCode    happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((Ch2 happy_var_1 happy_var_3 (fst $ foldl' insert_rule (M.empty, 0) happy_var_4)) : happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (err "empty rule list; useless scanner block."
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyTerminal (TCode    happy_var_3))
	_
	(HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn7
		 ((happy_var_1, Nothing, [], happy_var_3)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 7 happyReduction_9
happyReduction_9 ((HappyTerminal (TCode    happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_3, Nothing, happy_var_1, happy_var_5)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyTerminal (TCode    happy_var_3))
	_
	(HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn7
		 ((happy_var_1, Nothing, [], happy_var_3)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 9 happyReduction_13
happyReduction_13 ((HappyTerminal (TCode    happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_3, Nothing, happy_var_1, happy_var_5)
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 9 happyReduction_14
happyReduction_14 ((HappyTerminal (TCode    happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_1, Just happy_var_3, [], happy_var_5)
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 7 9 happyReduction_15
happyReduction_15 ((HappyTerminal (TCode    happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TName    happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_3, Just happy_var_5, happy_var_1, happy_var_7)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TName    happy_var_1))
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 19 19 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TName    happy_dollar_dollar -> cont 11;
	TCode    happy_dollar_dollar -> cont 12;
	TOptions happy_dollar_dollar -> cont 13;
	TColon -> cont 14;
	TAngle -> cont 15;
	TEq -> cont 16;
	TStart -> cont 17;
	TEnd -> cont 18;
	_ -> happyError' (tk:tks)
	}

happyError_ 19 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
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
    = (SRegname, Maybe SBlkname, [SCond], SCode)

data Token
    = TEq
    | TAngle
    | TColon
    | TStart
    | TEnd
    | TCode    SCode
    | TName    String
    | TOptions Options
    deriving (Show)

data OptionSet = OptionSet
    { opt_mode        :: Mode
    , opt_match       :: Match
    , opt_block       :: Maybe SBlkname
    , opt_token_type  :: TokenType
    , opt_prelexer    :: Maybe String
    , opt_default     :: Maybe SCode
    }


parseError :: [Token] -> a
parseError e = err $ "Parse error: " ++ show e


lexer :: String -> [Token]
lexer s =
    let (entry, s') = lex_entry s
    in  TCode entry : case s' of
            "" -> []
            _  -> TStart : lex_options s'


lex_entry :: SCode -> (SCode, String)
lex_entry s =
    let s1 = "/*start:"
        lex_entry' entry rest = case rest of
            ""                    -> (DL.toList entry, "")
            r | s1 `isPrefixOf` r -> (DL.toList entry, drop (length s1) r)
            c : cs                -> lex_entry' (DL.snoc entry c) cs
    in  lex_entry' DL.empty s


lex_options :: String -> [Token]
lex_options s =
    let s1 = "!cre2c_mode:"
        s2 = "!cre2c_match:"
        s3 = "!cre2c_type:"
        s4 = "!cre2c_prelexer:"
        s5 = "!cre2c_default:"
        lex_options' (opts, rest) = case skip_spaces rest of
            r | s1 `isPrefixOf` r -> (lex_options' . lex_mode       opts . drop (length s1)) r
            r | s2 `isPrefixOf` r -> (lex_options' . lex_match      opts . drop (length s2)) r
            r | s3 `isPrefixOf` r -> (lex_options' . lex_token_type opts . drop (length s3)) r
            r | s4 `isPrefixOf` r -> (lex_options' . lex_prelexer   opts . drop (length s4)) r
            r | s5 `isPrefixOf` r -> (lex_options' . lex_default    opts . drop (length s5)) r
            _                     -> (opts, rest)
        def_opts    = OptionSet Normal Longest Nothing TTChar Nothing Nothing
        (opts', s') = lex_options' (def_opts, s)
    in  case opts' of
            OptionSet _    Longest (Just block) ttype@(TTEnum _) pl df -> TOptions (OptsBlock block ttype  pl df) : lex_rules_b s'
            OptionSet _    Longest (Just block) TTChar           pl df -> TOptions (OptsBlock block TTChar pl df) : lex_rules_b s'
            OptionSet mode match   Nothing      ttype@(TTEnum _) pl df -> TOptions (Opts mode match ttype  pl df) : lex_rules   s'
            OptionSet mode match   Nothing      TTChar           pl df -> TOptions (Opts mode match TTChar pl df) : lex_rules   s'
            _                                                             -> err "conflicting options"


lex_mode :: OptionSet -> String -> (OptionSet, String)
lex_mode opts s =
    let (m, s') = lex_name s
    in  case m of
            "single" -> (opts{opt_mode = Single}, s')
            "normal" -> (opts{opt_mode = Normal}, s')
            "block"  ->
                let (b, s'') = lex_blockname s'
                in  (opts{opt_block = Just b}, s'')
            _        -> err $ printf "unknown mode: %s" m


lex_match :: OptionSet -> String -> (OptionSet, String)
lex_match opts s =
    let (m, s') = lex_name s
    in  case m of
            "longest"  -> (opts{opt_match = Longest}, s')
            "all"      -> (opts{opt_match = All},     s')
            _          -> err $ printf "unknown match: %s" m


lex_token_type :: OptionSet -> String -> (OptionSet, String)
lex_token_type opts s =
    let (t, s') = lex_name s
    in  case t of
            "char" -> (opts{opt_token_type = TTChar},   s')
            _      -> (opts{opt_token_type = TTEnum t}, s')


lex_prelexer :: OptionSet -> String -> (OptionSet, String)
lex_prelexer opts s =
    let (pl, s') = lex_name s
    in  (opts{opt_prelexer = Just pl}, s')


lex_default :: OptionSet -> String -> (OptionSet, String)
lex_default opts s =
    let (df, s') = (lex_code . skip_spaces) s
    in  (opts{opt_default = Just df}, s')


lex_blockname :: String -> (SBlkname, String)
lex_blockname ('(':s) =
    case span is_alpha_num_ s of
        ("", _     ) -> err "block name not specified."
        (b,  ')':s') -> (b, s')
        (b,  _     ) -> err $ printf "missing ')' after \"cre2c: block(%s\"" b
lex_block s           = err "missing '(' after \"block\" in \"cre2c_mode:\" directive"


lex_rules :: String -> [Token]
lex_rules s =
    let s1 = "end*/"
        s2 = skip_spaces s
        s3 = drop (length s1) s2
    in  case s2 of
            s'       | s1 `isPrefixOf` s' -> TEnd : lexer s3
            s'@(c:_) | isAlpha c          -> let (nm, s'') = lex_name s' in TName nm : lex_rules s''
            '>' : s'                      -> let (nm, s'') = lex_name s' in TAngle : TName nm : lex_rules s''
            '=' : s'                      -> TEq : lex_rules s'
            '{' : s'                      -> let (cd, s'') = lex_code s2 in TCode cd : lex_rules s''
            s'                            -> err $ printf "lex_rules : can't parse rules at %s" $ (take 50 . show) s'


lex_rules_b :: String -> [Token]
lex_rules_b s =
    let s1 = "end*/"
        s2 = skip_spaces s
        s3 = drop (length s1) s2
    in  case s2 of
            s'       | s1 `isPrefixOf` s' -> TEnd : lexer s3
            s'@(c:_) | isAlpha c          -> let (nm, s'') = lex_name s' in TName nm : lex_rules_b s''
            ':' : s'                      -> let (nm, s'') = lex_name s' in TColon : TName nm : lex_rules_b s''
            '>' : s'                      -> let (nm, s'') = lex_name s' in TAngle : TName nm : lex_rules_b s''
            '=' : s'                      -> TEq : lex_rules_b s'
            '{' : s'                      -> let (cd, s'') = lex_code s2 in TCode cd : lex_rules_b s''
            s'                            -> err $ printf "lex_rules : can't parse rules at %s" $ (take 50 . show) s'


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


combine_rules :: (IRegID, (Maybe SBlkname, MCondset2Code)) -> (IRegID, (Maybe SBlkname, MCondset2Code)) -> (IRegID, (Maybe SBlkname, MCondset2Code))
combine_rules (_, (Just b,  _         )) (_,  (Just b', _          )) | b /= b' = err "rules for one regexp lead to different blocks"
combine_rules (_, (Just b,  _         )) (_,  (Nothing, _          ))           = err "combine_rules: dark magic..."
combine_rules (_, (Nothing, _         )) (_,  (Just b,  _          ))           = err "combine_rules: dark magic..."
combine_rules (k, (b,       conds2code)) (k', (b',      conds2code'))           =
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
    let rules' = M.insertWith
            combine_rules
            name
            (k, (block, M.insert (S.fromList conds) code M.empty))
            rules
    in  (rules', k + 1)


parse_source :: String -> [Chunk]
parse_source = parser . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4















































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib64/ghc-7.10.2.20151030/include/ghcversion.h" #-}


















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
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
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
