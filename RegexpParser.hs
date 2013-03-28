{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token ta)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([(SRegname, Regexp ta)])
	| HappyAbsSyn5 ((SRegname, Regexp ta))
	| HappyAbsSyn6 (Regexp ta)
	| HappyAbsSyn7 (RegexpAlt ta)
	| HappyAbsSyn8 (RegexpCat ta)
	| HappyAbsSyn9 (RegexpIter ta)
	| HappyAbsSyn10 (RegexpPrim ta)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token ta)
	-> HappyState (Token ta) (HappyStk HappyAbsSyn -> [(Token ta)] -> m HappyAbsSyn)
	-> [HappyState (Token ta) (HappyStk HappyAbsSyn -> [(Token ta)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token ta)] -> m HappyAbsSyn
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
 action_39 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token ta)
	-> HappyState (Token ta) (HappyStk HappyAbsSyn -> [(Token ta)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token ta) (HappyStk HappyAbsSyn -> [(Token ta)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token ta)] -> (HappyIdentity) HappyAbsSyn)

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
 happyReduce_20 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token ta)
	-> HappyState (Token ta) (HappyStk HappyAbsSyn -> [(Token ta)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token ta) (HappyStk HappyAbsSyn -> [(Token ta)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token ta)] -> (HappyIdentity) HappyAbsSyn)

action_0 (11) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 _ = happyFail

action_1 (11) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (27) = happyShift action_7
action_3 _ = happyFail

action_4 (31) = happyAccept
action_4 _ = happyFail

action_5 (11) = happyShift action_3
action_5 (4) = happyGoto action_6
action_5 (5) = happyGoto action_5
action_5 _ = happyReduce_1

action_6 _ = happyReduce_2

action_7 (11) = happyShift action_13
action_7 (15) = happyShift action_14
action_7 (17) = happyShift action_15
action_7 (23) = happyShift action_16
action_7 (24) = happyShift action_17
action_7 (25) = happyShift action_18
action_7 (6) = happyGoto action_8
action_7 (7) = happyGoto action_9
action_7 (8) = happyGoto action_10
action_7 (9) = happyGoto action_11
action_7 (10) = happyGoto action_12
action_7 _ = happyFail

action_8 (28) = happyShift action_29
action_8 _ = happyFail

action_9 _ = happyReduce_4

action_10 (22) = happyShift action_28
action_10 _ = happyReduce_6

action_11 (11) = happyShift action_13
action_11 (15) = happyShift action_14
action_11 (17) = happyShift action_15
action_11 (23) = happyShift action_16
action_11 (24) = happyShift action_17
action_11 (25) = happyShift action_18
action_11 (8) = happyGoto action_27
action_11 (9) = happyGoto action_11
action_11 (10) = happyGoto action_12
action_11 _ = happyReduce_8

action_12 (19) = happyShift action_23
action_12 (26) = happyShift action_24
action_12 (29) = happyShift action_25
action_12 (30) = happyShift action_26
action_12 _ = happyReduce_14

action_13 _ = happyReduce_15

action_14 (11) = happyShift action_13
action_14 (15) = happyShift action_14
action_14 (17) = happyShift action_15
action_14 (23) = happyShift action_16
action_14 (24) = happyShift action_17
action_14 (25) = happyShift action_18
action_14 (7) = happyGoto action_22
action_14 (8) = happyGoto action_10
action_14 (9) = happyGoto action_11
action_14 (10) = happyGoto action_12
action_14 _ = happyFail

action_15 (13) = happyShift action_21
action_15 _ = happyFail

action_16 (12) = happyShift action_20
action_16 _ = happyFail

action_17 (12) = happyShift action_19
action_17 _ = happyFail

action_18 _ = happyReduce_20

action_19 (24) = happyShift action_35
action_19 _ = happyFail

action_20 (23) = happyShift action_34
action_20 _ = happyFail

action_21 (18) = happyShift action_33
action_21 _ = happyFail

action_22 (16) = happyShift action_32
action_22 _ = happyFail

action_23 (14) = happyShift action_31
action_23 _ = happyFail

action_24 _ = happyReduce_11

action_25 _ = happyReduce_9

action_26 _ = happyReduce_10

action_27 _ = happyReduce_7

action_28 (11) = happyShift action_13
action_28 (15) = happyShift action_14
action_28 (17) = happyShift action_15
action_28 (23) = happyShift action_16
action_28 (24) = happyShift action_17
action_28 (25) = happyShift action_18
action_28 (7) = happyGoto action_30
action_28 (8) = happyGoto action_10
action_28 (9) = happyGoto action_11
action_28 (10) = happyGoto action_12
action_28 _ = happyFail

action_29 _ = happyReduce_3

action_30 _ = happyReduce_5

action_31 (20) = happyShift action_36
action_31 (21) = happyShift action_37
action_31 _ = happyFail

action_32 _ = happyReduce_18

action_33 _ = happyReduce_19

action_34 _ = happyReduce_17

action_35 _ = happyReduce_16

action_36 _ = happyReduce_12

action_37 (14) = happyShift action_38
action_37 _ = happyFail

action_38 (20) = happyShift action_39
action_38 _ = happyFail

action_39 _ = happyReduce_13

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Regexp       happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Alt          happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (AltFromCat   happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Cat          happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (CatFromIter  happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (IterZeroMany happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (IterOneMany  happy_var_1
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (IterMaybe    happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 9 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (IterRepeat   happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 9 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (IterRange    happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (IterFromPrim happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 (Name         happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 _
	(HappyTerminal (TokenChain happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Elementary   happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 _
	(HappyTerminal (TokenChain happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Elementary   happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Wrapped      happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 _
	(HappyTerminal (TokenRange happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Range        happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn10
		 (Any
	)

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 11;
	TokenChain happy_dollar_dollar -> cont 12;
	TokenRange happy_dollar_dollar -> cont 13;
	TokenInt happy_dollar_dollar -> cont 14;
	TokenOBracket -> cont 15;
	TokenCBracket -> cont 16;
	TokenOSqBracket -> cont 17;
	TokenCSqBracket -> cont 18;
	TokenOParenthesis -> cont 19;
	TokenCParenthesis -> cont 20;
	TokenComma -> cont 21;
	TokenVSlash -> cont 22;
	TokenQuote -> cont 23;
	TokenDQuote -> cont 24;
	TokenDot -> cont 25;
	TokenQueryMark -> cont 26;
	TokenEq -> cont 27;
	TokenSemicolon -> cont 28;
	TokenStar -> cont 29;
	TokenPlus -> cont 30;
	_ -> happyError' (tk:tks)
	}

happyError_ 31 tk tks = happyError' tks
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
happyError' :: () => [(Token ta)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
