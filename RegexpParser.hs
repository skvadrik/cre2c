{-# OPTIONS_GHC -w #-}
module RegexpParser (parseRegexp) where

import Data.Char
import Types

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

action_0 (9) = happyShift action_6
action_0 (12) = happyShift action_7
action_0 (14) = happyShift action_8
action_0 (20) = happyShift action_9
action_0 (21) = happyShift action_10
action_0 (22) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail

action_1 (9) = happyShift action_6
action_1 (12) = happyShift action_7
action_1 (14) = happyShift action_8
action_1 (20) = happyShift action_9
action_1 (21) = happyShift action_10
action_1 (22) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (19) = happyShift action_20
action_3 _ = happyReduce_3

action_4 (9) = happyShift action_6
action_4 (12) = happyShift action_7
action_4 (14) = happyShift action_8
action_4 (20) = happyShift action_9
action_4 (21) = happyShift action_10
action_4 (22) = happyShift action_11
action_4 (6) = happyGoto action_19
action_4 (7) = happyGoto action_4
action_4 (8) = happyGoto action_5
action_4 _ = happyReduce_5

action_5 (16) = happyShift action_17
action_5 (23) = happyShift action_18
action_5 _ = happyReduce_9

action_6 _ = happyReduce_10

action_7 (9) = happyShift action_6
action_7 (12) = happyShift action_7
action_7 (14) = happyShift action_8
action_7 (20) = happyShift action_9
action_7 (21) = happyShift action_10
action_7 (22) = happyShift action_11
action_7 (5) = happyGoto action_16
action_7 (6) = happyGoto action_3
action_7 (7) = happyGoto action_4
action_7 (8) = happyGoto action_5
action_7 _ = happyFail

action_8 (10) = happyShift action_15
action_8 _ = happyFail

action_9 (10) = happyShift action_14
action_9 _ = happyFail

action_10 (10) = happyShift action_13
action_10 _ = happyFail

action_11 _ = happyReduce_15

action_12 (24) = happyAccept
action_12 _ = happyFail

action_13 (21) = happyShift action_26
action_13 _ = happyFail

action_14 (20) = happyShift action_25
action_14 _ = happyFail

action_15 (15) = happyShift action_24
action_15 _ = happyFail

action_16 (13) = happyShift action_23
action_16 _ = happyFail

action_17 (11) = happyShift action_22
action_17 _ = happyFail

action_18 _ = happyReduce_6

action_19 _ = happyReduce_4

action_20 (9) = happyShift action_6
action_20 (12) = happyShift action_7
action_20 (14) = happyShift action_8
action_20 (20) = happyShift action_9
action_20 (21) = happyShift action_10
action_20 (22) = happyShift action_11
action_20 (5) = happyGoto action_21
action_20 (6) = happyGoto action_3
action_20 (7) = happyGoto action_4
action_20 (8) = happyGoto action_5
action_20 _ = happyFail

action_21 _ = happyReduce_2

action_22 (17) = happyShift action_27
action_22 (18) = happyShift action_28
action_22 _ = happyFail

action_23 _ = happyReduce_13

action_24 _ = happyReduce_14

action_25 _ = happyReduce_12

action_26 _ = happyReduce_11

action_27 _ = happyReduce_7

action_28 (11) = happyShift action_29
action_28 _ = happyFail

action_29 (17) = happyShift action_30
action_29 _ = happyFail

action_30 _ = happyReduce_8

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Regexp       happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Alt          happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (AltFromCat   happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Cat          happy_var_1 happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (CatFromIter  happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (IterMaybe    happy_var_1
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (IterRepeat   happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (IterRange    happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (IterFromPrim happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn8
		 (Name         happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 _
	(HappyTerminal (TokenChain happy_var_2))
	_
	 =  HappyAbsSyn8
		 (Elementary   happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	(HappyTerminal (TokenChain happy_var_2))
	_
	 =  HappyAbsSyn8
		 (Elementary   happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Wrapped      happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 _
	(HappyTerminal (TokenChain happy_var_2))
	_
	 =  HappyAbsSyn8
		 (Range        happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn8
		 (Any
	)

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 9;
	TokenChain happy_dollar_dollar -> cont 10;
	TokenInt happy_dollar_dollar -> cont 11;
	TokenOBracket -> cont 12;
	TokenCBracket -> cont 13;
	TokenOSqBracket -> cont 14;
	TokenCSqBracket -> cont 15;
	TokenOParenthesis -> cont 16;
	TokenCParenthesis -> cont 17;
	TokenComma -> cont 18;
	TokenVSlash -> cont 19;
	TokenQuote -> cont 20;
	TokenDQuote -> cont 21;
	TokenDot -> cont 22;
	TokenQueryMark -> cont 23;
	_ -> happyError' (tk:tks)
	}

happyError_ 24 tk tks = happyError' tks
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

parse_tokenized_regexp tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Token
    = TokenChain String
    | TokenName String
    | TokenInt Int
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
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer [] = []
lexer (c : cs)
      | isSpace c = lexer cs
      | isAlpha c = lexName (c : cs)
lexer ('.'  : cs) = TokenDot : lexer cs
lexer ('?'  : cs) = TokenQueryMark : lexer cs
lexer ('('  : cs) = TokenOBracket : lexer cs
lexer (')'  : cs) = TokenCBracket : lexer cs
lexer ('{'  : cs) = TokenOParenthesis : lexInt cs
lexer ('}'  : cs) = TokenCParenthesis : lexer cs
lexer ('"'  : cs) = TokenDQuote : lexDQuotedChain cs
lexer ('\'' : cs) = TokenQuote : lexQuotedChain cs
lexer ('['  : cs) = TokenOSqBracket : lexChain cs
lexer ('|'  : cs) = TokenVSlash : lexer cs

lexInt cs =
    let (num, rest) = span isDigit cs
    in  case rest of
            ',' : cs' -> TokenInt (read num) : TokenComma : lexInt cs'
            '}' : cs' -> TokenInt (read num) : TokenCParenthesis : lexer cs'

lexName cs = TokenName nm : lexer rest
    where (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs

lexDQuotedChain cs = TokenChain ch : TokenDQuote : lexer (tail rest)
    where (ch, rest) = span (/= '"') cs

lexQuotedChain cs = TokenChain ch : TokenQuote : lexer (tail rest)
    where (ch, rest) = span (/= '\'') cs

lexChain cs = TokenChain ch : TokenCSqBracket : lexer (tail rest)
    where (ch, rest) = span (/= ']') cs

parseRegexp = parse_tokenized_regexp . lexer
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
