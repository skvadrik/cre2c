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

import           Types

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (9) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (9) = happyShift action_2
action_1 _ = happyFail

action_2 (14) = happyShift action_5
action_2 _ = happyFail

action_3 (17) = happyAccept
action_3 _ = happyFail

action_4 (14) = happyShift action_5
action_4 _ = happyReduce_2

action_5 (16) = happyShift action_6
action_5 _ = happyFail

action_6 (8) = happyShift action_10
action_6 (5) = happyGoto action_7
action_6 (6) = happyGoto action_8
action_6 (7) = happyGoto action_9
action_6 _ = happyFail

action_7 (15) = happyShift action_16
action_7 _ = happyFail

action_8 (8) = happyShift action_10
action_8 (5) = happyGoto action_15
action_8 (6) = happyGoto action_8
action_8 (7) = happyGoto action_9
action_8 _ = happyReduce_3

action_9 (10) = happyShift action_14
action_9 _ = happyFail

action_10 (8) = happyShift action_12
action_10 (13) = happyShift action_13
action_10 (7) = happyGoto action_11
action_10 _ = happyReduce_7

action_11 _ = happyReduce_8

action_12 (8) = happyShift action_12
action_12 (7) = happyGoto action_11
action_12 _ = happyReduce_7

action_13 (11) = happyShift action_19
action_13 _ = happyFail

action_14 (8) = happyShift action_18
action_14 _ = happyFail

action_15 _ = happyReduce_4

action_16 (9) = happyShift action_4
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_1

action_18 (13) = happyShift action_21
action_18 _ = happyFail

action_19 (9) = happyShift action_20
action_19 _ = happyFail

action_20 (12) = happyShift action_23
action_20 _ = happyFail

action_21 (11) = happyShift action_22
action_21 _ = happyFail

action_22 (9) = happyShift action_24
action_22 _ = happyFail

action_23 _ = happyReduce_5

action_24 (12) = happyShift action_25
action_24 _ = happyFail

action_25 _ = happyReduce_6

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyTerminal (TokenMode happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Source    happy_var_1 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenCode happy_var_1))
	 =  HappyAbsSyn4
		 (SourceEnd happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (OneRule   happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (ManyRules happy_var_1 happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SimpleRule  happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 7 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyTerminal (TokenCode happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ComplexRule happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn7
		 (OneCond   happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn7
		 (ManyConds happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 17 17 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 8;
	TokenCode happy_dollar_dollar -> cont 9;
	TokenAngle -> cont 10;
	TokenOParenthesis -> cont 11;
	TokenCParenthesis -> cont 12;
	TokenEq -> cont 13;
	TokenStart -> cont 14;
	TokenEnd -> cont 15;
	TokenMode happy_dollar_dollar -> cont 16;
	_ -> happyError' (tk:tks)
	}

happyError_ 17 tk tks = happyError' tks
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


data Source
    = Source     Code Mode Rules Source
    | SourceEnd  Code

data Token
    = TokenEq
    | TokenCode BS.ByteString
    | TokenName String
    | TokenOParenthesis
    | TokenCParenthesis
    | TokenAngle
    | TokenStart
    | TokenEnd
    | TokenMode Mode
    deriving (Show)


parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


lexer ::  String -> [Token]
lexer [] = []
lexer ('/':'*':'s':'t':'a':'r':'t':'_':'t':'o':'k':'e':'n':'i':'z':'e':':':cs) = TokenStart : TokenMode Tokenize : lex_rules cs
lexer ('/':'*':'s':'t':'a':'r':'t':'_':'o':'n':'c':'e':':':cs) = TokenStart : TokenMode Once : lex_rules cs
lexer ('/':'*':'s':'t':'a':'r':'t':':': cs) = TokenStart : TokenMode Normal : lex_rules cs
lexer cs =
    let lex_entry_code :: DL.DList Char -> String -> (DL.DList Char, String, Mode)
        lex_entry_code code "" = (code, "", Normal)
        lex_entry_code code ('\n':'/':'*':'s':'t':'a':'r':'t':'_':'t':'o':'k':'e':'n':'i':'z':'e':':':cs) = (code, cs, Tokenize)
        lex_entry_code code ('\n':'/':'*':'s':'t':'a':'r':'t':'_':'o':'n':'c':'e':':':cs) = (code, cs, Once)
        lex_entry_code code ('\n':'/':'*':'s':'t':'a':'r':'t':':':cs) = (code, cs, Normal)
        lex_entry_code code (c : cs) = lex_entry_code (DL.snoc code c) cs
        (code, rest, mode) = lex_entry_code DL.empty cs
    in  TokenCode ((BS.pack . DL.toList) code) : (if rest == "" then [] else TokenStart : TokenMode mode : lex_rules rest)


lex_rules :: String -> [Token]
lex_rules [] = []
lex_rules ('\n' : 'e' : 'n' : 'd' : '*' : '/' : cs) = TokenEnd : lexer cs
lex_rules (c : cs)
    | isSpace c = lex_rules cs
    | isAlpha c = lex_name (c : cs)
lex_rules ('>'  : cs) = TokenAngle        : lex_name cs
lex_rules ('='  : cs) = TokenEq           : lex_rules cs
lex_rules ('{'  : cs) = TokenOParenthesis : lex_code cs


lex_code :: String -> [Token]
lex_code cs =
    let lex_code' :: Int -> DL.DList Char -> String -> (DL.DList Char, String)
        lex_code' 0 tok rest         = (DL.cons '{' tok, rest)
        lex_code' i tok ""           = error "invalid code block in regexp definition"
        lex_code' i tok ('{' : rest) = lex_code' (i + 1) (DL.snoc tok '{') rest
        lex_code' i tok ('}' : rest) = lex_code' (i - 1) (DL.snoc tok '}') rest
        lex_code' i tok (c   : rest) = lex_code' i       (DL.snoc tok c)   rest
        (code, rest) = lex_code' 1 DL.empty cs
    in  TokenCode ((BS.pack . DL.toList) code) : TokenCParenthesis : lex_rules rest


lex_name :: String -> [Token]
lex_name cs =
    let (nm, rest) = span (\ c -> isAlphaNum c || c == '_') cs
        rest'      = lex_rules rest
    in  if nm /= "" then TokenName nm : rest' else rest'


--------------------------------------------------------------------------------
parse_source :: FilePath -> IO ChunkList
parse_source fp =
    let conds2list :: CondList -> [Cond]
        conds2list (OneCond c)      = [c]
        conds2list (ManyConds c cs) = c : conds2list cs

        rules2table :: Rules -> [(RegexpName, ([Cond], Code))]
        rules2table (OneRule   (SimpleRule           name code))       = [(name, ([], code))]
        rules2table (OneRule   (ComplexRule condlist name code))       = [(name, (conds2list condlist, code))]
        rules2table (ManyRules (SimpleRule           name code) rules) = (name, ([], code)) : rules2table rules
        rules2table (ManyRules (ComplexRule condlist name code) rules) = (name, (conds2list condlist, code)) : rules2table rules

        source2chunk_list :: Source -> ChunkList
        source2chunk_list (SourceEnd code)                = LastChunk code
        source2chunk_list (Source code mode rules source) = Chunk
            code
            mode
            ((foldl'
                (\ rules (name, (conds, code)) -> M.insertWith
                    (\ _ xs -> M.insertWith (\ _ code' -> BS.concat [BS.pack "{ ", code, code', BS.pack " }"]) (S.fromList conds) code xs)
                    name
                    (M.insert (S.fromList conds) code M.empty)
                    rules
                ) M.empty
            ) (rules2table rules))
            (source2chunk_list source)
    in  ( source2chunk_list
        . parser
        . lexer
        ) <$> readFile fp
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
