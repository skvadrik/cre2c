#!/usr/bin/env runghc


import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           Data.Functor               ((<$>))
import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')
import           CFA
import           Control.Monad              (forM_, forM)
import           RegexpParser
import           Data.List                  (intercalate, partition)
import           Data.Maybe                 (isNothing, fromJust, isJust)
import           Control.Arrow              (second)
import           RegexpGenerator
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList            as DL

import Debug.Trace


usage :: IO ()
usage = putStrLn "usage: ./scan.hs <source file> <destiination file>" >> exitFailure


type Code        = BS.ByteString
type Condition   = String
type Rule        = ([Condition], Regexp, Code)
type RegexpTable = M.HashMap String Regexp
type SignTable   = M.HashMap String [BS.ByteString]


parse_source :: FilePath -> IO (Code, M.HashMap SignNum Rule, Code)
parse_source fp = do
    (code, comment, rest) <-
          (\(code', rest') ->
              let (comment, rest'') = break (== BS.pack "end*/") (tail rest')
              in  (BS.intercalate (BS.pack "\n") code', comment, BS.intercalate (BS.pack "\n") (tail rest'')))
        . break (== BS.pack "/*start:")
        . BS.lines
        <$> BS.readFile fp
    let split_line :: BS.ByteString -> ([Condition], BS.ByteString, Code)
        split_line l =
            let (s1, s2) = (BS.break (== '>') . BS.reverse) l
                (s3, s4) = (BS.break (== '=') . BS.reverse) s1
                conditions  = (words . reverse . dropWhile (== '>')) (BS.unpack s2)
                signature   = BS.dropWhile (`elem` "> \t") s3
                code        = BS.dropWhile (/= '{') s4
            in  (conditions, signature, code)
    let rules =
            ( M.fromList
            . zip [0 .. length comment - 1]
            . map
                ( (\(conds, sign, code) -> (conds, parse_regexp (BS.concat [BS.pack "(", sign, BS.pack ")"]), code))
                . split_line
                )
            ) comment
    return (code, rules, rest)


gen_test_source :: FilePath -> Int -> Int -> IO ()
gen_test_source fp regexp_count regexp_length = do
    regexps <- forM [1 .. regexp_count] (\_ -> new_regexp regexp_length)
    let code = BS.pack $ concat
            [ "\n#include <stdio.h>"
            , "\n#include <string.h>"
            , "\n"
            , "\nvoid scan (const char * buffer, int size)"
            , "\n{"
            , "\n    char * cursor = (char *) buffer;"
            , "\n    char * marker = (char *) buffer;"
            , "\n    char * limit  = (char *) buffer + size;"
            , "\n"
            , "\n#define CURSOR      cursor"
            , "\n#define MARKER      marker"
            , "\n#define LIMIT       limit"
            , "\n#define FILL()      { return; }"
            , "\n"
            , concatMap (\k -> concat
                [ "\n#define c"
                , show k
                , " 1"
                ]) [0 .. regexp_count - 1]
            , "\n"
            , "\n/*start:"
            , concatMap (\k -> concat
                [ "\n    c"
                , show k
                , " > "
                , regexps !! k
                , " = { printf(\"%d\\n\", "
                , show k
                , "); }"
                ]) [0 .. regexp_count - 1]
            , "\nend*/"
            , "\n"
            , "\n}"
            ]
    BS.writeFile fp code


parse_signatures :: FilePath -> IO RegexpTable
parse_signatures fp =
    M.fromList
    . map
        ( second (parse_regexp . BS.pack . tail)
        . break (== '=')
        . filter (`notElem` " \t")
        )
    . lines
    <$> readFile fp




cfa_add_regexp :: (CFA, S.Set State) -> Regexp -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp (cfa, ss) (Regexp r) rt sign =
    let (cfa', ss') = bind_ralt (cfa, ss) r Nothing rt sign
        cfa''       = S.foldl (\ c s -> setFinal s sign c) cfa' ss'
    in  (cfa'', ss')

bind_ralt :: (CFA, S.Set State) -> RegexpAlt -> Maybe State -> RegexpTable -> SignNum -> (CFA, S.Set State)
bind_ralt (cfa, ss) r s rt sign = case r of
    AltFromCat rcat -> bind_rcat (cfa, ss) rcat s rt sign
    Alt rcat ralt   ->
        let (cfa', ss')   = bind_rcat (cfa, ss) rcat s rt sign
            sl            = if isJust s then s else bindableState cfa'
            (cfa'', ss'') = bind_ralt (cfa', ss) ralt sl rt sign
        in  (cfa'', S.union ss' ss'')

bind_rcat :: (CFA, S.Set State) -> RegexpCat -> Maybe State -> RegexpTable -> SignNum -> (CFA, S.Set State)
bind_rcat (cfa, ss) r s rt sign = case r of
    CatFromIter riter -> bind_riter (cfa, ss) riter s rt sign
    Cat riter rcat    -> bind_rcat (bind_riter (cfa, ss) riter Nothing rt sign) rcat s rt sign

bind_riter :: (CFA, S.Set State) -> RegexpIter -> Maybe State -> RegexpTable -> SignNum -> (CFA, S.Set State)
bind_riter (cfa, ss) r s rt sign = case r of
    IterFromPrim rprim -> bind_rprim (cfa, ss) rprim s rt sign
    Iter rprim n       ->
        let (cfa', ss') = foldl'
                (\ (cfa', ss') _ ->
                    let (cfa'', ss'') = bind_rprim (cfa', ss') rprim Nothing rt sign
                    in  (cfa'', S.union ss' ss'')
                ) (cfa, ss) [1 .. n - 1]
            (cfa'', ss'') = bind_rprim (cfa', ss') rprim s rt sign
        in  (cfa'', S.union ss' ss'')

bind_rprim :: (CFA, S.Set State) -> RegexpPrim -> Maybe State -> RegexpTable -> SignNum -> (CFA, S.Set State)
bind_rprim (cfa, ss) r s rt sign = case r of
    Elementary str ->
        let (cfa'', ss'') = foldl' (\ (cfa', ss') c -> bind_ratom (cfa', ss') c Nothing sign) (cfa, ss) (init str)
        in  bind_ratom (cfa'', ss'') (last str) s sign
    Name str       ->
        let Regexp ralt = M.lookupDefault undefined str rt
        in  bind_ralt (cfa, ss) ralt s rt sign
    Wrapped ralt   -> bind_ralt (cfa, ss) ralt s rt sign

bind_ratom :: (CFA, S.Set State) -> Char -> Maybe State -> SignNum -> (CFA, S.Set State)
bind_ratom (cfa, ss) c s sign =
    let (s', cfa') = case s of
            Just s' -> (s', cfa)
            Nothing -> let s'' = maxStateNumber cfa in (s'', setBindable (Just s'') cfa)
    in  S.foldl
            (\ (cfa'', ss') s'' ->
                let (cfa''', s''') = addTransition cfa'' (s'', c, sign, s')
                in  (cfa''', S.insert s''' ss')
            ) (cfa', S.empty) ss




gen_signatures_from_regexp :: Regexp -> SignTable -> [BS.ByteString]
gen_signatures_from_regexp (Regexp r) = sign_from_ralt r []

sign_from_ralt :: RegexpAlt -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_ralt r ss stbl = case r of
    AltFromCat rcat -> sign_from_rcat rcat ss stbl
    Alt rcat ralt   -> sign_from_rcat rcat ss stbl ++ sign_from_ralt ralt ss stbl

sign_from_rcat :: RegexpCat -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_rcat r ss stbl = case r of
    CatFromIter riter -> sign_from_riter riter ss stbl
    Cat riter rcat    -> concatMap (\s -> map (\s' -> BS.concat [s, s']) (sign_from_rcat rcat ss stbl)) (sign_from_riter riter ss stbl)

sign_from_riter :: RegexpIter -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_riter r ss stbl = case r of
    IterFromPrim rprim -> sign_from_rprim rprim ss stbl
    Iter rprim n       -> let ss' = sign_from_rprim rprim ss stbl in concatMap (\s' -> map (\k -> (BS.concat . replicate k) s') [0 .. n]) ss'

sign_from_rprim :: RegexpPrim -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_rprim r ss stbl = case r of
    Elementary s' -> let s''' = BS.pack s' in case ss of
        [] -> [s''']
        _  -> map (\s'' -> BS.concat [s''', s'']) ss
    Name name     -> concatMap (\s -> map (\s'' -> BS.concat [s, s'']) (M.lookupDefault undefined name stbl)) ss
    Wrapped ralt  -> sign_from_ralt ralt ss stbl

gen_test_string_with_stats :: M.HashMap SignNum [BS.ByteString] -> (BS.ByteString, [SignNum])
gen_test_string_with_stats signatures =
    ((BS.concat . map BS.concat . M.elems) signatures, concatMap (\(sign_num, sign_list) -> replicate (length sign_list) (sign_num - 1)) (M.toList signatures))




code_for_initial_state :: CFANode -> M.HashMap SignNum [Condition] -> String
code_for_initial_state node0 sign2conds = concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (l, (ks, s)) -> concat
        [ "case "
        , show l
        , ":"
        , code_for_conditions $ M.filterWithKey (\ k _ -> S.member k ks) sign2conds
        , "\n\t\tif (active_count > 0) {"
        , "\n\t\t\tgoto m_"
        , show s
        , ";\n\t\t} else {\n\t\t\tMARKER = CURSOR;\n\t\t\tgoto m_fin;\n\t\t}\n\t"
        ]) (M.toList node0)
    , "default:"
    , "\n\t\tMARKER = CURSOR;"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]


code_for_conditions :: M.HashMap SignNum [Condition] -> String
code_for_conditions = M.foldlWithKey' (\code k conditions -> code ++ case conditions of
    [] -> ""
    _  -> let conds = intercalate " && " conditions in concat
        [ "\n\t\tactive_signatures["
        , show k
        , "] = "
        , conds
        , ";\n\t\tactive_count += "
        , conds
        , ";"
        ]
    ) ""




trace' :: (Show a) => a -> a
trace' a = trace (show a) a

trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a




code_for_final_state :: State -> SignSet -> BS.ByteString
code_for_final_state s signs = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , S.foldl' (\ s k -> s ++ concat
        [ "\nif (active_signatures["
        , show k
        , "] == true) {"
        , "\n\taccepted_signatures[accepted_count++] = "
        , show k
        , ";\n\tactive_signatures["
        , show k
        , "] = false;"
        , "\nMARKER = CURSOR;"
        , "\nadjust_marker = false;\n}"
        ]) "" signs
    , "\nprintf(\"%s\\n\", CURSOR);"
    , "\ngoto m_fin;"
    ]


-- нужно следить за отпадающимим сигнатуоами
-- возможно, небольшой список будет эффективнее
code_for_state :: CFA -> State -> CFANode -> BS.ByteString
code_for_state cfa s node = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , if isFinal s cfa
        then S.foldl' (\ s k -> s ++ concat
            [ "\nif (active_signatures["
            , show k
            , "] == true) {"
            , "\n\taccepted_signatures[accepted_count++] = "
            , show k
            , ";\n\tactive_signatures["
            , show k
            , "] = false;"
            , "\nMARKER = CURSOR;"
            , "\nadjust_marker = false;\n}"
            ]) "" (acceptedSignatures s cfa)
        else ""
    , "\nprintf(\"%s\\n\", CURSOR);"
    , "\nswitch (*CURSOR++) {"
    , concatMap (\ (l, (_, s')) -> concat
        [ "\n\tcase "
        , show l
        , ":"
        , "\n\t\tif (active_count > 0)"
        , "\n\t\t\tgoto m_"
        , show s'
        , ";\n\t\telse {"
        , "\n\t\t\tif (adjust_marker)"
        , "\n\t\t\t\tMARKER = CURSOR;"
        , "\n\t\t\t\tgoto m_fin;"
        , "\n\t\t\t}"
        ]) (M.toList node)
    , "\n\tdefault:"
    , if isFinal s cfa then "" else "\n\t\tif (adjust_marker) \n\t\t\tMARKER = CURSOR;"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]




main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fsign) <- case args of
        [src, dest]       -> return (src, dest, Nothing)
        [src, dest, sign] -> return (src, dest, Just sign)
        _                 -> usage >> undefined

--    gen_test_source fsrc 3 10

    (code_prolog, rules, rest) <- parse_source fsrc
    regexp_table <- case fsign of
        Just fs -> parse_signatures fs
        Nothing -> return M.empty

    let (conditions, regexps, codes) = (unzip3 . M.elems) rules
    let indexes                      = M.keys rules
    let num_sign                     = length indexes
    let signs2conds                  = M.fromList $ zip indexes conditions
    let code_labels                  = map (\k -> "code" ++ show k) [0 .. length codes - 1]

    let cfa' = emptyCFA
    let cfa  = fst $ M.foldlWithKey'
            (\ (c, ss) k r -> (fst (cfa_add_regexp (c, ss) r regexp_table k), ss))
            (cfa', S.insert (initialState cfa') S.empty)
            (M.fromList (zip indexes regexps))

    toDot cfa "./cfa.dot"
    print cfa
    print regexps

    let sign_table           = M.map (\r -> gen_signatures_from_regexp r (M.empty)) regexp_table
    let all_signatures       = M.fromList $ zip [1 .. num_sign] $ map (\r -> gen_signatures_from_regexp r sign_table) regexps
    let sign_maxlen          = maximum $ map BS.length $ concat $ M.elems all_signatures
    let (test_string, stats) = gen_test_string_with_stats all_signatures

    print all_signatures
    print $ M.size all_signatures
    print sign_maxlen
    print (test_string, stats)

    let code_init = BS.pack $ concat
            [ "\n#define NUM_SIGN "
            , show num_sign
            , "\n#define SIGN_MAXLEN "
            , show sign_maxlen
            , "\n#define GOTO(x) { goto *code[x]; }"
            , "\n\nbool active_signatures[NUM_SIGN];"
            , "\nint  active_count   = 0;"
            , "\n\nint  accepted_signatures[NUM_SIGN + 1];"
            , "\nint  accepted_count = 0;"
            , "\nfor (int i = 0; i <= NUM_SIGN; i++) {"
            , "\n\taccepted_signatures[i] = NUM_SIGN;"
            , "\n}"
            , "\n\nint  j;"
            , "\nbool adjust_marker;"
            , "\n\nstatic void * code[] = {"
            , intercalate ", " (map ("&&" ++) code_labels)
            , "};"
            , "\n\ngoto m_start;\n\n"
            , concatMap (\ k -> concat
                [ "\n"
                , code_labels !! k
                , ": "
                , (map BS.unpack codes) !! k
                , "\ngoto m_continue;"
                ]) [0 .. length codes - 1]
            , "\n\n\nm_fin:"
            , "\nCURSOR = MARKER;"
            , "\nj = 0;"
            , "\nm_continue:"
            , "\nif (accepted_signatures[j] != NUM_SIGN)"
            , "\n\tGOTO(accepted_signatures[j++]);"
            , "\ngoto m_start;"
            , "\n\n\nm_start:"
            , "\nfor (int i = accepted_count - 1; i >= 0; i--)"
            , "\n\taccepted_signatures[i] = NUM_SIGN;"
            , "\nfor (int i = 0; i < NUM_SIGN; i++)"
            , "\n\tactive_signatures[i] = false;"
            , "\nactive_count   = 0;"
            , "\naccepted_count = 0;"
            , "\nadjust_marker  = true;"
            , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
            , code_for_initial_state (initialNode cfa) signs2conds
            ]

    let code_states = M.foldlWithKey'
            (\ code s node -> BS.concat [code, code_for_state cfa s node])
            (BS.pack "")
            (cfaGraph cfa)

    let code_epilog = BS.concat
            [ (BS.pack . concat)
                [ "}\n"
                , "\nint main ()"
                , "\n{"
                , "\n    const char * buffer = \""
                ]
            , test_string
            , (BS.pack . concat)
                [ replicate (sign_maxlen + 0) '*'
                , "\";\n    scan(buffer, strlen(buffer));"
                , "\n    return 0;"
                , "\n}"
                , "\n"
                ]
            ]

    BS.writeFile fdest $ BS.concat
        [ code_prolog
        , code_init
        , code_states
        , code_epilog
        ]
