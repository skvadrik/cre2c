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
    let (cfa', ss') = cfa_add_regexp_alt (cfa, ss) r rt sign
        cfa''       = S.foldl (\ c s -> setFinal s sign c) cfa' ss'
    in  (cfa'', ss')

------------------------------------------------------------------------------------------------------------------
cfa_add_tie_regexp_cat :: (CFA, S.Set State) -> RegexpCat -> State -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_tie_regexp_cat (cfa, ss) r s rt sign = case r of
    CatFromIter riter -> cfa_add_tie_regexp_iter (cfa, ss) riter s rt sign
    Cat riter rcat    -> cfa_add_regexp_cat (cfa_add_regexp_iter (cfa, ss) riter rt sign) rcat rt sign

cfa_add_tie_regexp_iter :: (CFA, S.Set State) -> RegexpIter -> State -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_tie_regexp_iter (cfa, ss) r s rt sign = case r of
    IterFromPrim rprim -> cfa_add_tie_regexp_prim (cfa, ss) rprim s rt sign
    Iter rprim n       -> foldl'
        (\ (cfa', ss') _ ->
            let (cfa'', ss'') = cfa_add_regexp_prim (cfa', ss') rprim rt sign
            in  (cfa'', S.union ss' ss'')
        )
        (cfa, ss)
        [1 .. n]

cfa_add_tie_regexp_prim :: (CFA, S.Set State) -> RegexpPrim -> State -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_tie_regexp_prim (cfa, ss) r s rt sign = case r of
    Elementary str -> foldl' (\(cfa', ss') c -> cfa_add_tie_regexp_atom (cfa', ss') c s sign) (cfa, ss) str
    Name s         ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  cfa_add_regexp_alt (cfa, ss) ralt rt sign
    Wrapped ralt   -> cfa_add_regexp_alt (cfa, ss) ralt rt sign

cfa_add_tie_regexp_atom :: (CFA, S.Set State) -> Char -> State -> SignNum -> (CFA, S.Set State)
cfa_add_tie_regexp_atom (cfa, ss) c s sign = S.foldl
    (\ (cfa', ss') s' ->
        let (cfa'', s'') = addTransition cfa' (s', c, sign, s)
        in  (cfa'', S.insert s'' ss')
    ) (cfa, S.empty)
    ss
------------------------------------------------------------------------------------------------------------------

cfa_add_regexp_alt :: (CFA, S.Set State) -> RegexpAlt -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_alt (cfa, ss) r rt sign = case r of
    AltFromCat rcat -> cfa_add_regexp_cat (cfa, ss) rcat rt sign
    Alt rcat ralt   ->
        let (cfa', ss')   = cfa_add_regexp_cat (cfa, ss) rcat rt sign
            (cfa'', ss'') = bind_ralt (cfa', ss) ralt (lastState cfa) rt sign
        in  (cfa'', S.union ss' ss'')

bind_ralt :: (CFA, S.Set State) -> RegexpAlt -> State -> RegexpTable -> SignNum -> (CFA, S.Set State)
bind_ralt (cfa, ss) r s rt sign = case r of
    AltFromCat rcat -> cfa_add_tie_regexp_cat (cfa, ss) rcat s rt sign
    Alt rcat ralt   ->
        let (cfa', ss')   = cfa_add_tie_regexp_cat (cfa, ss) rcat s rt sign
            (cfa'', ss'') = bind_ralt (cfa', ss) ralt s rt sign
        in  (cfa'', S.union ss' ss'')

-- есть смысл в том, чтобы мержить те состояния, в которые приводят альтернативы, взаимно не являющиесяя префиксами друг друга.
-- смысл однако чисто теоретический, не знаю, насколько полезным это будет на практике
-- число состояний уменьшится, но зато время/память генерации сканера возрастут.


-- можно забабахать функцию типа changeState, но шоб она проверяла, если из изменяемого состояния
-- есть переходы, то это состояние добавить в список


cfa_add_regexp_cat :: (CFA, S.Set State) -> RegexpCat -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_cat (cfa, ss) r rt sign = case r of
    CatFromIter riter -> cfa_add_regexp_iter (cfa, ss) riter rt sign
    Cat riter rcat    -> cfa_add_regexp_cat (cfa_add_regexp_iter (cfa, ss) riter rt sign) rcat rt sign

cfa_add_regexp_iter :: (CFA, S.Set State) -> RegexpIter -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_iter (cfa, ss) r rt sign = case r of
    IterFromPrim rprim -> cfa_add_regexp_prim (cfa, ss) rprim rt sign
    Iter rprim n       -> foldl'
        (\ (cfa', ss') _ ->
            let (cfa'', ss'') = cfa_add_regexp_prim (cfa', ss') rprim rt sign
            in  (cfa'', S.union ss' ss'')
        )
        (cfa, ss)
        [1 .. n]

cfa_add_regexp_prim :: (CFA, S.Set State) -> RegexpPrim -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_prim (cfa, ss) r rt sign = case r of
    Elementary s -> foldl' (\(d, s) c -> cfa_add_regexp_atom (d, s) c sign) (cfa, ss) s
    Name s       ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  cfa_add_regexp_alt (cfa, ss) ralt rt sign
    Wrapped ralt -> cfa_add_regexp_alt (cfa, ss) ralt rt sign

cfa_add_regexp_atom :: (CFA, S.Set State) -> Char -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_atom (cfa, ss) c sign =
    let sl  = lastState cfa
    in  S.foldl
            (\ (cfa', ss') s ->
                let (cfa'', s') = addTransition cfa' (s, c, sign, sl)
                in  (cfa'', S.insert s' ss')
            ) (cfa, S.empty)
            ss




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




code_for_initial_state :: CFA -> M.HashMap SignNum [Condition] -> String
code_for_initial_state cfa sign2conds = concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (l, (ks, s)) -> concat
        [ "case "
        , show l
        , ":"
        , code_for_conditions $ M.filterWithKey (\ k _ -> S.member k ks) sign2conds
        , "\n\t\tif (n_matching_signatures > 0) {"
        , "\n\t\t\tgoto m_"
        , show s
        , ";\n\t\t} else {\n\t\t\tMARKER = CURSOR;\n\t\t\tgoto m_fin;\n\t\t}\n\t"
        ]) (M.toList (neighbourhood (initialState cfa) cfa))
    , "default:"
    , "\n\t\tMARKER = CURSOR;"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]


code_for_conditions :: M.HashMap SignNum [Condition] -> String
code_for_conditions = M.foldlWithKey' (\code k conditions -> code ++ case conditions of
    [] -> ""
    _  -> concat
        [ "\n\t\tis_active = "
        , intercalate " && " conditions
        , ";\n\t\tactive_signatures["
        , show k
        , "] = is_active;"
        , "\n\t\tn_matching_signatures += is_active;"
        ]
    ) ""



trace' :: (Show a) => a -> a
trace' a = trace (show a) a


trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a




-- нужно следить за отпадающимим сигнатуоами
-- возможно, небольшой список будет эффективнее
code_for_state :: CFA -> State -> BS.ByteString
code_for_state cfa s = (BS.pack . concat)
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
    , "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (l, (_, s')) -> concat
        [ "case "
        , show l
        , ":\n\t\tgoto m_"
        , show s'
        , ";\n\t"
        ]) (M.toList (neighbourhood s cfa))
    , "default:"
--    , if isFinal s cfa then "" else "\n\t\tMARKER = CURSOR;"
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
            , "\nbool active_signatures[NUM_SIGN];"
            , "\nint accepted_signatures[NUM_SIGN + 1];"
            , "\nint accepted_count = 0;"
            , "\nint n_matching_signatures = 0;"
            , "\nint i = 0;"
            , "\nbool is_active;"
            , "\nbool adjust_marker = true;"
            , "\nstatic void * code[] = {"
            , intercalate ", " (map ("&&" ++) code_labels)
            , "};"
            , "\nfor (i = 0; i < NUM_SIGN; i++) {"
            , "\n\tactive_signatures[i] = true;"
            , "\n\taccepted_signatures[i] = NUM_SIGN;"
            , "\n}"
            , "\naccepted_signatures[NUM_SIGN] = NUM_SIGN;"
            , "\ngoto m_start;\n"
            , concatMap (\ k -> concat
                [ "\n"
                , code_labels !! k
                , ": "
                , (map BS.unpack codes) !! k
                , "\ngoto m_continue;"
                ]) [0 .. length codes - 1]
            , "\n\nm_fin:"
            , "\nCURSOR = MARKER;"
            , "\nadjust_marker = true;"
            , "\ni = 0;"
            , "\nm_continue:"
            , "\nif (accepted_signatures[i] != NUM_SIGN)"
            , "\n\tGOTO(accepted_signatures[i++]);"
            , "\ngoto m_start;"
            , "\n\nm_start:"
            , "\nfor (i = accepted_count - 1; i >= 0; i--)"
            , "\n\taccepted_signatures[i] = NUM_SIGN;"
            , "\naccepted_count = 0;"
            , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();"
            , "\nn_matching_signatures = 0;"
            , code_for_initial_state cfa signs2conds
            ]

    let code_states = foldl'
            (\ code s -> BS.concat [code, code_for_state cfa s])
            (BS.pack "")
            ((filter (/= (initialState cfa)) . states) cfa)

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
