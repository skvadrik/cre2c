#!/usr/bin/env runghc


import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           Data.Functor               ((<$>))
import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')
import           DFA
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




ncfa_add_lambda_chain :: NCFA -> State -> State -> SignNum -> NCFA
ncfa_add_lambda_chain ncfa st_start st_end sign =
    case addTransition ncfa (st_start, Nothing, sign, st_end) of
        (ncfa', st') | st' == st_end -> ncfa'
        (ncfa', st')                 -> ncfa_add_lambda_chain ncfa' st' st_end sign

ncfa_tie :: NCFA -> [State] -> SignNum -> (NCFA, State)
ncfa_tie ncfa states sign =
    let st_end = lastState ncfa
    in  (foldl' (\ d s -> ncfa_add_lambda_chain d s st_end sign) ncfa states, st_end)

ncfa_add_regexp :: (NCFA, State) -> Regexp -> RegexpTable -> SignNum -> (NCFA, State)
ncfa_add_regexp (ncfa, st_start) (Regexp r) rt sign =
    let (ncfa', st') = ncfa_add_regexp_alt (ncfa, st_start) r rt sign
        ncfa''       = setFinal ncfa' st' sign
    in  (ncfa'', lastState ncfa'')

ncfa_add_regexp_alt :: (NCFA, State) -> RegexpAlt -> RegexpTable -> SignNum -> (NCFA, State)
ncfa_add_regexp_alt (ncfa, st_start) r rt sign = case r of
    AltFromCat rcat -> ncfa_add_regexp_cat (ncfa, st_start) rcat rt sign
    Alt rcat ralt   ->
        let (ncfa', st)   = ncfa_add_regexp_cat (ncfa, st_start) rcat rt sign
            (ncfa'', st') = ncfa_add_regexp_alt (ncfa', st_start) ralt rt sign
            states        = st : [st']
        in  ncfa_tie ncfa'' states sign

ncfa_add_regexp_cat :: (NCFA, State) -> RegexpCat -> RegexpTable -> SignNum -> (NCFA, State)
ncfa_add_regexp_cat (ncfa, st_start) r rt sign = case r of
    CatFromIter riter -> ncfa_add_regexp_iter (ncfa, st_start) riter rt sign
    Cat riter rcat    -> ncfa_add_regexp_cat (ncfa_add_regexp_iter (ncfa, st_start) riter rt sign) rcat rt sign

ncfa_add_regexp_iter :: (NCFA, State) -> RegexpIter -> RegexpTable -> SignNum -> (NCFA, State)
ncfa_add_regexp_iter (ncfa, st_start) r rt sign = case r of
    IterFromPrim rprim -> ncfa_add_regexp_prim (ncfa, st_start) rprim rt sign
    Iter rprim n       ->
        let build_rcat :: RegexpPrim -> Int -> RegexpCat
            build_rcat rp m = foldl' (\rc _ -> Cat (IterFromPrim rp) rc) ((CatFromIter . IterFromPrim) rprim) [1 .. m - 1]

            ralt            = foldl' (\r k -> Alt (build_rcat rprim (n - k)) r) (AltFromCat (build_rcat rprim n)) [1 .. n - 1]
            (ncfa', st_end) = ncfa_add_regexp_alt (ncfa, st_start) ralt rt sign

        in  (ncfa_add_lambda_chain ncfa' st_start st_end sign, st_end)

ncfa_add_regexp_prim :: (NCFA, State) -> RegexpPrim -> RegexpTable -> SignNum -> (NCFA, State)
ncfa_add_regexp_prim (ncfa, st_start) r rt sign = case r of
    Elementary s -> foldl' (\(d, s) c -> ncfa_add_regexp_atom (d, s) c sign) (ncfa, st_start) s
    Name s       ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  ncfa_add_regexp_alt (ncfa, st_start) ralt rt sign
    Wrapped ralt -> ncfa_add_regexp_alt (ncfa, st_start) ralt rt sign

ncfa_add_regexp_atom :: (NCFA, State) -> Char -> SignNum -> (NCFA, State)
ncfa_add_regexp_atom (ncfa, st_start) c sign =
    addTransition ncfa (st_start, Just c, sign, lastState ncfa)




{-
code_for_initial_state :: DCFA -> M.HashMap SignNum [Condition] -> String
code_for_initial_state dcfa sign2conds =
    let (node0, merged_states) = neighbourhood dcfa (initialStateDCFA dcfa)
    in  concat
        [ ""
-}
{-
        , case M.lookup Nothing node0 of
            Just (signs, st) -> concat
                [ code_for_conditions $ M.filterWithKey (\ sign _ -> S.member sign signs) sign2conds
                , code_for_accepting_transition signs
                ]
            Nothing          -> ""
-}
{-
        , "\nswitch (*CURSOR++) {\n\t"
        , concatMap (\ (l, (sign_nums, st)) -> concat
            [ "case "
            , show $ fromJust l
            , ":"
            , code_for_conditions $ M.filterWithKey (\ sign _ -> S.member sign sign_nums) sign2conds
            , "\n\t\tif (n_matching_signatures > 0) {"
-}
-- Вот тута проблема
-- что если из начального состояния по лямбда-дугам достижимо финальное (ну то есть с 0 или 1 не-лямбда-дугой на пути)
--        , if isFinal st dfa then code_for_accepting_transition sign_nums else ""
{-
            , "\n\t\t\tgoto m_"
            , show st
            , ";\n\t\t} else {\n\t\t\tMARKER = CURSOR;\n\t\t\tgoto m_fin;\n\t\t}\n\t"
            ]) $ trace' $ filter (\ (l, _) -> isJust l) $ M.toList node0
        , "default:"
        , "\n\t\tMARKER = CURSOR;"
        , "\n\t\tgoto m_fin;"
        , "\n\t}"
        ]


code_for_conditions :: M.HashMap SignNum [Condition] -> String
code_for_conditions = M.foldlWithKey' (\code sign conditions -> code ++ case conditions of
    [] -> ""
    _  -> concat
        [ "\n\t\tis_active = "
        , intercalate " && " conditions
        , ";\n\t\tactive_signatures["
        , show sign
        , "] = is_active;"
        , "\n\t\tn_matching_signatures += is_active;"
        ]
    ) ""


code_for_accepting_transition :: SignSet -> String
code_for_accepting_transition signs =
    S.foldl' (\s k -> s ++ concat
        [ "\nif (active_signatures["
        , show k
        , "] == true) {"
        , "\n\taccepted_signatures[accepted_count++] = "
        , show k
        , ";\n\tactive_signatures["
        , show k
        , "] = false;\n}"
        ]) "" signs


code_for_non_accepting_transition :: [(Label, (SignSet, State))] -> Int -> Bool -> String
code_for_non_accepting_transition nodes num_sign is_accepting = concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\(l, (_, s)) -> concat
        [ "case "
        , case l of
            Just c  -> show c
            Nothing -> undefined
        , ":"
        , "\n\t\tgoto m_"
        , show s
        , ";\n\t"
        ]) nodes
    , "default:"
    , if is_accepting then "" else "\n\t\tif (adjust_marker)\n\t\t\tMARKER = CURSOR;"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]


trace' :: (Show a) => a -> a
trace' a = trace (show a) a


trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a


code_for_state :: DFA -> State -> Int -> StateMap -> (BS.ByteString, StateMap)
code_for_state dfa st num_sign merged_states =
    let (neighbours, merged_states') = neighbourhood dfa st
        all_neighbours = case M.lookup st merged_states of

            True  -> M.foldlWithKey'
                ( \ nbrs l' (signs', s') -> if S.intersection signs signs' /= S.empty
                    then case M.lookup l' node of
                        Just (signs'', s'') ->
                            ( M.adjust (\ (signs_old, s_old) -> (S.union signs_old signs', s_old)) l' node
                            , M.insertWith
                                (\ _ ss -> S.insert s' ss)
                                s''
                                (S.insert s' S.empty)
                                sts
                            )
                        Nothing             -> (M.insert l' (signs', s') node, sts)
                    else (node, sts)
                )
                neighbours
                states
            Just states -> merge_neighbours $ S.foldl
                (\ nbrs st' ->
                    let nbrs' = fst $ neighbourhood dfa st'
                    in  
                )
                neighbours
                states
            Nothing     -> neighbours
        (accepting, non_accepting)   = partition (\ (l, (_, st')) -> isNothing l && isFinal st' dfa) $ M.toList neighbours
        is_final                     = isFinal st dfa
        is_lambda_final              = accepting /= []
        code = BS.pack $ concat
            [ "\n\nm_"
            , show st
            , ":"
            , "\nprintf(\"c: %s\\n\", CURSOR);"
            , if is_final then code_for_accepting_transition (acceptedSignatures dfa st) else ""
            , if is_lambda_final then concatMap (\ (l, (_, st')) -> code_for_accepting_transition (acceptedSignatures dfa st')) accepting else ""
            , if is_final || is_lambda_final then "\nMARKER = CURSOR;\nadjust_marker = false;" else ""
            , case non_accepting of
                []    -> "\ngoto m_fin;"
                nodes -> code_for_non_accepting_transition nodes num_sign (is_final || is_lambda_final)
            ]
        merged_states'' = M.foldlWithKey'
            ( \ all_states st' states -> M.insertWith
                (\ new_states old_states -> S.union new_states old_states)
                st'
                states
                all_states
            )
            merged_states
            merged_states'
    in  (code, merged_states'')
-}



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




main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fsign) <- case args of
        [src, dest]       -> return (src, dest, Nothing)
        [src, dest, sign] -> return (src, dest, Just sign)
        _                 -> usage >> undefined

--    gen_test_source fsrc 3 10

    (code, rules, rest) <- parse_source fsrc
    regexp_table <- case fsign of
        Just fs -> parse_signatures fs
        Nothing -> return M.empty

    let (conditions, regexps, codes) = (unzip3 . M.elems) rules
    let indexes = M.keys rules
    let num_sign = length indexes
    let signs2conds = M.fromList $ zip indexes conditions

    let ncfa' = emptyNCFA
    let ncfa  = fst $ M.foldlWithKey'
            (\ (d, s) k r -> (fst (ncfa_add_regexp (d, s) r regexp_table k), s))
            (ncfa', initialStateNCFA ncfa')
            (M.fromList (zip indexes regexps))
    toDotNCFA ncfa "./ncfa.dot"

    let dcfa = determine_ncfa ncfa
    toDotDCFA dcfa "./dcfa.dot"

--    print ncfa
    print dcfa

    let sign_table = M.map (\r -> gen_signatures_from_regexp r (M.empty)) regexp_table
    let all_signatures = M.fromList $ zip [1 .. num_sign] $ map (\r -> gen_signatures_from_regexp r sign_table) regexps
--    let all_signatures' = M.foldlWithKey' (\signatures sign_num sign_list -> map () sign_list) all_signatures
    let sign_maxlen = maximum $ map BS.length $ concat $ M.elems all_signatures
    print all_signatures
    print $ M.size all_signatures
    print sign_maxlen

    let (test_string, stats) = gen_test_string_with_stats all_signatures
    print (test_string, stats)

{-
    let code_labels = map (\k -> "code" ++ show k) [0 .. length codes - 1]
    let main_code = BS.pack $ concat
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
            , code_for_initial_state dfa signs2conds
            ]
-}

    let (NCFA _ _ dg _) = ncfa
    print $ M.size dg
    let n_st = M.foldl' (\n (in_node, out_node) -> n + M.size in_node + M.size out_node) 0 dg
    print n_st


{-
    let states_to_gen_code = codegen_states dfa
    let states_code = fst $ foldl'
            (\ (str, merged_states) st -> let (code, states) = code_for_state dfa st num_sign merged_states in (BS.concat [str, code], states))
            (BS.pack "", M.empty)
            states_to_gen_code
    let end_code = BS.concat
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
        [ code
        , main_code
        , states_code
        , end_code
        ]
-}

--    print $ initialNode dfa
--    print dfa
