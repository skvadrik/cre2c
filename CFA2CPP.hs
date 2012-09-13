module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                   (intercalate)
import qualified Data.ByteString.Char8 as BS
import           Numeric                     (showHex)
import           Data.Char                   (ord)
--import qualified Data.DList            as DL

import           Types
import           CFA


cfa2cpp :: FilePath -> CFA -> Code -> Code -> [[Cond]] -> [Code] -> Int -> IO ()
cfa2cpp fp cfa prolog epilog conditions codes sign_maxlen =
    let entry  = code_for_entry (initialNode cfa) conditions codes sign_maxlen
        states = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , code_for_state s node (isFinal s cfa) (acceptedSignatures s cfa)
                ]
            )
            (BS.pack "")
            (cfaGraph cfa)
    in  BS.writeFile fp $ BS.concat
            [ prolog
            , entry
            , states
            , epilog
            ]


code_for_entry :: CFANode -> [[Cond]] -> [Code] -> Int -> Code
code_for_entry node0 conditions codes sign_maxlen =
    let n           = length codes
        sign2conds  = M.fromList $ zip [0 .. n - 1] conditions
        code_labels = map (\k -> "code" ++ show k) [0 .. n - 1]
    in  BS.pack $ concat
            [ "\n#define NUM_SIGN "
            , show n
            , "\n#define SIGN_MAXLEN "
            , show sign_maxlen
            , "\n#define GOTO(x) { goto *code[x]; }"
            , "\n\nbool active_signatures[NUM_SIGN];"
            , "\nint  active_count;"
            , "\n\nint accepted_signatures[NUM_SIGN];"
            , "\nint accepted_count = 0;"
            , "\n\nint  j;"
            , "\nbool adjust_marker;"
            , "\nbool default_case_enabled = true;"
            , "\nbool parsing_default;"
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
                ]) [0 .. n - 1]
            , "\n\n\nm_fin:"
            , "\nCURSOR = MARKER;"
            , "\nj = 0;"
            , "\nm_continue:"
            , "\nif (j < accepted_count)"
            , "\n\tGOTO(accepted_signatures[j++]);"
            , "\nif (!default_case_enabled && !parsing_default)"
            , "\n\tdefault_case_enabled = true;"
            , "\ngoto m_start;"
            , "\n\n\nm_start:"
            , "\nfor (int i = 0; i < NUM_SIGN; i++)"
            , "\n\tactive_signatures[i] = false;"
            , "\nactive_count    = 0;"
            , "\naccepted_count  = 0;"
            , "\nadjust_marker   = true;"
            , "\nparsing_default = false;"
            , "\ntoken           = CURSOR;"
            , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
            , code_for_initial_state node0 sign2conds
            ]


code_for_initial_state :: CFANode -> M.HashMap SignNum [Cond] -> String
code_for_initial_state node0 sign2conds = concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (l, (ks, s')) ->
        let code_for_cond_case :: Char -> String
            code_for_cond_case c = concat
                [ "\n\tcase "
                , "0x" ++ showHex (ord c) ""
--                , let i = ord c in "\'" ++ (if i < 0x20 || i == 0x5C || i == 0x27 || i > 0x7E then "\\x" ++ showHex i "" else [c])
--                , "':"
                , ":"
                , code_for_conditions $ M.filterWithKey (\ k _ -> S.member k ks) sign2conds
                , "\n\t\tif (active_count > 0)"
                , "\n\t\t\tgoto m_"
                , show s'
                , ";"
                ]
        in  case l of
            LabelAny     -> ""
            LabelChar c  -> code_for_cond_case c
            LabelRange s -> concatMap code_for_cond_case s
        ) (M.toList node0)
    , "\n\tdefault:"
    , case M.lookup LabelAny node0 of
        Just (_, s') -> concat
            [ "\n\t\tif (default_case_enabled) {"
            , "\n\t\t\tparsing_default = true;"
            , "\n\t\t\tgoto m_"
            , show s'
            ,  ";"
            , "\n\t\t} else if (adjust_marker) {"
            , "\n\t\t\tMARKER = CURSOR;"
            , "\n\t\t\tgoto m_fin;"
            , "\n\t\t}"
            ]
        Nothing      -> concat
            [ "\n\t\tMARKER = CURSOR;"
            , "\n\t\tgoto m_fin;"
            ]
    , "\n\t}"
    ]


code_for_conditions :: M.HashMap SignNum [Cond] -> String
code_for_conditions = M.foldlWithKey' (\code k conditions -> code ++ case conditions of
    [] -> concat
        [ "\n\t\tactive_signatures["
        , show k
        , "] = true;"
        , "\n\t\tactive_count ++;"
        ]
    _  -> let conds = intercalate " && " conditions in concat
        [ "\n\t\tif ("
        , conds
        , ") {"
        , "\n\t\t\tactive_signatures["
        , show k
        , "] = true;"
        , "\n\t\t\tactive_count ++;"
        , "\n\t\t}"
        ]
    ) ""


code_for_state :: State -> CFANode -> Bool -> SignSet -> Code
code_for_state s node is_final signs = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , if is_final
        then S.foldl' (\ s k -> s ++ concat
            [ "\nif (active_signatures["
            , show k
            , "] == true) {"
            , "\n\taccepted_signatures[accepted_count++] = "
            , show k
            , ";\n\tactive_signatures["
            , show k
            , "] = false;"
            , "\n\tactive_count --;"
            , "\n\tMARKER = CURSOR;"
            , "\n\tadjust_marker = false;\n}"
            ]) "" $ signs
        else ""
--    , "\nprintf(\"%s\\n\", CURSOR);"
    , if is_final then "\nif (active_count > 0) " else "\n"
    , "switch (*CURSOR++) {"
    , concatMap (\ (l, (_, s')) ->
        let code_for_case :: Char -> String
            code_for_case c = concat
                [ "\n\tcase "
                , "0x" ++ showHex (ord c) ""
--                , let i = ord c in "\'" ++ (if i < 0x20 || i == 0x5C || i == 0x27 || i > 0x7E then "\\x" ++ showHex i "" else [c])
--                , "':\n\t\tgoto m_"
                , ":\n\t\tgoto m_"
                , show s'
                , ";"
                ]
        in case l of
            LabelAny     -> ""
            LabelChar c  -> code_for_case c
            LabelRange s -> concatMap code_for_case s
        ) (M.toList node)
    , "\n\tdefault:"
    , case M.lookup LabelAny node of
        Just (_, s') -> concat
            [ "\n\t\tif (default_case_enabled) {"
            , "\n\t\t\tparsing_default = true;"
            , "\n\t\t\tgoto m_"
            , show s'
            ,  ";"
            , "\n\t\t} else if (adjust_marker) {"
            , "\n\t\t\tMARKER = CURSOR;"
            , "\n\t\t\tgoto m_fin;"
            , "\n\t\t}"
            ]
        Nothing      -> concat
            [ if is_final then "" else "\n\t\tif (adjust_marker && !parsing_default)\n\t\t\tMARKER = CURSOR;"
            , "\n\t\tif (parsing_default)"
            , "\n\t\t\tdefault_case_enabled = false;"
            , "\n\t\tgoto m_fin;"
            ]
    , "\n\t}"
    , if is_final then "\nelse goto m_fin;" else ""
    ]


