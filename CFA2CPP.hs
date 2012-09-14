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
    let n          = length codes
        entry      = code_for_entry n sign_maxlen
        sign2conds = M.fromList $ zip [0 .. n - 1] conditions
        states     = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , case s of
                    s | s == initialState cfa -> code_for_initial_state (initialNode cfa) sign2conds
                    s | isFinal s cfa         -> code_for_final_state s node (acceptedSignatures s cfa) codes
                    s                         -> code_for_state s node
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


code_for_entry :: Int -> Int -> Code
code_for_entry n sign_maxlen = BS.pack $ concat
    [ "\n#define NUM_SIGN "
    , show n
    , "\n#define SIGN_MAXLEN "
    , show sign_maxlen
    , "\n\nbool forbidden_signatures[NUM_SIGN + 1];"
    , "\nfor (int i = 0; i <= NUM_SIGN; i++)"
    , "\n\tforbidden_signatures[i] = NUM_SIGN;"
    , "\nint  forbidden_count;"
    , "\nint  new_forbidden_count;"
    , "\nbool adjust_marker;"
    , "\nbool default_case_enabled = true;"
    , "\nbool parsing_default;"
    , "\nint  j;"
    , "\n\ngoto m_start;\n\n"

    , "\n\n\nm_fin:"
    , "\nCURSOR = MARKER;"
    , "\nif (!default_case_enabled && !parsing_default)"
    , "\n\tdefault_case_enabled = true;"

    , "\n\n\nm_start:"
    , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
    ]


code_for_initial_state :: CFANode -> M.HashMap SignNum [Cond] -> Code
code_for_initial_state node0 sign2conds = BS.pack $ concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (l, (ks, s')) ->
        let code_for_cond_case :: Char -> String
            code_for_cond_case c = concat
                [ "\n\tcase "
                , "0x" ++ showHex (ord c) ""
                , ":"

--                , "\nprintf(\"case0: %X\\n\", "
--                , "0x" ++ showHex (ord c) ""
--                , ");"

--                , "\nprintf(\"%p\\n\", MARKER);"

                , "\n\t\tnew_forbidden_count = 0;"
                , "\n\t\tadjust_marker       = true;"
                , "\n\t\tparsing_default     = false;"
                , "\n\t\ttoken               = MARKER;"
                , code_for_conditions $ M.filterWithKey (\ k _ -> S.member k ks) sign2conds
                , "\n\t\tfor (int i = new_forbidden_count; i < forbidden_count; i++)"
                , "\n\t\t\tforbidden_signatures[i] = NUM_SIGN;"
                , "\n\t\tforbidden_count = new_forbidden_count;"
                , "\n\t\tif (forbidden_count <"
                , show $ S.size ks
                , ")"
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
--    , "\n\t\tprintf(\"def0\\n\");"
    , case M.lookup LabelAny node0 of
        Just (ks, s') -> concat
            [ "\n\t\tif (default_case_enabled) {"

--            , "\nprintf(\"def0\\n\");"

            , "\n\t\t\tnew_forbidden_count = 0;"
            , "\n\t\t\tadjust_marker       = true;"
            , "\n\t\t\tparsing_default     = true;"
            , "\n\t\t\ttoken               = MARKER;"
            , code_for_conditions $ M.filterWithKey (\ k _ -> S.member k ks) sign2conds
            , "\n\t\t\tfor (int i = new_forbidden_count; i < forbidden_count; i++)"
            , "\n\t\t\t\tforbidden_signatures[i] = NUM_SIGN;"
            , "\n\t\t\tforbidden_count = new_forbidden_count;"
            , "\n\t\t\tif (forbidden_count <"
            , show $ S.size ks
            , ")"
            , "\n\t\t\tgoto m_"
            , show s'
            ,  ";"
            , "\n\t\t} else"
            , "\n\t\t\tMARKER ++;// = CURSOR;"
            , "\n\t\tgoto m_fin;"
            ]
        Nothing      -> concat
            [ "\n\t\tMARKER ++;// = CURSOR;"

--            , "\nprintf(\"def0 - fallout\\n\");"

            , "\n\t\tgoto m_fin;"
            ]
    , "\n\t}"
    ]


code_for_conditions :: M.HashMap SignNum [Cond] -> String
code_for_conditions = M.foldlWithKey' (\code k conditions -> code ++ case conditions of
    [] -> ""
    _  -> concat
        [ "\n\t\tif (!("
        , intercalate " && " conditions
        , "))\n\t\tforbidden_signatures[new_forbidden_count++] = "
        , show k
        , ";"
        ]
    ) ""


code_for_state :: State -> CFANode -> Code
code_for_state s node = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
--    , "\nprintf(\"%s\\n\", CURSOR);"
    , "\nswitch (*CURSOR++) {"
    , concatMap (\ (l, (_, s')) ->
        let code_for_case :: Char -> String
            code_for_case c = concat
                [ "\n\tcase "
                , "0x"
                , showHex (ord c) ""
                , ":"

--                , "\nprintf(\"case: %X\\n\", "
--                , "0x" ++ showHex (ord c) ""
--                , ");"

                , "\n\t\tgoto m_"
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
            , "\n\t\t} else if (1 || adjust_marker)"
            , "\n\t\t\tMARKER ++;// = CURSOR;"

--            , "\n\t\tprintf(\"def\\n\");"

            , "\n\t\tgoto m_fin;"
            ]

        Nothing      -> concat
            [ "\n\t\tif (1 || adjust_marker && !parsing_default)"
            , "\n\t\t\tMARKER ++;// = CURSOR"
            , "\n\t\tif (parsing_default)"
            , "\n\t\t\tdefault_case_enabled = false;"

--            , "\n\t\tprintf(\"def - fallout\\n\");"

            , "\n\t\tgoto m_fin;"
            ]
    , "\n\t}"
    ]


code_for_final_state :: State -> CFANode -> SignSet -> [Code] -> Code
code_for_final_state s node signs codes = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , S.foldl' (\ s k -> s ++ concat
        [ "\nfor (j = 0; j <= forbidden_count; j++)"
        , "\n\tif (forbidden_signatures[j] == "
        , show k
        , ")\n\t\tbreak;"
--        , "\nprintf(\"fc = %d\\n\", forbidden_count);"
        , "\nif (j == forbidden_count + 1) {\n\t"
        , BS.unpack $ codes !! k
        , "\n\tMARKER = CURSOR;"
        , "\n\tadjust_marker = false;\n}"
        ]) "" signs
--    , "\nprintf(\"%s\\n\", CURSOR);"
    , "\nswitch (*CURSOR++) {"
    , concatMap (\ (l, (_, s')) ->
        let code_for_case :: Char -> String
            code_for_case c = concat
                [ "\n\tcase "
                , "0x"
                , showHex (ord c) ""
                , ":"

--                , "\nprintf(\"caseF: %X\\n\", "
--                , "0x" ++ showHex (ord c) ""
--                , ");"

                , "\n\t\tgoto m_"
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
            , "\n\t\t} else {"
            , "\n\t\t\tif (adjust_marker)"
            , "\n\t\t\t\tMARKER = CURSOR;"

--            , "\n\t\tprintf(\"defF\\n\");"

            , "\n\t\t\tgoto m_fin;"
            , "\n\t\t}"
            ]
        Nothing      -> concat
            [ "\n\t\tif (parsing_default)"
            , "\n\t\t\tdefault_case_enabled = false;"

--            , "\n\t\tprintf(\"defF - fallout\\n\");"

            , "\n\t\tgoto m_fin;"
            ]
    , "\n\t}"
-- it was for if(active_count > 0)    , "\nelse goto m_fin;"
    ]


