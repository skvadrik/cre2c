module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                   (intercalate, foldl', sort)
import qualified Data.ByteString.Char8 as BS
import           Text.Printf                 (printf)

import           Types
import           CFA


cfa2cpp :: FilePath -> DCFA -> Code -> Code -> [[Cond]] -> [Code] -> Int -> IO ()
cfa2cpp fp dcfa prolog epilog conditions codes sign_maxlen =
    let n          = length codes
        entry      = code_for_entry n sign_maxlen
        sign2conds = (M.fromList . zip [0 .. n - 1]) conditions
        g          = dcfaGraph dcfa
        init_state = code_for_initial_state (initNodeDCFA dcfa) sign2conds
        states     = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , case s of
                    s | isFinalDCFA s dcfa -> BS.empty
                    s                      -> code_for_state s node
                ]
            ) (BS.empty) g
        final_states = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , code_for_final_state s (M.lookupDefault M.empty s g) (acceptedSignatures s dcfa) codes
                ]
            ) (BS.empty) (finalStates dcfa)
    in  BS.writeFile fp $ BS.concat
            [ prolog
            , entry
            , init_state
            , states
            , final_states
            , epilog
            ]


code_for_entry :: Int -> Int -> Code
code_for_entry n sign_maxlen = BS.pack $ concat
    [ "\n#define NUM_SIGN "
    , show n
    , "\n#define SIGN_MAXLEN "
    , show sign_maxlen
    , "\n\nint forbidden_signatures[NUM_SIGN];"
    , "\nint  forbidden_count;"
    , "\nbool adjust_marker;"
    , "\nint  j;"
    , "\n\ngoto m_start;\n\n"

    , "\n\n\nm_fin:"
    , "\nCURSOR = MARKER;"

    , "\n\n\nm_start:"
    , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
    ]

-- remove useless defaults

code_for_initial_state :: DCFAInitNode -> M.HashMap SignNum [Cond] -> Code
code_for_initial_state node0 sign2conds = BS.pack $ concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (l, (ks, s')) -> concat
        [ let code_for_case c = printf "\n\tcase 0x%X:" c in case l of
            LabelChar c  -> code_for_case c
            LabelRange r -> concatMap code_for_case r
        , "\n\t\tadjust_marker   = true;"
        , "\n\t\ttoken           = MARKER;"
        , "\n\t\tforbidden_count = 0;"
        , let conds = M.filterWithKey (\ k conds -> conds /= [] && S.member k ks) sign2conds in if conds /= M.empty
            then concat
                [ code_for_conditions conds
                , "\n\t\tif (forbidden_count <"
                , show $ S.size ks
                , ")"
                , "\n\t\t\tgoto m_"
                ]
            else "\n\t\tgoto m_"
        , show s'
        , ";"
        ]) (M.toList node0)
    , "\n\tdefault:"
    , "\n\t\tMARKER ++;"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]


code_for_conditions :: M.HashMap SignNum [Cond] -> String
code_for_conditions = M.foldlWithKey' (\code k conditions -> code ++ case conditions of
    [] -> error "impossible"
    _  -> concat
        [ "\n\t\tif (!("
        , intercalate " && " conditions
        , "))\n\t\t\tforbidden_signatures[forbidden_count++] = "
        , show k
        , ";"
        ]
    ) ""


code_for_state :: State -> DCFANode -> Code
code_for_state s node = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , case M.toList node of
        [(LabelRange r, s)] | S.fromList r == S.fromList ['\x00' .. '\xFF'] -> "\nCURSOR++;\ngoto m_" ++ show s ++ ";"
        n -> concat
            [ "\nswitch (*CURSOR++) {"
            , concatMap (\ (l, s') -> concat
                [ let code_for_case c = printf "\n\tcase 0x%X:" c in case l of
                    LabelChar c  -> code_for_case c
                    LabelRange r -> concatMap code_for_case r
                , "\n\t\tgoto m_"
                , show s'
                , ";"
                ]) n
            , "\n\tdefault:"
            , "\n\t\tMARKER += adjust_marker;"
            , "\n\t\tgoto m_fin;"
            , "\n\t}"
            ]
    ]


code_for_final_state :: State -> DCFANode -> SignSet -> [Code] -> Code
code_for_final_state s node signs codes = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , S.foldl' (\ s k -> s ++ concat
        [ "\nfor (j = 0; j < forbidden_count; j++)"
        , "\n\tif (forbidden_signatures[j] == "
        , show k
        , ")\n\t\tbreak;"
        , "\nif (j == forbidden_count) \n\t"
        , BS.unpack $ codes !! k
        , "\nMARKER = CURSOR;"
        , "\nadjust_marker = false;\n"
        ]) "" signs
    , "\nswitch (*CURSOR++) {"
    , concatMap (\ (l, s') -> concat
        [ let code_for_case c = printf "\n\tcase 0x%X:" c in case l of
            LabelChar c  -> code_for_case c
            LabelRange r -> concatMap code_for_case r
        , "\n\t\tgoto m_"
        , show s'
        , ";"
        ]) $ M.toList node
    , "\n\tdefault:"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]


