module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                   (intercalate, foldl', partition)
import qualified Data.ByteString.Char8 as BS
import           Text.Printf                 (printf)

import           Types
import           CFA


cfa2cpp :: FilePath -> DCFA -> Code -> Code -> [M.HashMap (S.Set Cond) Code] -> Int -> IO ()
cfa2cpp fp dcfa prolog epilog conds2code sign_maxlen =
    let n          = length conds2code
        entry      = code_for_entry n sign_maxlen
        g          = dcfaGraph dcfa
        s0         = initStateDCFA dcfa
        init_state = code_for_state s0 True (isFinalDCFA s0 dcfa) (initNodeDCFA dcfa) conds2code (acceptedSignatures s0 dcfa)
        states     = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , case s of
                    s | isFinalDCFA s dcfa -> BS.empty
                    s                      -> code_for_state s False False node conds2code S.empty
                ]
            ) BS.empty g
        final_states = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , code_for_state s (s == s0) True (M.lookupDefault M.empty s g) conds2code (acceptedSignatures s dcfa)
                ]
            ) BS.empty (finalStates dcfa)
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
    , "\nbool adjust_marker = true;"
    , "\ngoto m_start;"
    , "\n\n\nm_fin:"
    , "\nCURSOR = MARKER;"
    , "\n\n\nm_start:"
    , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
    ]


code_for_conditions :: [[Cond]] -> String
code_for_conditions =
    let f xs = concat ["(", intercalate " && " xs, ")"]
    in  intercalate " || " . map f


code_for_state :: State -> Bool -> Bool -> DCFANode -> [M.HashMap (S.Set Cond) Code] -> SignSet -> Code
code_for_state s is_init is_final node conds2code signs = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , if is_final then code_for_final_state conds2code signs (node == M.empty) else ""
    , case M.toList node of
        [] -> "\n\tgoto m_fin;"
        [(LabelRange r, (_, s))] | S.fromList r == S.fromList ['\x00' .. '\xFF'] -> concat
            [ "\nCURSOR++;"
            , "\ngoto m_"
            , show s
            , ";"
            ]
        n -> concat
            [ "\nswitch (*CURSOR++) {"
            , concatMap (\ (l, (ks, s')) -> concat
                [ let code_for_case = printf "\n\tcase 0x%X:" in case l of
                    LabelChar c  -> code_for_case c
                    LabelRange r -> concatMap code_for_case r
                , if is_init || is_final then "\n\ttoken = MARKER;" else ""
                , if is_init then "\n\tadjust_marker = true;" else ""
                , let conds = S.foldl' (\ conds k -> (map S.toList . M.keys) (conds2code !! k) ++ conds) [] ks in case partition (== []) conds of
                    (conds', conds'') | conds' == [] -> concat
                        [ "\n\t\tif ("
                        , code_for_conditions conds''
                        , ")\n\t\t\tgoto m_"
                        , show s'
                        , ";\n\t\telse {"
                        , "\n\t\t\tMARKER += adjust_marker;"
                        , "\n\t\t\tgoto m_fin;"
                        , "\n\t\t}"
                        ]
                    _ -> concat
                        [ "\n\t\tgoto m_"
                        , show s'
                        , ";"
                        ]
                ]) (M.toList node)
            , "\n\tdefault:"
            , "\n\t\tMARKER += adjust_marker;"
            , "\n\t\tgoto m_fin;"
            , "\n\t}"
            ]
    ]


code_for_final_state :: [M.HashMap (S.Set Cond) Code] -> SignSet -> Bool -> String
code_for_final_state conds2code signs is_empty_node =
    let conds2code' = S.foldl' (\ conds k -> M.toList (conds2code !! k) ++ conds) [] signs in concatMap
        (\ (conds, code) ->
            if not is_empty_node && conds /= S.empty
                then concat
                    [ "\nif ("
                    , code_for_conditions [S.toList conds]
                    , ") {\n\t"
                    , BS.unpack code
                    , "\n\tMARKER = CURSOR;"
                    , "\n\tadjust_marker = false;"
                    , "\n}"
                    ]
                else concat
                    [ "\n\t"
                    , BS.unpack code
                    , "\n\tMARKER = CURSOR;"
                    , if not is_empty_node then "\n\tadjust_marker = false;" else ""
                    ]
        ) conds2code'


