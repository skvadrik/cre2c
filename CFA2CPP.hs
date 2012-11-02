module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                   (intercalate, partition)
import qualified Data.ByteString.Char8 as BS
import           Text.Printf                 (printf)

import           Types
import           CFA


cfa2cpp :: DCFA -> Code -> [M.HashMap (S.Set Cond) Code] -> Int -> Code
cfa2cpp dcfa prolog conds2code sign_maxlen =
    let n          = length conds2code
        entry      = code_for_entry n sign_maxlen
        g          = dcfa_graph dcfa
        s0         = dcfa_init_state dcfa
        states     = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , case s of
                    s | dcfa_is_final s dcfa -> BS.empty
                    s                        -> code_for_state s (s == s0) False node conds2code S.empty
                ]
            ) BS.empty g
        final_states = M.foldlWithKey'
            (\ code s accepted -> BS.concat
                [ code
                , code_for_state s (s == s0) True (M.lookupDefault M.empty s g) conds2code accepted
                ]
            ) BS.empty (dcfa_final_states dcfa)
    in  BS.concat
            [ prolog
            , entry
            , states
            , final_states
            , BS.pack "\n#undef NUM_SIGN"
            , BS.pack "\n#undef SIGN_MAXLEN"
            ]


code_for_entry :: Int -> Int -> Code
code_for_entry n sign_maxlen = BS.pack $ concat
    [ "\n#define NUM_SIGN "
    , show n
    , "\n#define SIGN_MAXLEN "
    , show sign_maxlen
    , "\ngoto m_start;"
    , "\n\n\nm_fin:"
    , "\nCURSOR = MARKER;"
    , "\n\n\nm_start:"
    , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
    , "\ngoto m_0;"
    ]


code_for_conditions :: [[Cond]] -> String
code_for_conditions =
    let f xs = concat ["(", intercalate " && " xs, ")"]
    in  intercalate " || " . map f


code_for_state :: State -> Bool -> Bool -> DCFANode -> [M.HashMap (S.Set Cond) Code] -> S.Set Id -> Code
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
        _ -> concat
            [ "\nswitch (*CURSOR++) {"
            , concatMap (\ (l, (ks, s')) -> concat
                [ let code_for_case = printf "\n\tcase 0x%X:" in case l of
                    LabelChar c  -> code_for_case c
                    LabelRange r -> concatMap code_for_case r
                , if is_init || is_final then "\n\ttoken = adjust_marker ? MARKER : token;" else ""
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
            , "\n\t\tadjust_marker = true;"
            , "\n\t\tMARKER += adjust_marker;"
            , "\n\t\tgoto m_fin;"
            , "\n\t}"
            ]
    ]


code_for_final_state :: [M.HashMap (S.Set Cond) Code] -> S.Set Id -> Bool -> String
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


