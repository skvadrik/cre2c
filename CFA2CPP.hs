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


cfa2cpp :: DCFA -> Code -> [M.HashMap (S.Set Cond) Code] -> Int -> Int -> Bool -> Code
cfa2cpp dcfa prolog conds2code maxlen n_scanner once =
    let entry      = code_for_entry maxlen n_scanner once
        g          = dcfa_graph dcfa
        s0         = dcfa_init_state dcfa
        states     = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , case s of
                    s | dcfa_is_final s dcfa -> BS.empty
                    s                        -> code_for_state s (s == s0) False node conds2code S.empty n_scanner once
                ]
            ) BS.empty g
        final_states = M.foldlWithKey'
            (\ code s accepted -> BS.concat
                [ code
                , code_for_state s (s == s0) True (M.lookupDefault M.empty s g) conds2code accepted n_scanner once
                ]
            ) BS.empty (dcfa_final_states dcfa)
    in  BS.concat
            [ prolog
            , entry
            , states
            , final_states
            , if once
                then (BS.pack . concat)
                    [ "\nm"
                    , show n_scanner
                    , "_fin:\n"
                    ]
                else BS.empty
            ]


code_for_entry :: Int -> Int -> Bool -> Code
code_for_entry maxlen k once = let n_scanner = show k in BS.pack $ concat
    [ "\n#define MAXLEN"
    , n_scanner
    , " "
    , show maxlen
    , if once
        then "\ntoken = MARKER;"
        else concat
            [ "\n\n\nm"
            , n_scanner
            , "_fin:"
            , "\nCURSOR = MARKER;"
            ]
    , "\nif (LIMIT - CURSOR < MAXLEN"
    , n_scanner
    , ") FILL();"
    , "\ngoto m"
    , n_scanner
    , "_0;"
    ]


code_for_conditions :: [[Cond]] -> String
code_for_conditions =
    let f xs = concat ["(", intercalate " && " xs, ")"]
    in  intercalate " || " . map f


code_for_state :: State -> Bool -> Bool -> DCFANode -> [M.HashMap (S.Set Cond) Code] -> S.Set Id -> Int -> Bool -> Code
code_for_state s is_init is_final node conds2code signs k once = let n_scanner = show k in (BS.pack . concat)
    [ "\nm"
    , n_scanner
    , "_"
    , show s
    , ":"
    , if is_final then code_for_final_state conds2code signs (node == M.empty) once else ""
    , case M.toList node of
        [] -> concat
            [ "\n\tgoto m"
            , n_scanner
            , "_fin;"
            ]
        [(LabelRange r, (_, s))] | S.fromList r == S.fromList ['\x00' .. '\xFF'] -> concat
            [ "\nCURSOR++;"
            , "\ngoto m"
            , n_scanner
            , "_"
            , show s
            , ";"
            ]
        _ -> concat
            [ "\nswitch (*CURSOR++) {"
            , concatMap (\ (l, (ks, s')) -> concat
                [ let code_for_case = printf "\n\tcase 0x%X:" in case l of
                    LabelChar c  -> code_for_case c
                    LabelRange r -> concatMap code_for_case r
                , if not once && (is_init || is_final) then "\n\ttoken = adjust_marker ? MARKER : token;" else ""
                , if not once && is_init then "\n\tadjust_marker = true;" else ""
                , let conds = S.foldl' (\ conds k -> (map S.toList . M.keys) (conds2code !! k) ++ conds) [] ks in case partition (== []) conds of
                    (conds', conds'') | conds' == [] -> concat
                        [ "\n\t\tif ("
                        , code_for_conditions conds''
                        , ")\n\t\t\tgoto m"
                        , n_scanner
                        , "_"
                        , show s'
                        , ";\n\t\telse {"
                        , if once then "" else "\n\t\t\tMARKER += adjust_marker;"
                        , "\n\t\t\tgoto m"
                        , n_scanner
                        , "_fin;"
                        , "\n\t\t}"
                        ]
                    _ -> concat
                        [ "\n\t\tgoto m"
                        , n_scanner
                        , "_"
                        , show s'
                        , ";"
                        ]
                ]) (M.toList node)
            , "\n\tdefault:"
            , if not once && is_init then "\n\t\tadjust_marker = true;" else ""
            , if once then "" else "\n\t\tMARKER += adjust_marker;"
            , "\n\t\tgoto m"
            , n_scanner
            , "_fin;"
            , "\n\t}"
            ]
    ]


code_for_final_state :: [M.HashMap (S.Set Cond) Code] -> S.Set Id -> Bool -> Bool -> String
code_for_final_state conds2code signs is_empty_node once =
    let conds2code' = S.foldl' (\ conds k -> M.toList (conds2code !! k) ++ conds) [] signs in concatMap
        (\ (conds, code) ->
            if not is_empty_node && conds /= S.empty
                then concat
                    [ "\nif ("
                    , code_for_conditions [S.toList conds]
                    , ") {\n\t"
                    , BS.unpack code
                    , if once then "" else "\n\tMARKER = CURSOR;"
                    , if once then "" else "\n\tadjust_marker = false;"
                    , "\n}"
                    ]
                else concat
                    [ "\n\t"
                    , BS.unpack code
                    , if once then "" else "\n\tMARKER = CURSOR;"
                    , if not once && not is_empty_node then "\n\tadjust_marker = false;" else ""
                    ]
        ) conds2code'


