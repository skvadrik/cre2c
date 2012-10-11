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


cfa2cpp :: FilePath -> DCFA -> Code -> Code -> [[Cond]] -> [Code] -> Int -> IO ()
cfa2cpp fp dcfa prolog epilog conditions codes sign_maxlen =
    let n          = length codes
        entry      = code_for_entry n sign_maxlen
        sign2conds = (M.fromList . zip [0 .. n - 1]) conditions
        g          = dcfaGraph dcfa
        s0         = initStateDCFA dcfa
        init_state = code_for_state s0 True (isFinalDCFA s0 dcfa) (initNodeDCFA dcfa) sign2conds (acceptedSignatures s0 dcfa) codes
        states     = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , case s of
                    s | isFinalDCFA s dcfa -> BS.empty
                    s                      -> code_for_state s False False node sign2conds S.empty []
                ]
            ) (BS.empty) g
        final_states = M.foldlWithKey'
            (\ code s node -> BS.concat
                [ code
                , code_for_state s (s == s0) True (M.lookupDefault M.empty s g) sign2conds (acceptedSignatures s dcfa) codes
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
    , "\nbool adjust_marker;"
    , "\ngoto m_start;"
    , "\n\n\nm_fin:"
    , "\nCURSOR = MARKER;"
    , "\n\n\nm_start:"
    , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
    ]


code_for_conditions :: [[Cond]] -> String
code_for_conditions conds = intercalate " || "
    (map
        (\ conditions -> concat
            [ "("
            , intercalate " && " conditions
            , ")"
            ]
        ) conds
    )


code_for_state :: State -> Bool -> Bool -> DCFANode -> M.HashMap SignNum [Cond] -> SignSet -> [Code] -> Code
code_for_state s is_init is_final node sign2conds signs codes = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , if is_final then code_for_final_state sign2conds signs codes (node == M.empty) else ""
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
                [ let code_for_case c = printf "\n\tcase 0x%X:" c in case l of
                    LabelChar c  -> code_for_case c
                    LabelRange r -> concatMap code_for_case r
                , if is_init || is_final then "\n\ttoken = MARKER;" else ""
                , if is_init then "\n\tadjust_marker = true;" else ""
                , let conds = M.filterWithKey (\ k conds -> S.member k ks) sign2conds in case partition (== []) (M.elems conds) of
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


code_for_final_state :: M.HashMap SignNum [Cond] -> SignSet -> [Code] -> Bool -> String
code_for_final_state sign2conds signs codes is_empty_node = concatMap
    (\ k -> let conds = M.lookupDefault [] k sign2conds in
        if not is_empty_node && conds /= []
            then concat
                [ "\nif ("
                , code_for_conditions [conds]
                , ") {\n\t"
                , BS.unpack $ codes !! k
                , "\n\tMARKER = CURSOR;"
                , "\n\tadjust_marker = false;"
                , "\n}"
                ]
            else concat
                [ "\n\t"
                , BS.unpack $ codes !! k
                , "\n\tMARKER = CURSOR;"
                , if not is_empty_node then "\n\tadjust_marker = false;" else ""
                ]
    ) (S.toList signs)


