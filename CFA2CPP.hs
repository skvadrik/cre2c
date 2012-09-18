module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                   (intercalate, foldl')
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
                    s | s == initialState cfa -> code_for_initial_state ((merge_cases . initialNode) cfa) sign2conds
                    s | isFinal s cfa         -> code_for_final_state s (merge_cases node) (acceptedSignatures s cfa) codes
                    s                         -> code_for_state s (merge_cases node)
                ]
            ) (BS.pack "") (cfaGraph cfa)
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
    , "\n\nbool forbidden_signatures[NUM_SIGN];"
    , "\nfor (int i = 0; i < NUM_SIGN; i++)"
    , "\n\tforbidden_signatures[i] = NUM_SIGN;"
    , "\nint  forbidden_count;"
    , "\nint  new_forbidden_count;"
    , "\nbool adjust_marker;"
    , "\nint  j;"
    , "\n\ngoto m_start;\n\n"

    , "\n\n\nm_fin:"
    , "\nCURSOR = MARKER;"

    , "\n\n\nm_start:"
    , "\nif (LIMIT - CURSOR < SIGN_MAXLEN) FILL();\n\n"
    ]


{-
три проблемы
1) Range ---- диапазоны. Видно придётся их делать на альтернативах.
2) Any ---- их надо загонять не только в дефолты, но из  всех состояний добавлять дуги.
3) case sensitivity
-}


code_for_initial_state :: DCFANode -> M.HashMap SignNum [Cond] -> Code
code_for_initial_state node0 sign2conds = BS.pack $ concat
    [ "\nswitch (*CURSOR++) {\n\t"
    , concatMap (\ (c, (ks, s')) -> concat
        [ "\n\tcase "
        , "0x" ++ showHex (ord c) ""
        , ":"
        , "\n\t\tnew_forbidden_count = 0;"
        , "\n\t\tadjust_marker       = true;"
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
        ]) (M.toList node0)
    , "\n\tdefault:"
    , "\n\t\tMARKER ++;"
    , "\n\t\tgoto m_fin;"
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


merge_cases :: CFANode -> DCFANode
merge_cases node = M.foldlWithKey'
    (\ n l (ks, s) ->
        let merge_arc :: DCFANode -> Char -> DCFANode
            merge_arc n c = M.insertWith (\ _ (ks', s') -> (S.union ks ks', if s == s' then s else error "nondeterministic arcs")) c (ks, s) n
        in  case l of
                LabelChar c  -> merge_arc n c
                LabelRange r -> foldl' merge_arc n r
    ) M.empty node


code_for_state :: State -> DCFANode -> Code
code_for_state s node = (BS.pack . concat)
    [ "\nm_"
    , show s
    , ":"
    , "\nswitch (*CURSOR++) {"
    , concatMap (\ (c, (_, s')) -> concat
        [ "\n\tcase "
        , "0x"
        , showHex (ord c) ""
        , ":"
        , "\n\t\tgoto m_"
        , show s'
        , ";"
        ]) (M.toList node)
    , "\n\tdefault:"
    , "\n\t\tMARKER += adjust_marker;"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
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
        , "\nif (j == forbidden_count) {\n\t"
        , BS.unpack $ codes !! k
        , "\n\tMARKER = CURSOR;"
        , "\n\tadjust_marker = false;\n}"
        ]) "" signs
    , "\nswitch (*CURSOR++) {"
    , concatMap (\ (c, (_, s')) -> concat
        [ "\n\tcase "
        , "0x"
        , showHex (ord c) ""
        , ":"
        , "\n\t\tgoto m_"
        , show s'
        , ";"
        ]) (M.toList node)
    , "\n\tdefault:"
    , "\n\t\tgoto m_fin;"
    , "\n\t}"
    ]


