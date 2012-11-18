module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict       as M
import qualified Data.Set                  as S
import           Data.List                       (foldl', partition, nub, unzip4)
import qualified Data.ByteString.Char8     as BS
import           Text.Printf                     (printf)
import           Text.PrettyPrint.HughesPJ       (($$), (<>), ($+$), Doc)
import qualified Text.PrettyPrint.HughesPJ as PP
import           Data.Maybe                      (fromJust)

import           Types
import           CFA


type Conds2Code          = M.HashMap (S.Set Cond) Code
type RegexpId2RegexpInfo = M.HashMap RegexpId (Maybe BlockName, Conds2Code)
data StateInfo           = SI State Bool Bool DCFANode (Maybe (S.Set RegexpId))
data BlockInfo           = BI Int Options RegexpId2RegexpInfo


cfa2cpp :: DCFA -> Code -> RegexpId2RegexpInfo -> Int -> Int -> Options -> Code
cfa2cpp dcfa prolog id_info maxlen n_scanner opts =
    let entry      = codegen_entry maxlen n_scanner opts id_info
        g          = dcfa_graph dcfa
        s0         = dcfa_init_state dcfa
        bi         = BI n_scanner opts id_info
        states     = M.foldlWithKey'
            (\ code s node ->
                let is_init  = s == s0
                    is_final = dcfa_is_final s dcfa
                    si       = SI s is_init False node Nothing
                in  code $$$ if is_final
                        then PP.empty
                        else codegen_state si bi
            ) PP.empty g
        final_states = M.foldlWithKey'
            (\ code s accepted ->
                let is_init = s == s0
                    node    = M.lookupDefault M.empty s g
                    si      = SI s is_init True node (Just accepted)
                in  code $$$ codegen_state si bi
            ) PP.empty (dcfa_final_states dcfa)
        ending      = router5 opts n_scanner id_info
    in  (BS.pack . PP.render)
            ( ( PP.text . BS.unpack ) prolog
            $$$ entry
            $$$ states
            $$$ final_states
            $$$ ending
            )


instance Eq Doc where
    d1 == d2 = PP.render d1 == PP.render d2


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 | PP.isEmpty d1 = d2
($$$) d1 d2 | PP.isEmpty d2 = d1
($$$) d1 d2                  = d1 $$ PP.text "" $$ d2
infixl 5 $$$


doc_decl :: Int -> Int -> Doc
doc_decl k n =
    PP.char 'm'
    <> PP.int k
    <> PP.char '_'
    <> PP.int n
    <> PP.colon


doc_goto :: Int -> Int -> Doc
doc_goto k n =
    PP.text "goto m"
    <> PP.int k
    <> PP.char '_'
    <> PP.int n
    <> PP.semi


doc_decl_label :: Int -> Options -> String -> Doc
doc_decl_label k opts s =
    let block_id = case opts of
            Options (Block b) _ -> PP.text b
            _                   -> PP.int k
    in  PP.text "m_"
        <> block_id
        <> PP.text s


doc_goto_label :: Int -> Options -> String -> Doc
doc_goto_label k opts s =
    let block_id = case opts of
            Options (Block b) _ -> PP.text b
            _                   -> PP.int k
    in  PP.text "goto m_"
        <> block_id
        <> PP.text s


doc_decl_fin :: Int -> Options -> Doc
doc_decl_fin k opts = doc_decl_label k opts "_fin:"


doc_decl_start :: Int -> Options -> Doc
doc_decl_start k opts = doc_decl_label k opts "_start:"


doc_goto_fin :: Int -> Options -> Doc
doc_goto_fin k opts = doc_goto_label k opts "_fin;"


--doc_goto_start :: Int -> Options -> Doc
--doc_goto_start k opts = doc_goto_label k opts "_start;"


doc_goto_block :: BlockName -> Doc
doc_goto_block block =
    PP.text "goto m_"
    <> PP.text block
    <> PP.text "_start;"


wrap_in_braces :: Doc -> Doc
wrap_in_braces d = PP.text "{" $+$ PP.nest 4 d $$ PP.text "}"


doc_if :: [S.Set Cond] -> Doc -> Doc
doc_if cs doc =
    let f :: String -> [Doc] -> Doc
        f s xs = case nub xs of
            []   -> PP.empty
            [x]  -> x
            xs'  ->
                ( PP.parens
                . PP.hcat
                . PP.punctuate (PP.text s)
                ) xs'
        cs'   = map (f " && " . map PP.text . S.toList) cs
        cs''  = f " || " cs'
    in  if cs'' == PP.empty
            then doc
            else PP.text "if " <> PP.parens cs'' $$ wrap_in_braces doc


doc_if_else :: [S.Set Cond] -> Doc -> Doc -> Doc
doc_if_else cs d1 d2 =
    doc_if cs d1
    $$ PP.text "else"
    $$ wrap_in_braces d2


doc_switch :: Doc -> Doc -> Doc
doc_switch d1 d2 = PP.text "switch " <> (PP.parens d1) $$ (wrap_in_braces d2)


doc_case_break :: Int -> Doc ->  Doc
doc_case_break n d =
    PP.text "case " <> PP.int n <> PP.colon
    $$ PP.nest 4 (d $$ PP.text "break;")



doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


get_conds2code :: RegexpId2RegexpInfo -> S.Set RegexpId -> [(RegexpId, S.Set Cond, Maybe BlockName, Code)]
get_conds2code id_info ids  =
    let f id_conds_code id (block, conds2code) = id_conds_code ++ if id `S.member` ids
            then map (\ (conds, code) -> (id, conds, block, code)) (M.toList conds2code)
            else []
    in  M.foldlWithKey' f [] id_info


get_conds :: RegexpId2RegexpInfo -> S.Set RegexpId -> [S.Set Cond]
get_conds id_info = (\ (_, conds, _, _) -> conds) . unzip4 . get_conds2code id_info


codegen_match_code :: RegexpId2RegexpInfo -> Doc
codegen_match_code id_info =
    let ids      = (S.fromList . M.keys) id_info
        id_info' = get_conds2code id_info ids
        f d (id, conds, block, code) =
            let d1 = doc_if [conds] ((PP.text . BS.unpack) code)
                d2 = case block of
                    Just b  -> doc_goto_block b
                    Nothing -> PP.empty
                d3 = doc_case_break id (d1 $$ d2)
            in  d $$ d3
        d = foldl' f PP.empty id_info'
    in  doc_switch (PP.text "accept") d


router0 :: Options -> Int -> RegexpId2RegexpInfo -> Doc
router0 opts k id_info =
    let d1 = doc_decl_fin k opts
        d2 = PP.text "token = MARKER;"
        d3 = codegen_match_code id_info
        d4 = PP.text "accept = -1;"
        d5 = PP.text "CURSOR = MARKER;"
    in  case opts of
            Options Single Longest ->       d2       $$ d4
            Options _      Longest -> d1       $$ d3 $$ d4 $$ d5
            Options Single All     ->       d2
            Options _      All     -> d1                   $$ d5


router1 :: Options -> Bool -> Bool -> Doc
router1 opts is_init is_final = case opts of
    Options Single _        -> PP.empty
    Options _      All      -> case (is_init, is_final) of
        (True, True)  -> PP.text "token = adjust_marker ? MARKER : token;"
        (True, False) -> PP.text "adjust_marker = true;"
        _             -> PP.empty
    Options _       Longest -> case (is_init, is_final) of
        (True, True)  -> PP.text "token = MARKER;"
        _             -> PP.empty


router2 :: Options -> Doc
router2 opts = case opts of
    Options Single _       -> PP.empty
    Options _      All     -> PP.text "MARKER += adjust_marker;"
    Options _      Longest -> PP.text "MARKER ++;"


router3 :: Options -> Bool -> Doc
router3 opts is_init = case opts of
    Options Single _       -> PP.empty
    Options _      All     -> (if is_init then PP.text "adjust_marker = true;" else PP.empty) $$ PP.text "MARKER += adjust_marker;"
    Options _      Longest -> PP.text "MARKER ++;"


router4 :: Options -> Bool -> (Int, Code) -> Doc
router4 opts empty_node (k, code) =
    let d1 = ( PP.text . BS.unpack ) code
        d2 = PP.text "accept = " <> PP.int k <> PP.semi
        d3 = PP.text "MARKER = CURSOR;"
        d4 = if empty_node then PP.empty else PP.text "adjust_marker = false;"
    in  case opts of
            Options Single Longest ->       d2
            Options _      Longest ->       d2 $$ d3
            Options Single All     -> d1
            Options _      All     -> d1       $$ d3 $$ d4


router5 :: Options -> Int -> RegexpId2RegexpInfo -> Doc
router5 opts k id_info = case opts of
    Options Single _   -> doc_decl_fin k opts $$ codegen_match_code id_info
    _                  -> PP.empty


codegen_entry :: Int -> Int -> Options -> RegexpId2RegexpInfo -> PP.Doc
codegen_entry maxlen k opts id_info =
    doc_decl_start k opts
    $$ PP.text "#define MAXLEN" <> PP.int k <> PP.space <> PP.int maxlen
    $$ router0 opts k id_info
    $$ PP.text "if (LIMIT - CURSOR < MAXLEN" <> PP.int k <> PP.text ") FILL();"
    $$ doc_goto k 0


codegen_cases :: StateInfo -> BlockInfo -> PP.Doc
codegen_cases (SI _ is_init is_final node _) (BI k opts id_info) =
    let show_u8b    = printf "case 0x%02X:"
        case_head l = case l of
            LabelChar c  -> PP.text $ show_u8b c
            LabelRange r -> PP.vcat $ map (PP.text . show_u8b) r
        case_body ids s =
            let conds          = get_conds id_info ids
                is_conditional = (null . fst . partition (== S.empty)) conds
                goto_m         = doc_goto k s
                goto_fin       = router2 opts $$ doc_goto_fin k opts
            in  if is_conditional
                    then doc_if_else conds goto_m goto_fin
                    else goto_m
        one_case doc l (ids, s) =
            doc
            $$ case_head l
            $$ PP.nest 4
                ( router1 opts is_init is_final
                $$ case_body ids s
                )
        cases        = M.foldlWithKey' one_case PP.empty node
        default_case = doc_default (router3 opts is_init $$ doc_goto_fin k opts)
    in  cases $$ default_case


codegen_fstate :: StateInfo -> BlockInfo -> PP.Doc
codegen_fstate (SI _ _ _ node ids) (BI _ opts id_info) =
    let id_conds_code = get_conds2code id_info (fromJust ids)
        f doc (id, conds, _, code) =
            let code'  = router4 opts (node == M.empty) (id, code)
                code'' = doc_if [conds] code'
            in  doc $$ PP.nest 4 code''
    in  foldl' f  PP.empty id_conds_code


codegen_state :: StateInfo -> BlockInfo -> PP.Doc
codegen_state si@(SI s _ is_final node _) bi@(BI k opts _) =
    let is_full range = S.fromList range == S.fromList ['\x00' .. '\xFF']
        fstate = if is_final then codegen_fstate si bi else PP.empty
        state  = case M.toList node of
            []                                   -> doc_goto_fin k opts
            [(LabelRange r, (_, s))] | is_full r -> PP.text "CURSOR++;" $$ doc_goto k s
            _                                    -> doc_switch (PP.text "*CURSOR++") (codegen_cases si bi)
    in  doc_decl k s
        $$ fstate
        $$ PP.nest 4 state


