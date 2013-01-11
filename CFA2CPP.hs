module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict       as M
import qualified Data.Set                  as S
import           Data.List                       (foldl', partition, nub, sortBy)
import           Text.PrettyPrint.HughesPJ       (($$), (<>), Doc)
import qualified Text.PrettyPrint.HughesPJ as PP
import           Data.Maybe                      (fromJust)

import           Types                     hiding (err)
import           Helpers
import           CFA


cfa2cpp :: Labellable a => DCFA a -> SCode -> Int -> BlockInfo a -> SCode
cfa2cpp dcfa prolog maxlen bi@(BI _ _ _ _) =
    let entry      = codegen_entry maxlen bi
        g          = dcfa_graph dcfa
        s0         = dcfa_init_state dcfa
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
        ending      = router5 bi
    in  PP.render $
            PP.text prolog
            $$$ entry
            $$$ states
            $$$ final_states
            $$$ ending


doc_decl_ :: Doc -> Doc -> Doc
doc_decl_ d1 d2 = PP.text "m_" <> d1 <> PP.char '_' <> d2 <> PP.colon


doc_goto_ :: Doc -> Doc -> Doc
doc_goto_ d1 d2 = PP.text "goto m_" <> d1 <> PP.char '_' <> d2 <> PP.semi


doc_decl :: IBlkID -> IStateID -> Doc
doc_decl k n =
    let d1 = PP.int k
        d2 = PP.int n
    in  doc_decl_ d1 d2


doc_goto :: IBlkID -> IStateID -> Doc
doc_goto k n =
    let d1 = PP.int k
        d2 = PP.int n
    in  doc_goto_ d1 d2


doc_decl_fin :: IBlkID -> Options -> Doc
doc_decl_fin k opts =
    let d1 = case opts of
            OptsBlock b _ _ _ -> PP.text b
            _                 -> PP.int k
        d2 = PP.text "fin"
    in  doc_decl_ d1 d2


doc_goto_fin :: IBlkID -> Options -> Doc
doc_goto_fin k opts =
    let d1 = case opts of
            OptsBlock b _ _ _ -> PP.text b
            _                 -> PP.int k
        d2 = PP.text "fin"
    in  doc_goto_ d1 d2


doc_decl_start :: IBlkID -> Options -> Doc
doc_decl_start k opts =
    let d1 = case opts of
            OptsBlock b _ _ _ -> PP.text b
            _                 -> PP.int k
        d2 = PP.text "start"
    in  doc_decl_ d1 d2


doc_goto_start :: IBlkID -> Options -> Doc
doc_goto_start k opts =
    let d1 = case opts of
            OptsBlock b _ _ _ -> PP.text b
            _                 -> PP.int k
        d2 = PP.text "start"
    in  doc_goto_ d1 d2


doc_goto_block :: SBlkname -> Doc
doc_goto_block block =
    PP.text "goto m_"
    <> PP.text block
    <> PP.text "_start;"


doc_if :: [S.Set SCond] -> Doc -> Doc
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


doc_if_else :: [S.Set SCond] -> Doc -> Doc -> Doc
doc_if_else cs d1 d2 =
    doc_if cs d1
    $$ PP.text "else"
    $$ wrap_in_braces d2


doc_switch :: Doc -> Doc -> Doc
doc_switch d1 d2 = PP.text "switch " <> (PP.parens d1) $$ (wrap_in_braces d2)


doc_case :: Labellable a => Label a -> Doc
doc_case l =
    let f x = PP.text "case " <> (PP.text . show_hex) x <> PP.colon
    in  case l of
            LOne x    -> f x
            LRange xs -> (PP.vcat . map f . S.toList) xs


doc_case_break :: IRegID -> Doc -> Doc
doc_case_break n d =
    PP.text "case " <> PP.int n <> PP.colon
    $$ PP.nest 4 (d $$ PP.text "break;")


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


get_conds2code :: MRegID2RegInfo -> S.Set IRegID -> [(IRegID, S.Set SCond, Maybe SBlkname, SCode)]
get_conds2code id_info ids  =
    let f id_conds_code id (block, conds2code) = id_conds_code ++ if id `S.member` ids
            then map (\ (conds, code) -> (id, conds, block, code)) (M.toList conds2code)
            else []
    in  M.foldlWithKey' f [] id_info


get_conds :: MRegID2RegInfo -> S.Set IRegID -> [S.Set SCond]
get_conds id_info ids =
    let f conds id (_, conds2code) = conds ++ if id `S.member` ids
            then (map fst . M.toList) conds2code
            else []
    in  M.foldlWithKey' f [] id_info


codegen_match_code :: MRegID2RegInfo -> Maybe SCode -> Doc
codegen_match_code id_info df =
    let ids      = (S.fromList . M.keys) id_info
        id_info' = get_conds2code id_info ids
        f d (id, conds, block, code) =
            let d1 = doc_if [conds] (PP.text code)
                d2 = case block of
                    Just b  -> doc_goto_block b
                    Nothing -> PP.empty
                d3 = doc_case_break id (d1 $$ d2)
            in  d $$ d3
        d1 = foldl' f PP.empty id_info'
        d2 = case df of
            Just code -> doc_default $ PP.text code
            Nothing   -> PP.empty
    in  doc_switch (PP.text "ACCEPT") (d1 $$ d2)


codegen_refill :: Doc
codegen_refill = PP.text "if (LIMIT - CURSOR < MAXLEN) FILL();"


router0 :: Options -> IBlkID -> MRegID2RegInfo -> Doc
router0 opts k id_info =
    let d0 = doc_goto_start k opts
        d1 = doc_decl_fin k opts
        d2 = codegen_match_code id_info (default_action opts)
        d3 = doc_decl_start k opts
        d4 = PP.text "TOKEN = MARKER;"
        d5 = PP.text "CURSOR = MARKER;"
        d6 = PP.text "ACCEPT = -1;"
        d7 = codegen_refill
    in  case opts of
            Opts      Single _       _ _ (Just _) -> d4
            Opts      Single _       _ _ Nothing  -> d4 $$ d7
            Opts      Normal Longest _ _ _        -> d0 $$ d1 $$ d2 $$ d5 $$ d6 $$ d3 $$ d7
            Opts      Normal All     _ _ _        -> d0 $$ d1 $$ d5 $$ d3 $$ d7
            OptsBlock _              _ _ _        -> d0 $$ d1 $$ d2 $$ d3 $$ d5 $$ d6 $$ d7


router1 :: Options -> Bool -> Bool -> Doc
router1 opts is_init is_final =
    let d1 = case (is_init, is_final) of
            (True, True)  -> PP.text "TOKEN = ADJUST_MARKER ? MARKER : TOKEN;"
            (True, False) -> PP.text "TOKEN = MARKER;" $$ PP.text "ADJUST_MARKER = true;"
            _             -> PP.empty
        d2 = case is_init of
            True -> PP.text "TOKEN = MARKER;"
            _    -> PP.empty
    in  case opts of
            Opts      Single _       _ _ _ -> PP.empty
            Opts      Normal All     _ _ _ -> d1
            Opts      Normal Longest _ _ _ -> d2
            OptsBlock _              _ _ _ -> d2


router2 :: Options -> Doc
router2 opts =
    let d1 = PP.text "MARKER += ADJUST_MARKER;"
        d2 = PP.text "MARKER ++;"
    in  case opts of
            Opts      Single _       _ _ _ -> PP.empty
            Opts      Normal All     _ _ _ -> d1
            Opts      Normal Longest _ _ _ -> d2
            OptsBlock _              _ _ _ -> d2


-- я предполагаю что начальное состояние не может быть финальным
router3 :: Options -> Bool -> Bool -> IBlkID -> Doc
router3 opts is_init is_final k =
    let d1 = if is_init
            then PP.text "ADJUST_MARKER = true;"
            else PP.empty
        d2 = PP.text "MARKER += ADJUST_MARKER;"
        d3 = if is_final
            then PP.empty
            else PP.text "MARKER ++;"
        d4 = doc_goto_fin k opts
    in  case opts of
            Opts      Single _       _ _ (Just def_act) -> PP.text def_act $$ d4
            Opts      Single _       _ _ Nothing        -> PP.empty $$ d4
            Opts      Normal _       _ _ (Just _)       -> d4
            Opts      Normal All     _ _ Nothing        -> d1 $$ d2 $$ d4
            Opts      Normal Longest _ _ Nothing        -> d3 $$ d4
            OptsBlock _              _ _ (Just _)       -> d4
            OptsBlock _              _ _ Nothing        -> d3 $$ d4


router4 :: Options -> Bool -> (IRegID, SCode) -> Doc
router4 opts empty_node (n, code) =
    let d1 = PP.text code
        d2 = PP.text "ACCEPT = " <> PP.int n <> PP.semi
        d3 = PP.text "MARKER = CURSOR;"
        d4 = if empty_node then PP.empty else PP.text "ADJUST_MARKER = false;"
    in  case opts of
            Opts      Single Longest _ _ _ ->       d2
            Opts      Single All     _ _ _ -> d1
            Opts      Normal Longest _ _ _ ->       d2 $$ d3
            Opts      Normal All     _ _ _ -> d1       $$ d3 $$ d4
            OptsBlock _              _ _ _ ->       d2 $$ d3


router5 :: BlockInfo a -> Doc
router5 (BI k opts id_info _) = case opts of
    Opts Single _ _ _ df -> doc_decl_fin k opts $$ codegen_match_code id_info df
    _                    -> PP.empty


router6 :: Options -> Bool -> Doc
router6 _    True  = PP.empty
router6 opts False =
    case prelexer opts of
        (Just prelexer) -> PP.text prelexer <> PP.text " ();"
        Nothing         -> PP.text "CURSOR++;"


router7 :: Options -> Doc
router7 opts =
    case prelexer opts of
        (Just prelexer) -> PP.text prelexer <> PP.text " ()"
        Nothing         -> PP.text "*CURSOR++"


codegen_entry :: Int -> BlockInfo a -> PP.Doc
codegen_entry maxlen (BI k opts id_info _) =
    PP.text "MAXLEN = " <> PP.int maxlen <> PP.semi
    $$ router0 opts k id_info
    $$ doc_goto k 0


compare_by_label :: Labellable a => (Label a, b) -> (Label a, b) -> Ordering
compare_by_label (l1, _) (l2, _) = l1 `compare` l2


codegen_cases :: Labellable a => StateInfo a -> BlockInfo a -> PP.Doc
codegen_cases (SI _ is_init is_final node _) (BI k opts id_info _) =
    let case_body ids b s =
            let check_cyclic   = if b then codegen_refill else PP.empty
                conds          = get_conds id_info ids
                is_conditional = (null . fst . partition (== S.empty)) conds
                goto_m         = doc_goto k s
                goto_fin       = router2 opts $$ doc_goto_fin k opts
                body           = if is_conditional then doc_if_else conds goto_m goto_fin else goto_m
            in  check_cyclic $$ body
        one_case doc (l, (ids, b, s)) =
            doc
            $$ doc_case l
            $$ PP.nest 4
--                ( PP.text "printf (\"%c\\n\", *(CURSOR - 1));"
                ( router1 opts is_init is_final
                $$ case_body ids b s
                )
        node'        = (sortBy compare_by_label . M.toList) node
        cases        = foldl' one_case PP.empty node'
        default_case = doc_default (router3 opts is_init is_final k)
    in  cases $$ default_case


codegen_fstate :: Labellable a => StateInfo a -> BlockInfo a -> PP.Doc
codegen_fstate (SI _ _ _ node ids) (BI _ opts id_info _) =
    let id_conds_code = get_conds2code id_info ((S.singleton . S.findMin . fromJust) ids)
        f doc (id, conds, _, code) =
            let code'  = router4 opts (node == M.empty) (id, code)
                code'' = doc_if [conds] code'
            in  doc $$ PP.nest 4 code''
    in  foldl' f  PP.empty id_conds_code


codegen_state :: Labellable a => StateInfo a -> BlockInfo a -> PP.Doc
codegen_state si@(SI s is_init is_final node _) bi@(BI k opts _ ttbl) =
    let fstate = if is_final then codegen_fstate si bi else PP.empty
        d1     = router6 opts is_init
        d2     = router7 opts
        state  = case M.toList node of
            []                                             -> doc_goto_fin k opts
            [(LRange r, (_, b, s))] | is_full_range r ttbl -> d1 $$ if b then codegen_refill else PP.empty $$ doc_goto k s
            _                                              -> doc_switch d2 (codegen_cases si bi)
    in  doc_decl k s
        $$ fstate
        $$ PP.nest 4 state


