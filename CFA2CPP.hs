module CFA2CPP
    ( cfa2cpp
    ) where


import           Data.Function                         (on)
import qualified Data.HashMap.Strict       as M hiding (lookupDefault)
import qualified Data.HashMap.Lazy         as M        (lookupDefault)
import qualified Data.Set                  as S
import           Data.List                             (foldl', partition, nub, sortBy)
import           Text.PrettyPrint.HughesPJ             (($$), (<>), Doc)
import qualified Text.PrettyPrint.HughesPJ as PP
import           Data.Maybe                            (fromJust)

import           Types                          hiding (err)
import           Helpers
import           CFA


cfa2cpp :: Labellable a => DCFA a -> SCode -> BlockInfo a -> SCode
cfa2cpp dcfa prolog bi =
    let entry        = codegen_entry bi
        g            = dcfa_graph dcfa
        s0           = dcfa_init_state dcfa
        finnable     = reachable_from_final dcfa
        states       = foldl'
            (\ code (s, node) ->
                let is_init     = s == s0
                    is_final    = dcfa_is_final s dcfa
                    si          = SI s is_init False node Nothing
                    is_finnable = S.member s finnable
                in  code $$$ if is_final
                        then PP.empty
                        else codegen_state si bi is_finnable
            ) PP.empty (sortBy (compare `on` fst) $ M.toList g)
        final_states = foldl'
            (\ code (s, accepted) ->
                let is_init  = s == s0
                    node     = M.lookupDefault M.empty s g
                    si       = SI s is_init True node (Just accepted)
                in  code $$$ codegen_state si bi True
            ) PP.empty (sortBy (compare `on` fst) $ M.toList $ dcfa_final_states dcfa)
    in  trace' (M.size g, S.size finnable) `seq` PP.render $
            (PP.text prolog)
            $$$ entry
            $$$ states
            $$$ final_states


reachable_from_final :: Labellable a => DCFA a -> S.Set IStateID
reachable_from_final (DCFA _ _ g fss) =
    let f1 ss s =
            let ss' =
                    ( S.fromList
                    . (\ (_, _, x) -> x)
                    . unzip3
                    . M.elems
                    . M.lookupDefault M.empty s
                    ) g
            in  S.union ss ss'

        f2 closed open | open == S.empty = closed
        f2 closed open =
            let closed' = S.union closed open
                open'   = S.foldl' f1 S.empty open
                open''  = S.difference open' closed'
            in  f2  closed' open''

    in  f2 S.empty (S.fromList (M.keys fss))


doc_decl_ :: Doc -> Doc -> Doc
doc_decl_ d1 d2 = PP.text "m_" <> d1 <> PP.char '_' <> d2 <> PP.colon


doc_goto_ :: Doc -> Doc -> Doc
doc_goto_ d1 d2 = PP.text "goto m_" <> d1 <> PP.char '_' <> d2 <> PP.semi


doc_decl :: SBlkname -> IStateID -> Doc
doc_decl block n =
    let d1 = PP.text block
        d2 = PP.int n
    in  doc_decl_ d1 d2


doc_goto :: SBlkname -> IStateID -> Doc
doc_goto block n =
    let d1 = PP.text block
        d2 = PP.int n
    in  doc_goto_ d1 d2


doc_decl_fin :: SBlkname -> Doc
doc_decl_fin block =
    let d1 = PP.text block
        d2 = PP.text "fin"
    in  doc_decl_ d1 d2


doc_goto_fin :: SBlkname -> Doc
doc_goto_fin block =
    let d1 = PP.text block
        d2 = PP.text "fin"
    in  doc_goto_ d1 d2


doc_decl_start :: SBlkname -> Doc
doc_decl_start block =
    let d1 = PP.text block
        d2 = PP.text "start"
    in  doc_decl_ d1 d2


doc_goto_start :: SBlkname -> Doc
doc_goto_start block =
    let d1 = PP.text block
        d2 = PP.text "start"
    in  doc_goto_ d1 d2


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


doc_case_clause_for_label :: Labellable a => Label a -> Doc
doc_case_clause_for_label l =
    let f x = PP.text "case " <> (PP.text . show_hex) x <> PP.colon
    in  case l of
            LOne x    -> f x
            LRange xs -> (PP.vcat . map f . S.toList) xs


doc_case :: IRegID -> Doc -> Doc
doc_case n d =
    PP.text "case " <> PP.int n <> PP.colon
    $$ PP.nest 4 d


doc_default :: Doc -> Doc
doc_default d = PP.text "default:" $$ PP.nest 4 d


get_conds2code :: MRegID2RegInfo -> S.Set IRegID -> [(IRegID, S.Set SCond, SBlkname, SCode)]
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
            let d0 = PP.text "ACCEPT = -1;"
                d1 = doc_if [conds] (PP.text code)
                d2 = doc_goto_start block
            in  d $$ (doc_case id (d0 $$ d1 $$ d2))
        d1 = foldl' f PP.empty id_info'
        d2 = case df of
            Just code -> doc_default $ PP.text code
            Nothing   -> PP.empty
    in  doc_switch (PP.text "ACCEPT") (d1 $$ d2)


codegen_refill :: Int -> Doc
codegen_refill maxlen = PP.text "if (LIMIT - CURSOR < " <> PP.int maxlen <> PP.text ") FILL();"


-- suppose init state cannot be final
router3 :: Options -> Bool -> Bool -> Bool -> Doc
router3 opts is_init is_final is_finnable =
    let d3 = if is_final
            then PP.text "MARKER = CURSOR;"
            else if is_init
                then PP.text "MARKER = ++ CURSOR;"
                else PP.text "CURSOR = MARKER;"
        d4 = if is_finnable
            then doc_goto_fin (block_name opts)
            else doc_goto_start (block_name opts)
    in  case opts of
            Options _ _ (Just def_act) -> d3 $$ PP.text def_act
            Options _ _ Nothing        -> d3 $$ d4


codegen_entry :: BlockInfo a -> PP.Doc
codegen_entry (BI _ maxlen opts id_info _) =
    PP.text "MAXLEN = " <> PP.int maxlen <> PP.semi
    $$ doc_goto_start (block_name opts)
    $$$ doc_decl_fin (block_name opts)
    $$ PP.text "++ CURSOR;"
    $$ codegen_match_code id_info (default_action opts)
    $$$ doc_decl_start (block_name opts)
    $$ codegen_refill maxlen


compare_by_label :: Labellable a => (Label a, b) -> (Label a, b) -> Ordering
compare_by_label (l1, _) (l2, _) = l1 `compare` l2


codegen_cases :: Labellable a => StateInfo a -> BlockInfo a -> Bool -> PP.Doc
codegen_cases (SI _ is_init is_final node _) (BI _ maxlen opts id_info _) is_finnable =
    let case_body ids b s =
            let check_cyclic   = if b then codegen_refill maxlen else PP.empty
                conds          = get_conds id_info ids
                is_conditional = (null . fst . partition (== S.empty)) conds
                inc            = if is_init then PP.text "++ MARKER;" else PP.empty
                goto_m         = inc $$ doc_goto (block_name opts) s
                goto_fin       = inc $$ PP.text "CURSOR = MARKER;" $$ doc_goto_fin (block_name opts)
                body           = if is_conditional then doc_if_else conds goto_m goto_fin else goto_m
            in  check_cyclic $$ body
        one_case doc (l, (ids, b, s)) =
            doc
            $$ doc_case_clause_for_label l
            $$ PP.nest 4
                ( (if is_init then PP.text "TOKEN = MARKER;" else PP.empty)
                $$ case_body ids b s
                )
        node'        = (sortBy compare_by_label . M.toList) node
        cases        = foldl' one_case PP.empty node'
        default_case = doc_default (router3 opts is_init is_final is_finnable)
    in  cases $$ default_case


codegen_fstate :: Labellable a => StateInfo a -> BlockInfo a -> PP.Doc
codegen_fstate (SI _ _ _ _ ids) (BI _ _ _ id_info _) =
    let id_conds_code = get_conds2code id_info ((S.singleton . S.findMin . fromJust) ids)
        f doc (id, conds, _, _) =
            let code'  =
                    PP.text "ACCEPT = " <> PP.int id <> PP.semi
                    $$ PP.text "MARKER = CURSOR;"
                code'' = if conds /= S.empty
                    then doc_if [conds] code'
                    else code'
            in  doc $$ PP.nest 4 code''
    in  foldl' f  PP.empty id_conds_code


codegen_state :: Labellable a => StateInfo a -> BlockInfo a -> Bool -> PP.Doc
codegen_state si@(SI s is_init is_final node _) bi@(BI _ maxlen opts _ ttbl) is_finnable =
    let fstate = if is_final then codegen_fstate si bi else PP.empty
        d0     = if is_init then PP.empty else doc_decl (block_name opts) s
        d1     = if is_init then PP.empty else PP.text "++ CURSOR;"
        d2     = if is_init then PP.text "* CURSOR" else PP.text "* (++ CURSOR)"
        state  = case M.toList node of
            []                                             -> doc_goto_fin (block_name opts)
            [(LRange r, (_, b, s))] | is_full_range r ttbl -> d1 $$ if b then codegen_refill maxlen else PP.empty $$ doc_goto (block_name opts) s
            _                                              -> doc_switch d2 (codegen_cases si bi is_finnable)
    in  d0
        $$ fstate
        $$ PP.nest 4 state


