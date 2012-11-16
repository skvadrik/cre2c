module CFA2CPP
    ( cfa2cpp
    ) where


import qualified Data.HashMap.Strict       as M
import qualified Data.Set                  as S
import           Data.List                       (foldl', partition, nub)
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
    let entry      = codegen_entry maxlen n_scanner opts
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
        ending = router0 opts n_scanner
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
($$$) d1 d2 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


m_decl :: Int -> Int -> Doc
m_decl k n =
    PP.char 'm'
    <> PP.int k
    <> PP.char '_'
    <> PP.int n
    <> PP.colon


m_fin_decl :: Int -> Doc
m_fin_decl k =
    PP.char 'm'
    <> PP.int k
    <> PP.text "_fin:"


m_goto :: Int -> Int -> Doc
m_goto k n =
    PP.text "goto m"
    <> PP.int k
    <> PP.char '_'
    <> PP.int n
    <> PP.semi


m_fin_goto :: Int -> Doc
m_fin_goto k =
    PP.text "goto m"
    <> PP.int k
    <> PP.text "_fin;"


wrap_in_braces :: Doc -> Doc
wrap_in_braces d = PP.text "{" $+$ PP.nest 4 d $$ PP.text "}"


codegen_if :: [S.Set Cond] -> Doc -> Doc
codegen_if cs doc =
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


codegen_if_else :: [S.Set Cond] -> Doc -> Doc -> Doc
codegen_if_else cs d1 d2 =
    codegen_if cs d1
    $$ PP.text "else"
    $$ wrap_in_braces d2


codegen_switch :: Doc -> Doc -> Doc
codegen_switch d1 d2 = PP.text "switch " <> (PP.parens d1) $$ (wrap_in_braces d2)


codegen_default :: Doc -> Doc
codegen_default d = PP.text "default:" $$ PP.nest 4 d


get_conds2code :: RegexpId2RegexpInfo -> S.Set RegexpId -> [(S.Set Cond, Code)]
get_conds2code id_info =
    let f :: RegexpId -> [(S.Set Cond, Code)]
        f k = M.toList $ snd $ M.lookupDefault
            (error (printf "*** CFA2CPP : missing regexp info for regexp number %d" k))
            k
            id_info
    in  S.foldl' (\ conds k -> f k ++ conds) []


get_conds :: RegexpId2RegexpInfo -> S.Set RegexpId -> [S.Set Cond]
get_conds id_info = fst . unzip . get_conds2code id_info


router0 :: Options -> Int -> Doc
router0 opts k = case opts of
    Options Matcher _   _ -> m_fin_decl k
    _                     -> PP.empty


router1 :: Options -> Bool -> Bool -> Doc
router1 opts is_init is_final = case opts of
    Options Matcher _   _ -> PP.empty
    Options _       All _ -> case (is_init, is_final) of
        (True, True)  -> PP.text "token = adjust_marker ? MARKER : token;"
        (True, False) -> PP.text "adjust_marker = true;"
        _             -> PP.empty
    Options _       _   _ -> case (is_init, is_final) of
        (True, True)  -> PP.text "token = MARKER;"
        _             -> PP.empty


router2 :: Options -> Doc
router2 opts = case opts of
    Options Matcher _   _ -> PP.empty
    Options _       All _ -> PP.text "MARKER += adjust_marker;"
    _                     -> PP.text "MARKER ++;"


router3 :: Options -> Bool -> Doc
router3 opts is_init = case opts of
    Options Matcher _   _ -> PP.empty
    Options _       All _ -> (if is_init then PP.text "adjust_marker = true;" else PP.empty) $$ PP.text "MARKER += adjust_marker;"
    _                     -> PP.text "MARKER ++;"


router4 :: Options -> Bool -> Doc
router4 opts empty_node = case opts of
    Options Matcher _   _ -> PP.empty
    Options _       All _ -> PP.text "MARKER = CURSOR;" $$ (if empty_node then PP.empty else PP.text "adjust_marker = false;")
    _                     -> PP.text "MARKER = CURSOR;"


codegen_entry :: Int -> Int -> Options -> PP.Doc
codegen_entry maxlen k opts =
    PP.text "#define MAXLEN" <> PP.int k <> PP.space <> PP.int maxlen
    $$ case mode opts of
        Matcher -> PP.text "token = MARKER;"
        _       -> m_fin_decl k $$ PP.text "CURSOR = MARKER;"
    $$ PP.text "if (LIMIT - CURSOR < MAXLEN" <> PP.int k <> PP.text ") FILL();"
    $$ m_goto k 0


codegen_cases :: StateInfo -> BlockInfo -> PP.Doc
codegen_cases (SI _ is_init is_final node _) (BI k opts id_info) =
    let show_u8b    = printf "case 0x%02X:"
        case_head l = case l of
            LabelChar c  -> PP.text $ show_u8b c
            LabelRange r -> PP.vcat $ map (PP.text . show_u8b) r
        case_body ids s =
            let conds          = get_conds id_info ids
                is_conditional = (null . fst . partition (== S.empty)) conds
                goto_m         = m_goto k s
                goto_fin       = router2 opts $$ m_fin_goto k
            in  if is_conditional
                    then codegen_if_else conds goto_m goto_fin
                    else goto_m
        one_case doc l (ids, s) =
            doc
            $$ case_head l
            $$ PP.nest 4
                ( router1 opts is_init is_final
                $$ case_body ids s
                )
        cases        = M.foldlWithKey' one_case PP.empty node
        default_case = codegen_default (router3 opts is_init $$ m_fin_goto k)
    in  cases $$ default_case


codegen_fstate :: StateInfo -> BlockInfo -> PP.Doc
codegen_fstate (SI _ _ _ node ids) (BI _ opts id_info) =
    let conds2code = get_conds2code id_info (fromJust ids)
        f doc (conds, code) =
            let code'  = ( PP.text . BS.unpack ) code $$ router4 opts (node == M.empty)
                code'' = codegen_if [conds] code'
            in  doc $$ PP.nest 4 code''
    in  foldl' f  PP.empty conds2code


codegen_state :: StateInfo -> BlockInfo -> PP.Doc
codegen_state si@(SI s _ is_final node _) bi@(BI k _ _) =
    let is_full range = S.fromList range == S.fromList ['\x00' .. '\xFF']
        fstate = if is_final then codegen_fstate si bi else PP.empty
        state  = case M.toList node of
            []                                   -> m_fin_goto k
            [(LabelRange r, (_, s))] | is_full r -> PP.text "CURSOR++;" $$ m_goto k s
            _                                    -> codegen_switch (PP.text "*CURSOR++") (codegen_cases si bi)
    in  m_decl k s
        $$ fstate
        $$ PP.nest 4 state


