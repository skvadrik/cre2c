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


type Conds2Code = M.HashMap (S.Set Cond) Code
type RegexpId2RegexpInfo = M.HashMap RegexpId (Maybe BlockName, Conds2Code)
data StateInfo = SI State Bool Bool DCFANode (Maybe (S.Set RegexpId))
data BlockInfo = BI Int Options RegexpId2RegexpInfo

{-
data StateInfo = SI
    { state      :: State
    , is_init    :: Bool
    , is_final   :: Bool
    , node       :: DCFANode
    , regexp_ids :: Maybe (S.Set RegexpId)
    }


data BlockInfo = BI
    { number  :: Int
    , options :: Options
    , id_info :: RegexpId2RegexpInfo
    }
-}

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
    in  (BS.pack . PP.render)
            ( ( PP.text . BS.unpack ) prolog
            $$$ entry
            $$$ states
            $$$ final_states
            $$$ case mode opts of
                Matcher -> m_fin_decl n_scanner
                _       -> PP.empty )


instance Eq Doc where
    d1 == d2 = PP.render d1 == PP.render d2


($$$) :: Doc -> Doc -> Doc
($$$) d1 d2 = d1 $$ PP.text "" $$ d2
infixl 5 $$$


wrap_in_braces :: Doc -> Doc
wrap_in_braces d = PP.text "{" $+$ PP.nest 4 d $$ PP.text "}"


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


codegen_entry :: Int -> Int -> Options -> PP.Doc
codegen_entry maxlen k opts =
    PP.text "#define MAXLEN" <> PP.int k <> PP.space <> PP.int maxlen
    $$ case mode opts of
        Matcher -> PP.text "token = MARKER;"
        _       -> m_fin_decl k $$ PP.text "CURSOR = MARKER;"
    $$ PP.text "if (LIMIT - CURSOR < MAXLEN" <> PP.int k <> PP.text ") FILL();"
    $$ m_goto k 0


if_satisfy :: [S.Set Cond] -> Doc -> Doc
if_satisfy cs doc =
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


codegen_case_alternatives :: Label -> Doc
codegen_case_alternatives l = case l of
    LabelChar c  -> PP.text $ printf "case 0x%X:" c
    LabelRange r -> PP.vcat $ map (PP.text . printf "case 0x%X:") r


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


maybe_adjust_marker1 :: Match -> Bool -> Bool -> PP.Doc
maybe_adjust_marker1 match is_init is_final = case match of
    All -> if is_init
        then if is_final
            then PP.text "token = adjust_marker ? MARKER : token;"
            else PP.text "adjust_marker = true;"
        else PP.empty
    _   -> PP.empty


maybe_adjust_marker2 :: Options -> PP.Doc
maybe_adjust_marker2 opts = case opts of
    Options Matcher _   _ -> PP.empty
    Options _       All _ -> PP.text "MARKER += adjust_marker;"
    _                     -> PP.empty


maybe_adjust_marker3 :: Match -> Bool -> PP.Doc
maybe_adjust_marker3 match is_init = case match of
    All -> if is_init then PP.text "adjust_marker = true;" else PP.empty
        $$ PP.text "MARKER += adjust_marker;"
    _   -> PP.empty


maybe_adjust_marker4 :: Match -> Bool -> PP.Doc
maybe_adjust_marker4 match not_empty_node = case match of
    All ->
        PP.text "MARKER = CURSOR;"
        $$ ( if not_empty_node then PP.empty else PP.text "adjust_marker = false;")
    _   -> PP.empty


codegen_case :: StateInfo -> BlockInfo -> PP.Doc
codegen_case (SI _ is_init is_final node _) (BI k opts id_info) =
    let code' = M.foldlWithKey'
            (\ doc l (ks, s) -> doc
                $$ codegen_case_alternatives l
                $$ PP.nest 4 (maybe_adjust_marker1 (match opts) is_init is_final)
                $$ PP.nest 4
                    ( case partition (== S.empty) (get_conds id_info ks) of
                        ( conds', conds'' ) | conds' == [] ->
                            if_satisfy conds'' (m_goto k s)
                            $$ PP.text "else"
                            $$ wrap_in_braces
                                ( maybe_adjust_marker2 opts
                                $$ m_fin_goto k
                                )
                        _ -> m_goto k s
                    )
            ) PP.empty node
    in  code'
        $$ PP.text "default:"
        $$ PP.nest 4
            ( maybe_adjust_marker3 (match opts) is_init
            $$ m_fin_goto k
            )


codegen_fstate :: StateInfo -> BlockInfo -> PP.Doc
codegen_fstate (SI _ _ _ node ids) (BI _ opts id_info) =
    let conds2code = get_conds2code id_info (fromJust ids)
        f doc (conds, code) =
            let code'  = ( PP.text . BS.unpack ) code $$ maybe_adjust_marker4 (match opts) (node /= M.empty)
                code'' = if_satisfy [conds] code'
            in  doc $$ PP.nest 4 code''
    in  foldl' f  PP.empty conds2code


codegen_state :: StateInfo -> BlockInfo -> PP.Doc
codegen_state si@(SI s _ is_final node _) bi@(BI k _ _) =
    m_decl k s
    $$ ( if not is_final then PP.empty else codegen_fstate si bi)
    $$ ( PP.nest 4 $ case M.toList node of
        [] -> m_fin_goto k
        [(LabelRange r, (_, s))] | S.fromList r == S.fromList ['\x00' .. '\xFF'] ->
            PP.text "CURSOR++;"
            $$ m_goto k s
        _ ->
            PP.text "switch (*CURSOR++)"
            $$ ( wrap_in_braces $ codegen_case si bi ) )

