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

import           Types
import           CFA


cfa2cpp :: DCFA -> Code -> [M.HashMap (S.Set Cond) Code] -> Int -> Int -> Options -> Code
cfa2cpp dcfa prolog conds2code maxlen n_scanner opts =
    let entry      = codegen_entry maxlen n_scanner opts
        g          = dcfa_graph dcfa
        s0         = dcfa_init_state dcfa
        states     = M.foldlWithKey'
            (\ code s node -> code
                $$$ case s of
                    s | dcfa_is_final s dcfa -> PP.empty
                    s                        -> codegen_state s (s == s0) False node conds2code S.empty n_scanner opts
            ) PP.empty g
        final_states = M.foldlWithKey'
            (\ code s accepted -> code
                $$$ codegen_state s (s == s0) True (M.lookupDefault M.empty s g) conds2code accepted n_scanner opts
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


codegen_conditions :: [[Cond]] -> Doc
codegen_conditions =
    let f :: String -> [Doc] -> Doc
        f s xs = case nub xs of
            []   -> PP.empty
            [x]  -> x
            xs'  ->
                ( PP.parens
                . PP.hcat
                . PP.punctuate (PP.text s)
                ) xs'
    in  f " || " . map (f " && " . map PP.text)


codegen_case_alternatives :: Label -> Doc
codegen_case_alternatives l = case l of
    LabelChar c  -> PP.text $ printf "case 0x%X:" c
    LabelRange r -> PP.vcat $ map (PP.text . printf "case 0x%X:") r


codegen_case_matcher :: DCFANode -> [M.HashMap (S.Set Cond) Code] -> Int -> PP.Doc
codegen_case_matcher node conds2code k =
    let get_conds = S.foldl' (\ conds k -> (map S.toList . M.keys) (conds2code !! k) ++ conds) []
        code' = M.foldlWithKey'
            (\ doc l (ks, s) -> doc
                $$ codegen_case_alternatives l
                $$ PP.nest 4
                    ( case partition (== []) (get_conds ks) of
                        ( conds', conds'' ) | conds' == [] ->
                            PP.text "if "
                            <> ( PP.parens . codegen_conditions) conds''
                            $$ PP.nest 4 ( m_goto k s )
                            $$ PP.text "else"
                            $$ PP.nest 4 ( m_fin_goto k )
                        _ -> m_goto k s
                    )
            ) PP.empty node
    in  code'
        $$ PP.text "default:"
        $$ PP.nest 4 ( m_fin_goto k )


codegen_case_scanner :: Bool -> Bool -> DCFANode -> [M.HashMap (S.Set Cond) Code] -> Int -> PP.Doc
codegen_case_scanner is_init is_final node conds2code k =
    let get_conds = S.foldl' (\ conds k -> (map S.toList . M.keys) (conds2code !! k) ++ conds) []
        code' = M.foldlWithKey'
            (\ doc l (ks, s) -> doc
                $$ codegen_case_alternatives l
                $$ PP.nest 4
                    ( if is_init
                        then if is_final
                            then PP.text "token = adjust_marker ? MARKER : token;"
                            else PP.text "adjust_marker = true;"
                        else PP.empty
                    )
                $$ PP.nest 4
                    ( case partition (== []) (get_conds ks) of
                        ( conds', conds'' ) | conds' == [] ->
                            PP.text "if "
                            <> ( PP.parens . codegen_conditions) conds''
                            $$ PP.nest 4 ( m_goto k s )
                            $$ PP.text "else"
                            $$ wrap_in_braces
                                ( PP.text "MARKER += adjust_marker;"
                                $$ m_fin_goto k
                                )
                        _ -> m_goto k s
                    )
            ) PP.empty node
    in  code'
        $$ PP.text "default:"
        $$ PP.nest 4
            ( if is_init then PP.text "adjust_marker = true;" else PP.empty
            $$ PP.text "MARKER += adjust_marker;"
            $$ m_fin_goto k
            )


codegen_state :: State -> Bool -> Bool -> DCFANode -> [M.HashMap (S.Set Cond) Code] -> S.Set Id -> Int -> Options -> PP.Doc
codegen_state s is_init is_final node conds2code signs k opts =
    m_decl k s
    $$ ( if not is_final then PP.empty else
        let conds2code' = S.foldl' (\ conds k -> M.toList (conds2code !! k) ++ conds) [] signs
        in  case mode opts of
                Matcher   -> codegen_fstate_matcher   conds2code' (node == M.empty)
                Scanner   -> codegen_fstate_scanner   conds2code' (node == M.empty)
                Tokenizer -> codegen_fstate_tokenizer conds2code' (node == M.empty) )
    $$ ( PP.nest 4 $ case M.toList node of
        [] -> m_fin_goto k
        [(LabelRange r, (_, s))] | S.fromList r == S.fromList ['\x00' .. '\xFF'] ->
            PP.text "CURSOR++;"
            $$ m_goto k s
        _ ->
            PP.text "switch (*CURSOR++)"
            $$ ( wrap_in_braces $ case mode opts of
                Matcher -> codegen_case_matcher node conds2code k
                _       -> codegen_case_scanner is_init is_final node conds2code k ) )


codegen_fstate_matcher :: [(S.Set Cond, Code)] -> Bool -> Doc
codegen_fstate_matcher conds2code is_empty_node =
    let doc_conds = PP.parens . codegen_conditions
        doc_code  = PP.nest 4 . PP.text . BS.unpack
        f doc (conds, code) = doc $$ PP.nest 4
            ( if not is_empty_node && conds /= S.empty
                then
                    PP.text "if "
                    <> doc_conds [S.toList conds]
                    $$ doc_code code
                else ( PP.text . BS.unpack ) code
            )
    in  foldl' f  PP.empty conds2code


codegen_fstate_scanner :: [(S.Set Cond, Code)] -> Bool -> Doc
codegen_fstate_scanner conds2code is_empty_node =
    let doc_conds = PP.parens . codegen_conditions
        doc_code  = PP.text . BS.unpack
        f doc (conds, code) = doc $$ PP.nest 4
            ( if not is_empty_node && conds /= S.empty
                then
                    PP.text "if "
                    <> doc_conds [S.toList conds]
                    $$ wrap_in_braces
                        ( doc_code code
                        $$ PP.text "MARKER = CURSOR;"
                        $$ PP.text "adjust_marker = false;"
                        )
                else
                    ( PP.text . BS.unpack ) code
                    $$ PP.text "MARKER = CURSOR;"
                    $$ if is_empty_node then PP.empty else PP.text "adjust_marker = false;"
            )
    in  foldl' f  PP.empty conds2code


codegen_fstate_tokenizer :: [(S.Set Cond, Code)] -> Bool -> Doc
codegen_fstate_tokenizer conds2code is_empty_node =
    let doc_conds = PP.parens . codegen_conditions
        doc_code  = PP.text . BS.unpack
        f doc (conds, code) = doc $$ PP.nest 4
            ( if not is_empty_node && conds /= S.empty
                then
                    PP.text "if "
                    <> doc_conds [S.toList conds]
                    $$ wrap_in_braces
                        ( doc_code code
                        $$ PP.text "MARKER = CURSOR;"
                        $$ PP.text "adjust_marker = false;"
                        )
                else
                    ( PP.text . BS.unpack ) code
                    $$ PP.text "MARKER = CURSOR;"
                    $$ if is_empty_node then PP.empty else PP.text "adjust_marker = false;"
            )
    in  foldl' f  PP.empty conds2code

