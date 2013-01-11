module RE2CFA
    ( re2ncfa
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')

import           Types                 hiding (err)
import           CFA


ncfa_add_regexp :: (Labellable a) => (NCFA a, S.Set IStateID, Int) -> Regexp a -> MRegname2Regexp a -> Maybe MTokname2TokID -> IRegID -> (NCFA a, S.Set IStateID, Int)
ncfa_add_regexp (ncfa, ss, l) (Regexp r) rt ttbl sign =
    let (ncfa', ss', l') = ncfa_add_regexp_alt (ncfa, ss, l) r rt ttbl sign
        ncfa''           = S.foldl' (\ c s -> ncfa_set_final s sign c) ncfa' ss'
    in  (ncfa'', ss', l')


ncfa_add_regexp_alt :: (Labellable a) => (NCFA a, S.Set IStateID, Int) -> RegexpAlt a -> MRegname2Regexp a -> Maybe MTokname2TokID -> IRegID -> (NCFA a, S.Set IStateID, Int)
ncfa_add_regexp_alt (ncfa, ss, l) r rt ttbl sign = case r of
    AltFromCat rcat -> ncfa_add_regexp_cat (ncfa, ss, l) rcat rt ttbl sign
    Alt rcat ralt   ->
        let (ncfa', ss', l')    = ncfa_add_regexp_cat (ncfa, ss, l) rcat rt ttbl sign
            (ncfa'', ss'', l'') = ncfa_add_regexp_alt (ncfa', ss, l) ralt rt ttbl sign
        in  (ncfa'', S.union ss' ss'', max l' l'')


ncfa_add_regexp_cat :: (Labellable a) => (NCFA a, S.Set IStateID, Int) -> RegexpCat a -> MRegname2Regexp a -> Maybe MTokname2TokID -> IRegID -> (NCFA a, S.Set IStateID, Int)
ncfa_add_regexp_cat (ncfa, ss, l) r rt ttbl sign = case r of
    CatFromIter riter -> ncfa_add_regexp_iter (ncfa, ss, l) riter rt ttbl sign
    Cat riter rcat    -> ncfa_add_regexp_cat (ncfa_add_regexp_iter (ncfa, ss, l) riter rt ttbl sign) rcat rt ttbl sign


ncfa_add_regexp_iter :: (Labellable a) => (NCFA a, S.Set IStateID, Int) -> RegexpIter a -> MRegname2Regexp a -> Maybe MTokname2TokID -> IRegID -> (NCFA a, S.Set IStateID, Int)
ncfa_add_regexp_iter (ncfa, ss, l) r rt ttbl sign = case r of
    IterFromPrim rprim  -> ncfa_add_regexp_prim (ncfa, ss, l) rprim rt ttbl sign
    -- non-determinism of NCFA allows to build a small NCFA subgraph and merge it to the whole NCFA graph painlessly
    -- so that you can iterate only on subgraph when tying states
    IterZeroMany rprim  ->
        let (ncfa', ss', l') = ncfa_add_regexp_prim (ncfa, ss, l) rprim rt ttbl sign
            (ncfa'', l'')    = S.foldl'
                (\ (ncfa1, _) s ->
                    let (ncfa2, ss2, l2) = ncfa_add_regexp_prim (ncfa1, S.insert s S.empty, l) rprim rt ttbl sign
                        ncfa3            = ncfa_tie_states ncfa2 (S.delete s ss2) s
                        ncfa4            = ncfa_set_cyclic s ncfa3
                    in  (ncfa4, l2)
                )
                (new_ncfa (ncfa_max_state ncfa'), l')
                ss'
        in  (ncfa_union ncfa' ncfa'', S.union ss ss', l'')
    IterOneMany rprim   ->
        let (ncfa', ss', l') = ncfa_add_regexp_prim (ncfa, ss, l) rprim rt ttbl sign
            (ncfa'', l'')    = S.foldl'
                (\ (ncfa1, _) s ->
                    let (ncfa2, ss2, l2) = ncfa_add_regexp_prim (ncfa1, S.insert s S.empty, l) rprim rt ttbl sign
                        ncfa3            = ncfa_tie_states ncfa2 (S.delete s ss2) s
                        ncfa4            = ncfa_set_cyclic s ncfa3
                    in  (ncfa4, l2)
                )
                (new_ncfa (ncfa_max_state ncfa'), l')
                ss'
        in  (ncfa_union ncfa' ncfa'', ss', l'')
    IterMaybe rprim     ->
        let (ncfa', ss', l') = ncfa_add_regexp_prim (ncfa, ss, l) rprim rt ttbl sign
        in  (ncfa', S.union ss ss', l')
    IterRepeat rprim n  ->
        let rcat = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n - 1]
        in  ncfa_add_regexp_cat (ncfa, ss, l) rcat rt ttbl sign
    IterRange rprim n m
        | m == 0         -> (ncfa, ss, l)
        | n < 0 || m < n -> err $ "ncfa_add_regexp_iter : Invalid iteration bounds in regexp: {" ++ show n ++ "," ++ show m ++ "}"
        | otherwise      ->
        let rcat               = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n - 1]
            (ncfa', ss', l')   = ncfa_add_regexp_cat (ncfa, ss, l) rcat rt ttbl sign
            ss''               = if n == 0 then S.union ss ss' else ss'
            (ncfa'', _, ss''', l'') = foldl'
                (\ (ncfa, ss, xs, l) _ ->
                    let (ncfa', ss', l') = ncfa_add_regexp_prim (ncfa, ss, l) rprim rt ttbl sign
                    in  (ncfa', ss', S.union ss' xs, l')
                ) (ncfa', ss', ss'', l') [n .. m - 2]
        in  (ncfa'', ss''', l'')


ncfa_add_regexp_prim :: (Labellable a) => (NCFA a, S.Set IStateID, Int) -> RegexpPrim a -> MRegname2Regexp a -> Maybe MTokname2TokID -> IRegID -> (NCFA a, S.Set IStateID, Int)
ncfa_add_regexp_prim (ncfa, ss, l) r rt ttbl sign = case r of
    Elementary s -> foldl' (\(d, s, l) c -> ncfa_add_regexp_atom (d, s, l) (LOne c) sign) (ncfa, ss, l) s
    Name s       ->
        let Regexp ralt = M.lookupDefault (err ("ncfa_add_regexp_prim : undefined regexp: " ++ show s)) s rt
        in  ncfa_add_regexp_alt (ncfa, ss, l) ralt rt ttbl sign
    Wrapped ralt -> ncfa_add_regexp_alt (ncfa, ss, l) ralt rt ttbl sign
    Any          -> ncfa_add_regexp_atom (ncfa, ss, l) (LRange (full_range ttbl)) sign
    Range s      -> ncfa_add_regexp_atom (ncfa, ss, l) (LRange s) sign


ncfa_add_regexp_atom :: (NCFA a, S.Set IStateID, Int) -> Label a -> IRegID -> (NCFA a, S.Set IStateID, Int)
ncfa_add_regexp_atom (ncfa, ss, l) lbl sign =
    let s_max        = ncfa_max_state ncfa
        (ncfa', ss') = S.foldl'
            (\ (ncfa', ss') s ->
                     let (ncfa'', s') = ncfa_add_transition ncfa' (s, lbl, sign, s_max)
                     in  (ncfa'', S.insert s' ss')
            ) (ncfa, S.empty) ss
    in  (ncfa', ss', l + 1)


err :: String -> a
err s = error $ "*** RE2CFA : " ++ s


re2ncfa :: (Labellable a) => M.HashMap IRegID SRegname -> MRegname2Regexp a -> Maybe MTokname2TokID -> (NCFA a, Int)
re2ncfa rs rt ttbl =
    let ncfa          = new_ncfa 0
        (ncfa', _, l) = M.foldlWithKey'
            (\ (ncfa, ss, l) k r ->
                let regexp         = M.lookupDefault (err ("re2ncfa : undefined regexp: " ++ show r)) r rt
                    (ncfa', _, l') = ncfa_add_regexp (ncfa, ss, 0) regexp rt ttbl k
                in  (ncfa', ss, max l l')
            )
            (ncfa, S.insert (ncfa_init_state ncfa) S.empty, 0)
            rs
    in  (ncfa', l)
