module RE2CFA
    ( re2ncfa
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')

import           Types
import           CFA


ncfa_add_regexp :: (Labellable a) => (NCFA a, S.Set State, Int) -> Regexp a -> RegexpTable a -> RegexpId -> (NCFA a, S.Set State, Int)
ncfa_add_regexp (ncfa, ss, l) (Regexp r) rt sign =
    let (ncfa', ss', l') = ncfa_add_regexp_alt (ncfa, ss, l) r rt sign
        ncfa''           = S.foldl' (\ c s -> ncfa_set_final s sign c) ncfa' ss'
    in  (ncfa'', ss', l')


ncfa_add_regexp_alt :: (Labellable a) => (NCFA a, S.Set State, Int) -> RegexpAlt a -> RegexpTable a -> RegexpId -> (NCFA a, S.Set State, Int)
ncfa_add_regexp_alt (ncfa, ss, l) r rt sign = case r of
    AltFromCat rcat -> ncfa_add_regexp_cat (ncfa, ss, l) rcat rt sign
    Alt rcat ralt   ->
        let (ncfa', ss', l')    = ncfa_add_regexp_cat (ncfa, ss, l) rcat rt sign
            (ncfa'', ss'', l'') = ncfa_add_regexp_alt (ncfa', ss, l) ralt rt sign
        in  (ncfa'', S.union ss' ss'', max l' l'')


ncfa_add_regexp_cat :: (Labellable a) => (NCFA a, S.Set State, Int) -> RegexpCat a -> RegexpTable a -> RegexpId -> (NCFA a, S.Set State, Int)
ncfa_add_regexp_cat (ncfa, ss, l) r rt sign = case r of
    CatFromIter riter -> ncfa_add_regexp_iter (ncfa, ss, l) riter rt sign
    Cat riter rcat    -> ncfa_add_regexp_cat (ncfa_add_regexp_iter (ncfa, ss, l) riter rt sign) rcat rt sign


ncfa_add_regexp_iter :: (Labellable a) => (NCFA a, S.Set State, Int) -> RegexpIter a -> RegexpTable a -> RegexpId -> (NCFA a, S.Set State, Int)
ncfa_add_regexp_iter (ncfa, ss, l) r rt sign = case r of
    IterFromPrim rprim  -> ncfa_add_regexp_prim (ncfa, ss, l) rprim rt sign
    IterMaybe rprim     ->
        let (ncfa', ss', l') = ncfa_add_regexp_prim (ncfa, ss, l) rprim rt sign
        in  (ncfa', S.union ss ss', l')
    IterRepeat rprim n  ->
        let rcat = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n - 1]
        in  ncfa_add_regexp_cat (ncfa, ss, l) rcat rt sign
    IterRange rprim n m
        | m == 0         -> (ncfa, ss, l)
        | n < 0 || m < n -> error $ "RE2CFA ERROR: Invalid iteration bounds in regexp: {" ++ show n ++ "," ++ show m ++ "}"
        | otherwise      ->
        let rcat               = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n - 1]
            (ncfa', ss', l')   = ncfa_add_regexp_cat (ncfa, ss, l) rcat rt sign
            ss''               = if n == 0 then S.union ss ss' else ss'
            (ncfa'', _, ss''', l'') = foldl'
                (\ (ncfa, ss, xs, l) _ ->
                    let (ncfa', ss', l') = ncfa_add_regexp_prim (ncfa, ss, l) rprim rt sign
                    in  (ncfa', ss', S.union ss' xs, l')
                ) (ncfa', ss', ss'', l') [n .. m - 2]
        in  (ncfa'', ss''', l'')


ncfa_add_regexp_prim :: (Labellable a) => (NCFA a, S.Set State, Int) -> RegexpPrim a -> RegexpTable a -> RegexpId -> (NCFA a, S.Set State, Int)
ncfa_add_regexp_prim (ncfa, ss, l) r rt sign = case r of
    Elementary s -> foldl' (\(d, s, l) c -> ncfa_add_regexp_atom (d, s, l) (LOne c) sign) (ncfa, ss, l) s
    Name s       ->
        let Regexp ralt = M.lookupDefault (error ("undefined regexp: " ++ show r)) s rt
        in  ncfa_add_regexp_alt (ncfa, ss, l) ralt rt sign
    Wrapped ralt -> ncfa_add_regexp_alt (ncfa, ss, l) ralt rt sign
    Any          -> ncfa_add_regexp_atom (ncfa, ss, l) (LRange full_range) sign
    Range s      -> ncfa_add_regexp_atom (ncfa, ss, l) (LRange s) sign


ncfa_add_regexp_atom :: (NCFA a, S.Set State, Int) -> Label a -> RegexpId -> (NCFA a, S.Set State, Int)
ncfa_add_regexp_atom (ncfa, ss, l) lbl sign =
    let s_max        = ncfa_max_state ncfa
        (ncfa', ss') = S.foldl'
            (\ (ncfa', ss') s ->
                     let (ncfa'', s') = ncfa_add_transition ncfa' (s, lbl, sign, s_max)
                     in  (ncfa'', S.insert s' ss')
            ) (ncfa, S.empty) ss
    in  (ncfa', ss', l + 1)


re2ncfa :: (Labellable a) => [RegexpName] -> RegexpTable a -> (NCFA a, Int)
re2ncfa rs rt =
    let ncfa          = ncfa_empty
        (ncfa', _, l) = foldl'
            (\ (ncfa, ss, l) (k, r) ->
                let regexp         = M.lookupDefault (error ("undefined regexp: " ++ show r)) r rt
                    (ncfa', _, l') = ncfa_add_regexp (ncfa, ss, 0) regexp rt k
                in  (ncfa', ss, max l l')
            )
            (ncfa, S.insert (ncfa_init_state ncfa) S.empty, 0)
            (zip [0 .. length rs - 1] rs)
    in  (ncfa', l)
