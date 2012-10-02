module RE2CFA
    ( re2ncfa
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')

import           Types
import           CFA


ncfa_add_regexp :: (NCFA, S.Set State) -> Regexp -> RegexpTable -> SignNum -> (NCFA, S.Set State)
ncfa_add_regexp (ncfa, ss) (Regexp r) rt sign =
    let (ncfa', ss') = ncfa_add_regexp_alt (ncfa, ss) r rt sign
        ncfa''       = S.foldl (\ c s -> setFinal s sign c) ncfa' ss'
    in  (ncfa'', ss')


ncfa_add_regexp_alt :: (NCFA, S.Set State) -> RegexpAlt -> RegexpTable -> SignNum -> (NCFA, S.Set State)
ncfa_add_regexp_alt (ncfa, ss) r rt sign = case r of
    AltFromCat rcat -> ncfa_add_regexp_cat (ncfa, ss) rcat rt sign
    Alt rcat ralt   ->
        let (ncfa', ss')   = ncfa_add_regexp_cat (ncfa, ss) rcat rt sign
            (ncfa'', ss'') = ncfa_add_regexp_alt (ncfa', ss) ralt rt sign
        in  (ncfa'', S.union ss' ss'')


ncfa_add_regexp_cat :: (NCFA, S.Set State) -> RegexpCat -> RegexpTable -> SignNum -> (NCFA, S.Set State)
ncfa_add_regexp_cat (ncfa, ss) r rt sign = case r of
    CatFromIter riter -> ncfa_add_regexp_iter (ncfa, ss) riter rt sign
    Cat riter rcat    -> ncfa_add_regexp_cat (ncfa_add_regexp_iter (ncfa, ss) riter rt sign) rcat rt sign


ncfa_add_regexp_iter :: (NCFA, S.Set State) -> RegexpIter -> RegexpTable -> SignNum -> (NCFA, S.Set State)
ncfa_add_regexp_iter (ncfa, ss) r rt sign = case r of
    IterFromPrim rprim  -> ncfa_add_regexp_prim (ncfa, ss) rprim rt sign
    IterMaybe rprim     ->
        let (ncfa', ss') = ncfa_add_regexp_prim (ncfa, ss) rprim rt sign
        in  (ncfa', S.union ss ss')
    IterRepeat rprim n  ->
        let rcat = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n]
        in  ncfa_add_regexp_cat (ncfa, ss) rcat rt sign
    IterRange rprim n m ->
        let rcat               = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n - 1]
            (ncfa', ss')       = ncfa_add_regexp_cat (ncfa, ss) rcat rt sign
            ss''               = if n == 0 then S.union ss ss' else ss'
            (ncfa'', _, ss''') = foldl'
                (\ (ncfa, ss, xs) _ ->
                    let (ncfa', ss') = ncfa_add_regexp_prim (ncfa, ss) rprim rt sign
                    in  (ncfa', ss', S.union ss' xs)
                ) (ncfa', ss', ss'') [n .. m - 1]
        in  (ncfa'', ss''')


ncfa_add_regexp_prim :: (NCFA, S.Set State) -> RegexpPrim -> RegexpTable -> SignNum -> (NCFA, S.Set State)
ncfa_add_regexp_prim (ncfa, ss) r rt sign = case r of
    Elementary s -> foldl' (\(d, s) c -> ncfa_add_regexp_atom (d, s) (LabelChar c) sign) (ncfa, ss) s
    Name s       ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  ncfa_add_regexp_alt (ncfa, ss) ralt rt sign
    Wrapped ralt -> ncfa_add_regexp_alt (ncfa, ss) ralt rt sign
    Any          -> ncfa_add_regexp_atom (ncfa, ss) (LabelRange ['\x00' .. '\xFF']) sign
    Range s      -> ncfa_add_regexp_atom (ncfa, ss) (LabelRange s) sign


ncfa_add_regexp_atom :: (NCFA, S.Set State) -> Label -> SignNum -> (NCFA, S.Set State)
ncfa_add_regexp_atom (ncfa, ss) l sign =
    let s_max = maxState ncfa
    in  S.foldl
            (\ (ncfa', ss') s ->
                     let (ncfa'', s') = addTransitionNCFA ncfa' (s, l, sign, s_max)
                     in  (ncfa'', S.insert s' ss')
            ) (ncfa, S.empty) ss


re2ncfa :: [RegexpName] -> RegexpTable -> NCFA
re2ncfa rs rt =
    let ncfa = emptyNCFA
    in  fst $ foldl'
            (\ (ncfa, ss) (k, r) -> (fst (ncfa_add_regexp (ncfa, ss) (M.lookupDefault undefined r rt) rt k), ss))
            (ncfa, S.insert (initStateNCFA ncfa) S.empty)
            (zip [0 .. length rs - 1] rs)
