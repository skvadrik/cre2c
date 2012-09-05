module RE2CFA
    ( re2cfa
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')
import           CFA
import           RegexpParser


type RegexpTable = M.HashMap String Regexp


cfa_add_regexp :: (CFA, S.Set State) -> Regexp -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp (cfa, ss) (Regexp r) rt sign =
    let (cfa', ss') = cfa_add_regexp_alt (cfa, ss) r rt sign
        cfa''       = S.foldl (\ c s -> setFinal s sign c) cfa' ss'
    in  (cfa'', ss')


cfa_add_regexp_alt :: (CFA, S.Set State) -> RegexpAlt -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_alt (cfa, ss) r rt sign = case r of
    AltFromCat rcat -> cfa_add_regexp_cat (cfa, ss) rcat rt sign
    Alt rcat ralt   ->
        let (cfa', ss')   = cfa_add_regexp_cat (cfa, ss) rcat rt sign
            (cfa'', ss'') = cfa_add_regexp_alt (cfa', ss) ralt rt sign
        in  (cfa'', S.union ss' ss'')


cfa_add_regexp_cat :: (CFA, S.Set State) -> RegexpCat -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_cat (cfa, ss) r rt sign = case r of
    CatFromIter riter -> cfa_add_regexp_iter (cfa, ss) riter rt sign
    Cat riter rcat    -> cfa_add_regexp_cat (cfa_add_regexp_iter (cfa, ss) riter rt sign) rcat rt sign


cfa_add_regexp_iter :: (CFA, S.Set State) -> RegexpIter -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_iter (cfa, ss) r rt sign = case r of
    IterFromPrim rprim -> cfa_add_regexp_prim (cfa, ss) rprim rt sign
    Iter rprim n       -> foldl'
        (\ (cfa', ss') _ ->
            let (cfa'', ss'') = cfa_add_regexp_prim (cfa', ss') rprim rt sign
            in  (cfa'', S.union ss' ss'')
        ) (cfa, ss) [1 .. n]


cfa_add_regexp_prim :: (CFA, S.Set State) -> RegexpPrim -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_prim (cfa, ss) r rt sign = case r of
    Elementary s -> foldl' (\(d, s) c -> cfa_add_regexp_atom (d, s) c sign) (cfa, ss) s
    Name s       ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  cfa_add_regexp_alt (cfa, ss) ralt rt sign
    Wrapped ralt -> cfa_add_regexp_alt (cfa, ss) ralt rt sign


cfa_add_regexp_atom :: (CFA, S.Set State) -> Char -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_atom (cfa, ss) c sign =
    let sl  = maxStateNumber cfa
    in  S.foldl
            (\ (cfa', ss') s ->
                     let (cfa'', s') = addTransition cfa' (s, c, sign, sl)
                     in  (cfa'', S.insert s' ss')
            ) (cfa, S.empty) ss


re2cfa :: [Regexp] -> RegexpTable -> CFA
re2cfa rs rt =
    let cfa = emptyCFA
    in  fst $ foldl'
            (\ (cfa, ss) (k, r) -> (fst (cfa_add_regexp (cfa, ss) r rt k), ss))
            (cfa, S.insert (initialState cfa) S.empty)
            (zip [0 .. length rs] rs)
