module RE2CFA
    ( re2cfa
    ) where


import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl')

import           Types
import           CFA


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
    IterFromPrim rprim  -> cfa_add_regexp_prim (cfa, ss) rprim rt sign
    IterMaybe rprim     ->
        let (cfa', ss') = cfa_add_regexp_prim (cfa, ss) rprim rt sign
        in  (cfa', S.union ss ss')
    IterRepeat rprim n  ->
        let rcat = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n]
        in  cfa_add_regexp_cat (cfa, ss) rcat rt sign
    IterRange rprim n m ->
        let rcat        = foldl' (\ rc _ -> Cat (IterFromPrim rprim) rc) ((CatFromIter . IterFromPrim) rprim)  [1 .. n]
            (cfa', ss') = cfa_add_regexp_cat (cfa, ss) rcat rt sign
        in  foldl'
            (\ (cfa'', ss'') _ ->
                let (cfa''', ss''') = cfa_add_regexp_prim (cfa'', ss'') rprim rt sign
                in  (cfa''', S.union ss'' ss''')
            ) (cfa', ss') [n .. m - 1]


cfa_add_regexp_prim :: (CFA, S.Set State) -> RegexpPrim -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_prim (cfa, ss) r rt sign = case r of
    Elementary s -> foldl' (\(d, s) c -> cfa_add_regexp_atom (d, s) (LabelChar (trace' c)) sign) (cfa, ss) s
    Name s       ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  cfa_add_regexp_alt (cfa, ss) ralt rt sign
    Wrapped ralt -> cfa_add_regexp_alt (cfa, ss) ralt rt sign
-- тут надо осторожно: добавлять ещё все состояния, в которые переходим по обычным дугам
-- потом будет ещё жопа при добавлении новой дуги
    Any          -> cfa_add_regexp_atom (cfa, ss) (LabelRange ['\x00' .. '\xFF']) sign
--    Any          -> cfa_add_regexp_atom (cfa, ss) LabelAny sign
-- что если значение из диапазона совпадает с уже добавленной дугой?
-- тогда останется только первая дуга
-- надо addTransition переделывать
-- может оказаться проще убрать диапазрны, заменить альтернативами
-- ну то есть времени на генерацию не жалко. Но шоб не намутить.
    Range s      -> cfa_add_regexp_atom (cfa, ss) (LabelRange s) sign
--        let s' = filter (\ c -> isNothing (M.lookup c )) s
--        in  cfa_add_regexp_atom (cfa, ss) (LabelRange s') sign


{-
три проблемы
1) Range ---- диапазоны. Видно придётся их делать на альтернативах.
2) Any ---- их надо загонять не только в дефолты, но из  всех состояний добавлять дуги.
3) case sensitivity

Any, Range делать через LabelRange. Вычислять "оставшиеся" символы.
-}


cfa_add_regexp_atom :: (CFA, S.Set State) -> Label -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_atom (cfa, ss) l sign = S.foldl
    (\ (cfa', ss') s ->
             let s_max         = maxStateNumber cfa'
                 (cfa'', ss'') = addTransition cfa' (s, l, sign, s_max)
             in  (cfa'', S.union ss'' ss')
    ) (cfa, S.empty) ss

{-
cfa_add_regexp_atom :: (CFA, S.Set State) -> Label -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_atom (cfa, ss) l sign =
    let s_max = maxStateNumber cfa
    in  S.foldl
            (\ (cfa', ss') s ->
                     let (cfa'', ss'') = addTransition cfa' (s, l, sign, s_max)
                     in  (cfa'', S.union ss'' ss')
            ) (cfa, S.empty) ss
-}



re2cfa :: [RegexpName] -> RegexpTable -> CFA
re2cfa rs rt =
    let cfa = emptyCFA
    in  fst $ foldl'
            (\ (cfa, ss) (k, r) -> (fst (cfa_add_regexp (cfa, ss) (M.lookupDefault undefined r rt) rt k), ss))
            (cfa, S.insert (initialState cfa) S.empty)
            (zip [0 .. length rs - 1] rs)
