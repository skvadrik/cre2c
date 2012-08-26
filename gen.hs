#!/usr/bin/env runghc


import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           Data.Functor               ((<$>))
import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import           Data.List                  (foldl', nub)
import           CFA
import           Control.Monad              (forM_, forM)
import           RegexpParser
import           Data.List                  (intercalate, partition)
import           Data.Maybe                 (isNothing, fromJust, isJust)
import           Control.Arrow              (second)
import           RegexpGenerator
import qualified Data.ByteString.Char8 as BS

import Debug.Trace


usage :: IO ()
usage = putStrLn "usage: ./scan.hs <source file> <destiination file>" >> exitFailure


type Code        = BS.ByteString
type Condition   = String
type Rule        = ([Condition], Regexp, Code)
type RegexpTable = M.HashMap String Regexp
type SignTable   = M.HashMap String [BS.ByteString]


parse_source :: FilePath -> IO (Code, M.HashMap SignNum Rule, Code)
parse_source fp = do
    (code, comment, rest) <-
          (\(code', rest') ->
              let (comment, rest'') = break (== BS.pack "end*/") (tail rest')
              in  (BS.intercalate (BS.pack "\n") code', comment, BS.intercalate (BS.pack "\n") (tail rest'')))
        . break (== BS.pack "/*start:")
        . BS.lines
        <$> BS.readFile fp
    let split_line :: BS.ByteString -> ([Condition], BS.ByteString, Code)
        split_line l =
            let (s1, s2) = (BS.break (== '>') . BS.reverse) l
                (s3, s4) = (BS.break (== '=') . BS.reverse) s1
                conditions  = (words . reverse . dropWhile (== '>')) (BS.unpack s2)
                signature   = BS.dropWhile (`elem` "> \t") s3
                code        = BS.dropWhile (/= '{') s4
            in  (conditions, signature, code)
    let rules =
            ( M.fromList
            . zip [0 .. length comment - 1]
            . map
                ( (\(conds, sign, code) -> (conds, parse_regexp (BS.concat [BS.pack "(", sign, BS.pack ")"]), code))
                . split_line
                )
            ) comment
    return (code, rules, rest)


gen_test_source :: FilePath -> Int -> Int -> IO ()
gen_test_source fp regexp_count regexp_length = do
    regexps <- forM [1 .. regexp_count] (\_ -> new_regexp regexp_length)
    let code = BS.pack $ concat
            [ "\n#include <stdio.h>"
            , "\n#include <string.h>"
            , "\n"
            , "\nvoid scan (const char * buffer, int size)"
            , "\n{"
            , "\n    char * cursor = (char *) buffer;"
            , "\n    char * marker = (char *) buffer;"
            , "\n    char * limit  = (char *) buffer + size;"
            , "\n"
            , "\n#define CURSOR      cursor"
            , "\n#define MARKER      marker"
            , "\n#define LIMIT       limit"
            , "\n#define FILL()      { return; }"
            , "\n"
            , concatMap (\k -> concat
                [ "\n#define c"
                , show k
                , " 1"
                ]) [0 .. regexp_count - 1]
            , "\n"
            , "\n/*start:"
            , concatMap (\k -> concat
                [ "\n    c"
                , show k
                , " > "
                , regexps !! k
                , " = { printf(\"%d\\n\", "
                , show k
                , "); }"
                ]) [0 .. regexp_count - 1]
            , "\nend*/"
            , "\n"
            , "\n}"
            ]
    BS.writeFile fp code


parse_signatures :: FilePath -> IO RegexpTable
parse_signatures fp =
    M.fromList
    . map
        ( second (parse_regexp . BS.pack . tail)
        . break (== '=')
        . filter (`notElem` " \t")
        )
    . lines
    <$> readFile fp




-- уже щас вижу, что будет особый случай, когда звезда клини в конце регэкспа





trace' :: (Show a) => a -> a
trace' a = trace (show a) a


trace'' :: (Show a) => String -> a -> a
trace'' s a = trace (s ++ show a) a





-- мошт надо внутрь автомата засунуть список состояний и получчать его функцией
cfa_add_regexp :: (CFA, S.Set State) -> Regexp -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp (cfa, ss) (Regexp r) rt sign =
    let (cfa', ss') = cfa_add_regexp_alt (cfa, ss) r rt sign
        cfa''       = S.foldl (\ c s -> setFinal c sign s) cfa' ss'
    in  (cfa'', ss')

cfa_add_regexp_alt :: (CFA, S.Set State) -> RegexpAlt -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_alt (cfa, ss) r rt sign = case r of
    AltFromCat rcat -> trace'' "from rcat " $ cfa_add_regexp_cat (cfa, ss) rcat rt sign
    Alt rcat ralt   -> trace'' "from ralt " $
        let (cfa', ss')   = cfa_add_regexp_cat (cfa, ss) rcat rt sign
            (cfa'', ss'') = cfa_add_regexp_alt (cfa', ss) ralt rt sign
        in  (cfa'', S.union ss' ss'')
-- если какая-то альтернатива вернула не то, что просили, то возвращённое состояние надо добавить в список
-- для итераций это будет всегда так, поэтому для них отдельную функцию
-- иначе пытаться добавить всё в конечное состояние

-- можно забабахать функцию типа changeState, но шоб она проверяла, если из изменяемого состояния
-- есть переходы, то это состояние добавить в список
-- полученный т.о. список вернуть
-- если что, так можно же и все состояния не связанными оставить, только оверхед будет по состояниям.

-- ДОБАВЛЯЯ ДУГУ надо смотреть на множество last states и из всех этих состояний добавлять дугу В ОДНО СОСТОЯНИЕ
-- И В ОБЩЕМ-ТО  НУ НАФИГ СВЯЗЫВАНИЕ АЛЬТЕРНАТИВ, НАПРИМЕР :)

cfa_add_regexp_cat :: (CFA, S.Set State) -> RegexpCat -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_cat (cfa, ss) r rt sign = case r of
    CatFromIter riter -> cfa_add_regexp_iter (cfa, ss) riter rt sign
    Cat riter rcat    -> cfa_add_regexp_cat (cfa_add_regexp_iter (cfa, ss) riter rt sign) rcat rt sign

cfa_add_regexp_iter :: (CFA, S.Set State) -> RegexpIter -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_iter (cfa, ss) r rt sign = case r of
    IterFromPrim rprim -> cfa_add_regexp_prim (cfa, ss) rprim rt sign
    Iter rprim n       ->
        let build_rcat :: RegexpPrim -> Int -> RegexpCat
            build_rcat rp m = foldl' (\ rc _ -> Cat (IterFromPrim rp) rc) ((CatFromIter . IterFromPrim) rprim) [1 .. m - 1]

            ralt            = foldl' (\ r k -> Alt (build_rcat rprim (n - k)) r) (AltFromCat (build_rcat rprim n)) [1 .. n - 1]
            (cfa', ss')     = cfa_add_regexp_alt (cfa, ss) ralt rt sign

          in  (cfa', ss')
--        in  (cfa', ss ++ ss')

cfa_add_regexp_prim :: (CFA, S.Set State) -> RegexpPrim -> RegexpTable -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_prim (cfa, ss) r rt sign = case r of
    Elementary s -> foldl' (\(d, s) c -> cfa_add_regexp_atom (d, s) c sign) (cfa, ss) s
    Name s       ->
        let Regexp ralt = M.lookupDefault undefined s rt
        in  cfa_add_regexp_alt (cfa, ss) ralt rt sign
    Wrapped ralt -> cfa_add_regexp_alt (cfa, ss) ralt rt sign

cfa_add_regexp_atom :: (CFA, S.Set State) -> Char -> SignNum -> (CFA, S.Set State)
cfa_add_regexp_atom (cfa, ss) c sign =
    let sl  = lastState cfa
    in  S.foldl
            (\ (cfa', ss') s ->
                let (cfa'', s') = addTransition cfa' (s, c, sign, sl)
                in  (cfa'', S.insert s' ss')
            ) (cfa, S.empty) (trace'' "** " ss)




gen_signatures_from_regexp :: Regexp -> SignTable -> [BS.ByteString]
gen_signatures_from_regexp (Regexp r) = sign_from_ralt r []

sign_from_ralt :: RegexpAlt -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_ralt r ss stbl = case r of
    AltFromCat rcat -> sign_from_rcat rcat ss stbl
    Alt rcat ralt   -> sign_from_rcat rcat ss stbl ++ sign_from_ralt ralt ss stbl

sign_from_rcat :: RegexpCat -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_rcat r ss stbl = case r of
    CatFromIter riter -> sign_from_riter riter ss stbl
    Cat riter rcat    -> concatMap (\s -> map (\s' -> BS.concat [s, s']) (sign_from_rcat rcat ss stbl)) (sign_from_riter riter ss stbl)

sign_from_riter :: RegexpIter -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_riter r ss stbl = case r of
    IterFromPrim rprim -> sign_from_rprim rprim ss stbl
    Iter rprim n       -> let ss' = sign_from_rprim rprim ss stbl in concatMap (\s' -> map (\k -> (BS.concat . replicate k) s') [0 .. n]) ss'

sign_from_rprim :: RegexpPrim -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_rprim r ss stbl = case r of
    Elementary s' -> let s''' = BS.pack s' in case ss of
        [] -> [s''']
        _  -> map (\s'' -> BS.concat [s''', s'']) ss
    Name name     -> concatMap (\s -> map (\s'' -> BS.concat [s, s'']) (M.lookupDefault undefined name stbl)) ss
    Wrapped ralt  -> sign_from_ralt ralt ss stbl

gen_test_string_with_stats :: M.HashMap SignNum [BS.ByteString] -> (BS.ByteString, [SignNum])
gen_test_string_with_stats signatures =
    ((BS.concat . map BS.concat . M.elems) signatures, concatMap (\(sign_num, sign_list) -> replicate (length sign_list) (sign_num - 1)) (M.toList signatures))




main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fsign) <- case args of
        [src, dest]       -> return (src, dest, Nothing)
        [src, dest, sign] -> return (src, dest, Just sign)
        _                 -> usage >> undefined

--    gen_test_source fsrc 3 10

    (code, rules, rest) <- parse_source fsrc
    regexp_table <- case fsign of
        Just fs -> parse_signatures fs
        Nothing -> return M.empty

    let (conditions, regexps, codes) = (unzip3 . M.elems) rules
    let indexes = M.keys rules
    let num_sign = length indexes
    let signs2conds = M.fromList $ zip indexes conditions

    let cfa' = emptyCFA
    let cfa  = fst $ M.foldlWithKey'
            (\ (c, ss) k r -> (fst (cfa_add_regexp (c, ss) r regexp_table k), ss))
            (cfa', S.insert (initialState cfa') S.empty)
            (M.fromList (zip indexes regexps))

    toDot cfa "./cfa.dot"
    print cfa
