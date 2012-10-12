#!/usr/bin/env runghc

module TestGenerator
    ( newTest
    ) where


import           System.Environment          (getArgs)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as M
import           Control.Monad               (forM)

import           Types
import           RegexpGenerator
import           RegexpParser


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
    IterFromPrim rprim     -> sign_from_rprim rprim ss stbl
    IterRepeat   rprim n   -> let ss' = sign_from_rprim rprim ss stbl in map (BS.concat . replicate n) ss'
    IterRange    rprim n m -> let ss' = sign_from_rprim rprim ss stbl in concatMap (\s' -> map (\k -> (BS.concat . replicate k) s') [n .. m]) ss'


sign_from_rprim :: RegexpPrim -> [BS.ByteString] -> SignTable -> [BS.ByteString]
sign_from_rprim r ss stbl = case r of
    Elementary s' -> let s''' = BS.pack s' in case ss of
        [] -> [s''']
        _  -> map (\s'' -> BS.concat [s''', s'']) ss
    Name name     -> concatMap (\s -> map (\s'' -> BS.concat [s, s'']) (M.lookupDefault undefined name stbl)) ss
    Wrapped ralt  -> sign_from_ralt ralt ss stbl
    Range s       -> let ss' = map (\ c -> BS.pack [c]) s in concatMap (\ s' -> map (\ s -> BS.concat [s', s]) ss) ss'
    Any           -> let s' = BS.pack "*" in case ss of
        [] -> [s']
        _  -> map (\s'' -> BS.concat [s', s'']) ss


new_test :: [Regexp] -> RegexpTable -> (BS.ByteString, [SignNum], Int)
new_test rs rt =
    let predefined_signs = M.map (`gen_signatures_from_regexp` M.empty) rt
        signs            = M.fromList $ zip [1 .. length rs] $ map (`gen_signatures_from_regexp` predefined_signs) rs
        sign_maxlen      = maximum $ map BS.length $ concat $ M.elems signs
    in  (( BS.concat . map BS.concat . M.elems) signs
        , concatMap (\(sign_num, sign_list) -> replicate (length sign_list) (sign_num - 1)) (M.toList signs)
        , sign_maxlen
        )


newTest :: FilePath -> Int -> Int -> IO ()
newTest fp regexp_count regexp_length = do
    regexp_strings <- forM [1 .. regexp_count] (\_ -> newRegexp regexp_length)
    let regexps = map parseRegexp regexp_strings
    let (test_string, stats, sign_maxlen) = new_test regexps M.empty
    let code = BS.concat
            [ (BS.pack . concat)
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
                    , regexp_strings !! k
                    , " = { printf(\"%d\\n\", "
                    , show k
                    , "); }"
                    ]) [0 .. regexp_count - 1]
                , "\nend*/"
                , "\n"
                , "\n}"
                , "\nint main ()"
                , "\n{"
                , "\n    const char * buffer = \""
                ]
            , test_string
            , (BS.pack . concat)
                [ replicate (sign_maxlen + 0) '*'
                , "\";\n    scan(buffer, strlen(buffer));"
                , "\n    return 0;"
                , "\n}"
                , "\n// "
                , show stats
                , "\n"
                ]
            ]
    BS.writeFile fp code


main :: IO ()
main = do
    args <- getArgs
    (fp, n, l) <- case args of
        [a, b, c] -> return (a, read b :: Int, read c :: Int)
        _         -> error "usage: ./TestGenerator.hs <destination file> <number of regexps> <regexp average length>"

    newTest fp n l

