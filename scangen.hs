#!/usr/bin/env runghc


import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           Data.Functor               ((<$>))
import qualified Data.HashMap.Strict   as M
import           Data.List                  (intercalate)
import           Control.Arrow              (second)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe                 (catMaybes)

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           RegexpParser

import Debug.Trace


usage :: IO ()
usage = putStrLn "usage: ./scangen.hs <source code file> <destination code file> <regexp file>" >> exitFailure


parse_source :: FilePath -> IO (Code, M.HashMap SignNum Rule, Code)
parse_source fp = do
    (prolog, scangen_code, epilog) <-
        (\ (prolog, rest) ->
              let (scangen_code, epilog) = break (== BS.pack "end*/") (tail rest)
              in  (BS.unlines prolog, filter (/= BS.empty) scangen_code, BS.unlines (tail epilog))
        )
        . break (== BS.pack "/*start:")
        . BS.lines
        <$> BS.readFile fp
{-
    let split_line :: String -> ([Condition], String, Code)
        split_line l =
            let (s1, s2) = break (== '>') l
                (s3, s4) = (break (== '=') . tail) s2
                conditions  = words s1
                regexp_name = (head . words) s3
                code        = (BS.pack . dropWhile (/= '{')) s4
            in  (conditions, regexp_name, code)
-}
    let split_line :: String -> ([Condition], String, Code)
        split_line l =
            let (s1, s2) = (break (not . isAlphaNum) . dropWhile (`elem` "\t ")) l
                (s3, s4) = (break (== '=') . tail) s2
                conditions  = words s1
                regexp_name = (head . words) s3
                code        = (BS.pack . dropWhile (/= '{')) s4
            in  (conditions, regexp_name, code)
    let rules =
            ( M.fromList
            . zip [0 .. length scangen_code - 1]
            . map (split_line . BS.unpack)
            ) scangen_code
    return (prolog, rules, epilog)


trace' a = trace (show a) a


main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fsign) <- case args of
        [src, dest]       -> return (src, dest, Nothing)
        [src, dest, sign] -> return (src, dest, Just sign)
        _                 -> usage >> undefined

    (prolog, rules, epilog) <- parse_source fsrc
    print prolog
    print rules
    print epilog
    regexp_table <- case fsign of
        Just fs -> parse_regexps fs
        Nothing -> return M.empty

    let (conditions, regexps, codes) = (unzip3 . M.elems) rules
    let cfa = re2cfa regexps regexp_table
    let sign_maxlen = 50

    cfa2cpp fdest cfa prolog epilog conditions codes sign_maxlen

    toDot cfa "./cfa.dot"
--    print cfa
--    print regexps
    print regexp_table
