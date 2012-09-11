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
import           SourceParser

import Debug.Trace


usage :: IO ()
usage = putStrLn "usage: ./scangen.hs <source code file> <destination code file> <regexp file>" >> exitFailure


trace' a = trace (show a) a


main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fsign) <- case args of
        [src, dest]       -> return (src, dest, Nothing)
        [src, dest, sign] -> return (src, dest, Just sign)
        _                 -> usage >> undefined

    (prolog, rules, epilog) <- parse_source fsrc
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
