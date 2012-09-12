#!/usr/bin/env runghc


import           System.Environment         (getArgs)
import qualified Data.HashMap.Strict   as M

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           RegexpParser
import           SourceParser


main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fre) <- case args of
        [src, dest, re] -> return (src, dest, re)
        _               -> error "usage: ./scangen.hs <source code file> <destination code file> <regexp file>"

    (prolog, rules, epilog) <- parse_source fsrc
    regexp_table            <- parse_regexps fre

    let (conds, regexps, codes) = (unzip3 . M.elems) rules
    let cfa                     = re2cfa regexps regexp_table
    let sign_maxlen             = 56

    cfa2cpp fdest cfa prolog epilog conds codes sign_maxlen

    toDot cfa "./cfa.dot"
    print cfa
--    print regexps
--    print regexp_table
