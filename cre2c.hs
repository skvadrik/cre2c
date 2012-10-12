#!/usr/bin/env runghc


import           System.Environment         (getArgs)
import qualified Data.HashMap.Strict   as M

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fre) <- case args of
        [src, dest, re] -> return (src, dest, re)
        _               -> error "usage: ./cre2c.hs <source.cre> <destination.cpp> <rules.def>"

    (prolog, rules, epilog) <- parse_source fsrc
    regexp_table            <- parse_regexps fre

    let (regexps, conds2code) = (unzip . M.toList) rules
    let ncfa                  = re2ncfa regexps regexp_table
    let dcfa                  = determine ncfa
    let sign_maxlen           = 56

    cfa2cpp fdest dcfa prolog epilog conds2code sign_maxlen
