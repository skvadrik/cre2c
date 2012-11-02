#!/usr/bin/env runghc


import           System.Environment          (getArgs)
import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString       as BS

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


gen_code :: ChunkList -> RegexpTable -> BS.ByteString
gen_code (LastChunk code)              regexp_table = code
gen_code (Chunk code rules chunk_list) regexp_table =
    let (regexps, conds2code) = (unzip . M.toList) rules
        ncfa                  = re2ncfa regexps regexp_table
        dcfa                  = determine ncfa
        sign_maxlen           = 56
        code'                 = cfa2cpp dcfa code conds2code sign_maxlen
    in  BS.append code' $ gen_code chunk_list regexp_table


main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fre) <- case args of
        [src, dest, re] -> return (src, dest, re)
        _               -> error "usage: ./cre2c.hs <source.cre> <destination.cpp> <rules.def>"

    chunk_list   <- parse_source fsrc
    regexp_table <- parse_regexps fre

    BS.writeFile fdest $ gen_code chunk_list regexp_table
