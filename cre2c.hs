#!/usr/bin/env runghc


import           System.Environment          (getArgs)
import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


gen_code :: ChunkList -> Int -> RegexpTable -> (BS.ByteString, Int)
gen_code (LastChunk code)              _ _            = (code, 0)
gen_code (Chunk code rules chunk_list) k regexp_table =
    let (regexps, conds2code) = (unzip . M.toList) rules
        (ncfa, maxlen')       = re2ncfa regexps regexp_table
        dcfa                  = determine ncfa
        code'                 = cfa2cpp dcfa code conds2code maxlen' k
        (code'', maxlen'')    = gen_code chunk_list (k + 1) regexp_table
    in  (BS.append code' code'', max maxlen' maxlen'')


main :: IO ()
main = do
    args <- getArgs
    (fsrc, fdest, fre) <- case args of
        [src, dest, re] -> return (src, dest, re)
        _               -> error "usage: ./cre2c.hs <source.cre> <destination.cpp> <rules.def>"

    chunk_list   <- parse_source fsrc
    regexp_table <- parse_regexps fre
    let (code, maxlen) = gen_code chunk_list 0 regexp_table

    BS.writeFile fdest $ BS.concat
        [ BS.pack "#define MAXLEN "
        , BS.pack $ show maxlen
        , BS.pack "\n"
        , code
        ]
