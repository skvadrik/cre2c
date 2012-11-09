#!/usr/bin/env runghc


import           System.Environment          (getArgs)
import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import           Data.List                   (foldl')
import           Control.Applicative         ((<$>))

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


gen_code :: ChunkList -> Int -> RegexpTable -> (BS.ByteString, Int)
gen_code (LastChunk code)                   _ _            = (code, 0)
gen_code (Chunk code opts rules chunk_list) k regexp_table =
    let (regexps, conds2code) = (unzip . M.toList) rules
        (ncfa, maxlen')       = re2ncfa regexps regexp_table
        dcfa                  = determine ncfa
        code'                 = cfa2cpp dcfa code conds2code maxlen' k opts
        (code'', maxlen'')    = gen_code chunk_list (k + 1) regexp_table
    in  (BS.append code' code'', max maxlen' maxlen'')


merge_regexp_tables :: [RegexpTable] -> RegexpTable
merge_regexp_tables []         = error "No .def files specified"
merge_regexp_tables (rt : rts) =
    let merge_one :: RegexpTable -> RegexpTable -> RegexpTable
        merge_one = M.foldlWithKey' (\ rt nm r ->  case M.lookup nm rt of
            Nothing -> M.insert nm r rt
            Just _  -> error $ concat
                [ "\nMultiple definitions of the same name found: "
                , nm
                , "\nEither remove or rename one of the regexps."
                ])
    in  foldl' merge_one rt rts


main :: IO ()
main = do
    args <- getArgs
    (fcre, fcpp, fdefs) <- case args of
        cre : cpp : defs -> return (cre, cpp, defs)
        _                -> error "usage: ./cre2c.hs <source.cre> <destination.cpp> <rules.def> [<rules1.def> ... <rulesN.def>]"

    chunk_list   <- parse_source fcre
    regexp_table <- merge_regexp_tables <$> mapM parse_regexps fdefs
    let (code, maxlen) = gen_code chunk_list 0 regexp_table

    BS.writeFile fcpp $ BS.concat
        [ BS.pack "#define MAXLEN "
        , BS.pack $ show maxlen
        , BS.pack "\n"
        , code
        ]
