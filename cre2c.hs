#!/usr/bin/env runghc


import           System.Environment          (getArgs)
import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import           Data.List                   (foldl')
import           Control.Applicative         ((<$>))
import           Debug.Trace

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


trace' :: (Show a) => a -> a
trace' a = trace (show a) a


gen_code :: ChunkList -> Int -> RegexpTable -> Verbosity -> (BS.ByteString, Int)
gen_code (LastChunk code)                   _ _            _ = (code, 0)
gen_code (Chunk code opts rules chunk_list) k regexp_table v =
    let verbose :: (Show a) => a -> a
        verbose = case v of
            V0 -> id
            V1 -> trace'
        (regexps, conds2code) = (unzip . M.toList) rules
        (ncfa, maxlen')       = re2ncfa regexps regexp_table
        dcfa                  = determine (verbose ncfa)
        code'                 = cfa2cpp (verbose dcfa) code conds2code maxlen' k opts
        (code'', maxlen'')    = gen_code chunk_list (k + 1) regexp_table v
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


data Verbosity = V0 | V1
data Args = Args
    { src     :: Maybe FilePath
    , dest    :: Maybe FilePath
    , defs    :: [FilePath]
    , verbose :: Verbosity
    }


parse_args :: [String] -> Args
parse_args =
    let parse_args' :: Args -> [String] -> Args
        parse_args' args []              = args
        parse_args' args ("-v" : xs)     = parse_args' (args { verbose = V1 }) xs
        parse_args' args ("-o" : x : xs) = parse_args' (args { dest = Just x }) xs
        parse_args' args ("-d" : xs) =
            let span_defs :: [FilePath] -> [String] -> Args
                span_defs ds []                 = args { defs = ds }
                span_defs ds xs@(('-' : _) : _) = parse_args' (args { defs = ds }) xs
                span_defs ds (x : xs)           = span_defs (x : ds) xs
            in  span_defs [] xs
        parse_args' args (x : xs)        = parse_args' (args { src = Just x }) xs
    in  parse_args' (Args Nothing Nothing [] V0)


main :: IO ()
main = do
    args <- parse_args <$> getArgs
    let (fsrc, fdest, fdefs, verbose) = case args of
            (Args Nothing     _          _    _) -> error "No source file specified."
            (Args _           Nothing    _    _) -> error "No destination file specified."
            (Args _           _          []   _) -> error "No .def files specified."
            (Args (Just src) (Just dest) defs v) -> (src, dest, defs, v)

    chunk_list   <- parse_source fsrc
    regexp_table <- merge_regexp_tables <$> mapM parse_regexps fdefs
    let (code, maxlen) = gen_code chunk_list 0 regexp_table verbose

    BS.writeFile fdest $ BS.concat
        [ BS.pack "#define MAXLEN "
        , BS.pack $ show maxlen
        , BS.pack "\n"
        , code
        ]
