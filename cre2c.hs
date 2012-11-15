#!/usr/bin/env runghc


import           System.Environment          (getArgs)
import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import           Data.List                   (foldl')
import           Control.Applicative         ((<$>))
import           Control.Monad               (when)
import           Debug.Trace
import           System.Cmd
import           Text.Printf
import           System.Console.GetOpt

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


trace' :: (Show a) => a -> a
trace' a = trace (show a) a


gen_code :: ChunkList -> Int -> RegexpTable -> Verbosity -> IO (BS.ByteString, Int)
gen_code (LastChunk code)                   _ _            _ = return (code, 0)
gen_code (Chunk code opts rules chunk_list) k regexp_table v = do
    let verbose :: (Show a) => a -> a
        verbose = case v of
            V1 -> trace'
            _  -> id
        (regexps, conds2code) = (unzip . M.toList) rules
        (ncfa, maxlen')       = re2ncfa regexps regexp_table
        dcfa                  = determine (verbose ncfa)
        code'                 = cfa2cpp (verbose dcfa) code conds2code maxlen' k opts
    when (v == V2) $
        putStrLn "Generating .dot for NCFA..." >> ncfa_to_dot ncfa (printf "ncfa%d.dot" k) >>
        putStrLn "Generating .dot for DCFA..." >> dcfa_to_dot dcfa (printf "dcfa%d.dot" k) >>
        putStrLn "Generating .png for NCFA..." >> system (printf "dot -Tpng -oncfa%d.png ncfa%d.dot" k k) >>
        putStrLn "Generating .png for DCFA..." >> system (printf "dot -Tpng -odcfa%d.png dcfa%d.dot" k k) >>
        return ()
    (code'', maxlen'') <- gen_code chunk_list (k + 1) regexp_table v
    return (BS.append code' code'', max maxlen' maxlen'')


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


data Verbosity = V0 | V1 | V2

instance Eq Verbosity where
    V0 == V0 = True
    V1 == V1 = True
    V2 == V2 = True
    _  == _  = False

data CmdOptions = CmdOptions
    { src     :: Maybe FilePath
    , dest    :: Maybe FilePath
    , defs    :: [FilePath]
    , verbose :: Verbosity
    }

default_options = CmdOptions
    { src     = Nothing
    , dest    = Nothing
    , defs    = []
    , verbose = V0
    }


options :: [OptDescr (CmdOptions -> CmdOptions)]
options =
    [ Option "v"  ["verbose"]      (NoArg (\ opts -> opts { verbose = V1 }))                                       "trace inner structures"
    , Option "V"  ["very-verbose"] (NoArg (\ opts -> opts { verbose = V2 }))                                       "trace inner structures and output CFA graphs to PNG"
    , Option "o"  ["output"]       (ReqArg (\ f opts -> opts{ dest = Just f })                     "<c/cpp-file>") "output FILE"
    , Option "i"  ["input"]        (ReqArg (\ f opts -> opts{ src = Just f })                      "<cre-file>")   "input FILE"
    , Option "d"  ["def-file"]     (ReqArg (\ f opts@CmdOptions{defs = fs} -> opts{ defs = f:fs }) "<def-file>")   "definition FILE"
    ]


parse_args :: [String] -> IO (CmdOptions, [String], [String])
parse_args argv =
    let header = "Usage: ic [OPTION...] files..."
    in  case getOpt' Permute options argv of
            (o, n, u, []  ) -> return (foldl (flip id) default_options o, n, u)
            (_, _, _, errs) -> error $ concat errs ++ usageInfo header options


main :: IO ()
main = do
    (opts, non_opts, unknown_opts) <- getArgs >>= parse_args
    when (non_opts /= []) $
        error $ "*** cre2c : unparsed cmd arguments: " ++ unwords non_opts
    when (unknown_opts /= []) $
        error $ "*** cre2c : unknown cmd-options: " ++ unwords unknown_opts
    let (fsrc, fdest, fdefs, verbose) = case opts of
            (CmdOptions Nothing     _          _    _) -> error "No source file specified."
            (CmdOptions _           Nothing    _    _) -> error "No destination file specified."
            (CmdOptions _           _          []   _) -> error "No .def files specified."
            (CmdOptions (Just src) (Just dest) defs v) -> (src, dest, defs, v)

    chunk_list     <- parse_source fsrc
    regexp_table   <- merge_regexp_tables <$> mapM parse_regexps fdefs
    (code, maxlen) <- gen_code chunk_list 0 regexp_table verbose

    BS.writeFile fdest $ BS.concat
        [ BS.pack "#define MAXLEN "
        , BS.pack $ show maxlen
        , BS.pack "\n"
        , code
        ]
