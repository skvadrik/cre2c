#!/usr/bin/env runghc


import           System.Environment          (getArgs)
import qualified Data.HashMap.Strict   as M
import qualified Data.ByteString.Char8 as BS
import           Data.List                   (foldl')
import           Data.Either                 (lefts, rights)
import           Control.Applicative         ((<$>))
import           Control.Monad               (when)
import           System.Cmd
import           Text.Printf
import           System.Console.GetOpt

import           Types
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


gen_code :: [Chunk] -> RegexpTable Char -> M.HashMap String (RegexpTable Int) -> Verbosity -> IO (BS.ByteString, Int)
gen_code chunks crtbl irtbls v = do
    let chunks'      = zip [0 .. length chunks] chunks
        find_irtbl t = M.lookupDefault (err "gen_code : regexp table not found") t irtbls
        f (k, chunk) = case chunk of
            Ch2 _ (Opts _ _ (TTEnum t))    _ -> gen_code_for_chunk k chunk (find_irtbl t) v
            Ch2 _ (OptsBlock _ (TTEnum t)) _ -> gen_code_for_chunk k chunk (find_irtbl t) v
            _                                -> gen_code_for_chunk k chunk crtbl          v
    (codes, maxlens) <- unzip <$> mapM f chunks'
    return (BS.concat codes, maximum maxlens)


gen_code_for_chunk :: Labellable a => Int -> Chunk -> RegexpTable a -> Verbosity -> IO (BS.ByteString, Int)
gen_code_for_chunk _ (Ch1 code)            _    _ = return (code, 0)
gen_code_for_chunk k (Ch2 code opts rules) rtbl v = do
    let verbose :: (Show a) => a -> a
        verbose = case v of
            V1 -> trace'
            _  -> id
        (regexps, conds2code) = (unzip . M.toList) rules
        conds2code'           = M.fromList $ zip [0 .. length conds2code - 1] conds2code
        (ncfa, maxlen')       = re2ncfa regexps rtbl
        dcfa                  = determine (verbose ncfa)
        code'                 = cfa2cpp (verbose dcfa) code conds2code' maxlen' k opts
    when (v == V2) $
        putStrLn "Generating .dot for NCFA..." >> ncfa_to_dot ncfa (printf "ncfa%d.dot" k) >>
        putStrLn "Generating .dot for DCFA..." >> dcfa_to_dot dcfa (printf "dcfa%d.dot" k) >>
        putStrLn "Generating .png for NCFA..." >> system (printf "dot -Tpng -oncfa%d.png ncfa%d.dot" k k) >>
        putStrLn "Generating .png for DCFA..." >> system (printf "dot -Tpng -odcfa%d.png dcfa%d.dot" k k) >>
        return ()
    return (code', maxlen')


parse_token_table :: String -> (String, TokenTable)
parse_token_table s =
    let (s1, s2) = (span is_alpha_num_ . skip_spaces) s
        tokens   = case (break (== '}') . skip_spaces) s2 of
            ('{':s3@(_:_), '}':"") -> (map check_token_name . words) s3
            _                      -> err "lex_token_table : bad token table"
        check_token_name s =
            let s' = takeWhile is_alpha_num_ s
            in  if s' == s then s else err "lex_token_table : bad token"
        ttbl = M.fromList $ zip tokens [1 .. length tokens]
    in   (s1, ttbl)


merge_regexp_tables :: RegexpTable a -> RegexpTable a -> RegexpTable a
merge_regexp_tables tbl1 tbl2 =
    let insert_regexp tbl nm r = case M.lookup nm tbl of
            Nothing -> M.insert nm r tbl
            Just _  -> err $ printf "\nMultiple definitions of the same name found: %s" nm
    in  M.foldlWithKey' insert_regexp tbl2 tbl1


group_regexp_tables :: [Either (RegexpTable Char) (String, RegexpTable Int)] -> (RegexpTable Char, M.HashMap String (RegexpTable Int))
group_regexp_tables []    = err "No .def files specified"
group_regexp_tables rtbls =
    let merge_irtbls tbls (t, tbl) = M.insertWith merge_regexp_tables t tbl tbls
        irtbls = (foldl' merge_irtbls M.empty . rights) rtbls
        crtbl  = (foldl' merge_regexp_tables M.empty . lefts) rtbls
    in  (crtbl, irtbls)


data Verbosity = V0 | V1 | V2
instance Eq Verbosity where
    V0 == V0 = True
    V1 == V1 = True
    V2 == V2 = True
    _  == _  = False


data CmdOptions = CmdOpts
    { src     :: Maybe FilePath
    , dest    :: Maybe FilePath
    , defs    :: [FilePath]
    , ttbls   :: [FilePath]
    , verbose :: Verbosity
    }


options :: [OptDescr (CmdOptions -> CmdOptions)]
options =
    [ Option "v"  ["verbose"]   (NoArg  (\ opts -> opts{verbose = V1})                                     ) "trace inner structures"
    , Option "p"  ["pictures"]  (NoArg  (\ opts -> opts{verbose = V2})                                     ) "generate pictures of CFA graphs"
    , Option "o"  ["output"]    (ReqArg (\ f opts -> opts{dest = Just f})                    "<c/cpp-file>") "output FILE"
    , Option "i"  ["input"]     (ReqArg (\ f opts -> opts{src = Just f})                     "<cre-file>"  ) "input FILE"
    , Option "d"  ["def-file"]  (ReqArg (\ f opts@CmdOpts{defs = fs} -> opts{defs = f:fs})   "<def-file>"  ) "definition FILE"
    , Option "t"  ["ttbl-file"] (ReqArg (\ f opts@CmdOpts{ttbls = fs} -> opts{ttbls = f:fs}) "<ttbl-file>" ) "token table FILE"
    ]


parse_args :: [String] -> IO (CmdOptions, [String], [String])
parse_args argv =
    let usage    = "Usage: cre2c [OPTION...] files..."
        def_opts = CmdOpts Nothing Nothing [] [] V0
    in  case getOpt' Permute options argv of
            (o, n, u, []  ) -> return (foldl' (flip id) def_opts o, n, u)
            (_, _, _, errs) -> err $ concat errs ++ usageInfo usage options


err :: String -> a
err s = error $ "*** cre2c : " ++ s


main :: IO ()
main = do
    (opts, non_opts, unknown_opts) <- getArgs >>= parse_args
    when (non_opts /= []) $
        err $ "unparsed cmd arguments: " ++ unwords non_opts
    when (unknown_opts /= []) $
        err $ "unknown cmd-options: " ++ unwords unknown_opts
    let (fsrc, fdest, fdefs, fttbls, verbose) = case opts of
            (CmdOpts Nothing     _          _    _     _) -> err "No source file specified."
            (CmdOpts _           Nothing    _    _     _) -> err "No destination file specified."
            (CmdOpts _           _          []   _     _) -> err "No .def files specified."
            (CmdOpts (Just src) (Just dest) defs ttbls v) -> (src, dest, defs, ttbls, v)

    chunks          <- parse_source <$> readFile fsrc
    ttbls           <- M.fromList . map parse_token_table <$> mapM readFile fttbls
    (crtbl, irtbls) <- group_regexp_tables . map (parse_def_file ttbls) <$> mapM readFile fdefs
    (code, maxlen)  <- gen_code chunks crtbl irtbls verbose

    BS.writeFile fdest $ BS.concat
        [ BS.pack "#define MAXLEN "
        , BS.pack $ show maxlen
        , BS.pack "\n"
        , code
        , BS.pack "\n#undef MAXLEN"
        ]
