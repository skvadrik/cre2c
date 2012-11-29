#!/usr/bin/env runghc


import           Control.Applicative             ((<$>))
import           Control.Monad                   (when)
import           Data.Char                       (toUpper, toLower)
import qualified Data.HashMap.Strict       as M
import           Data.Either                     (lefts, rights)
import           Data.List                       (foldl')
import           System.Cmd
import           System.Console.GetOpt
import           System.Environment              (getArgs)
import           System.FilePath.Posix           (takeDirectory, (<.>), (</>))
import           Text.Printf
import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.PrettyPrint.HughesPJ       (($$), (<>))

import           Types                           hiding (err)
import           Helpers
import           CFA
import           RE2CFA
import           CFA2CPP
import           SourceParser
import           RegexpParser


gen_code :: [Chunk] -> MRegname2Regexp Char -> M.HashMap STokname (MRegname2Regexp Int) -> M.HashMap STokname MTokname2TokID -> Verbosity -> IO (SCode, Int)
gen_code chunks crtbl irtbls ttbls v = do
    let chunks'      = zip [0 .. length chunks] chunks
        find_irtbl t = M.lookupDefault (err "gen_code : regexp table not found") t irtbls
        find_ttbl  t = Just $ M.lookupDefault (err "gen_code : token table not found") t ttbls
        f (k, chunk) = case chunk of
            Ch2 _ (Opts _ _ (TTEnum t) _)    _ -> gen_code_for_chunk k chunk (find_irtbl t) (find_ttbl t) v
            Ch2 _ (OptsBlock _ (TTEnum t) _) _ -> gen_code_for_chunk k chunk (find_irtbl t) (find_ttbl t) v
            _                                  -> gen_code_for_chunk k chunk crtbl          Nothing v
    (codes, maxlens) <- unzip <$> mapM f chunks'
    return (concat codes, maximum maxlens)


gen_code_for_chunk :: Labellable a => IBlkID -> Chunk -> MRegname2Regexp a -> Maybe MTokname2TokID -> Verbosity -> IO (SCode, Int)
gen_code_for_chunk _ (Ch1 code)            _    _    _ = return (code, 0)
gen_code_for_chunk k (Ch2 code opts rules) rtbl ttbl v = do
    let verbose :: (Show a) => a -> a
        verbose = case v of
            V1 -> trace'
            _  -> id
        (regexps, conds2code) = (unzip . M.toList) rules
        conds2code'           = M.fromList $ zip [0 .. length conds2code - 1] conds2code
        (ncfa, maxlen')       = re2ncfa regexps rtbl ttbl
        dcfa                  = determine (verbose ncfa)
        bi                    = BI k opts conds2code' ttbl
        code'                 = cfa2cpp (verbose dcfa) code maxlen' bi
    when (v == V2) $
        putStrLn "Generating .dot for NCFA..." >> ncfa_to_dot ncfa (printf "ncfa%d.dot" k) >>
        putStrLn "Generating .dot for DCFA..." >> dcfa_to_dot dcfa (printf "dcfa%d.dot" k) >>
        putStrLn "Generating .png for NCFA..." >> system (printf "dot -Tpng -oncfa%d.png ncfa%d.dot" k k) >>
        putStrLn "Generating .png for DCFA..." >> system (printf "dot -Tpng -odcfa%d.png dcfa%d.dot" k k) >>
        return ()
    return (code', maxlen')


parse_token_table :: String -> (STokname, MTokname2TokID)
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


merge_regexp_tables :: MRegname2Regexp a -> MRegname2Regexp a -> MRegname2Regexp a
merge_regexp_tables tbl1 tbl2 =
    let insert_regexp tbl nm r = case M.lookup nm tbl of
            Nothing -> M.insert nm r tbl
            Just _  -> err $ printf "Multiple definitions of the same name found: %s" nm
    in  M.foldlWithKey' insert_regexp tbl2 tbl1


group_regexp_tables :: [Either (MRegname2Regexp Char) (STokname, MRegname2Regexp Int)] -> (MRegname2Regexp Char, M.HashMap STokname (MRegname2Regexp Int))
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


codegen_token_enum :: FilePath -> STokname -> MTokname2TokID -> SCode
codegen_token_enum f t ttbl =
    let d1 = (PP.text . map (\ c -> if is_alpha_num_ c then toUpper c else '_')) f
        d2 = PP.text t
        d3 = (PP.vcat . map (PP.text . (++ ",")) . M.keys) ttbl
    in  PP.render $
            PP.text "#ifndef " <> d1
            $$ PP.text "#define " <> d1
            $$$ PP.text "enum " <> d2
            $$ wrap_in_braces d3 <> PP.semi
            $$$ PP.text "#endif // " <> d1


gen_ttbl_headers :: [FilePath] -> M.HashMap STokname MTokname2TokID -> IO [FilePath]
gen_ttbl_headers fs ttbls = do
    let filename (f, t) = takeDirectory f </> map toLower t <.> "h"
        ts       = M.keys ttbls
        tbls     = M.elems ttbls
        fs'      = map filename (zip fs ts)
        fs2codes = map (\ (f, t, tbl) -> (f, codegen_token_enum f t tbl)) (zip3 fs' ts tbls)
    mapM_ (\ (f, c) -> writeFile f c) fs2codes
    return fs'


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
    (code, maxlen)  <- gen_code chunks crtbl irtbls ttbls verbose
    fttbl_hdrs      <- gen_ttbl_headers fttbls ttbls

    writeFile fdest $ PP.render $
        PP.text "#define MAXLEN " <> PP.int maxlen
        $$$ PP.vcat (map ((PP.text "#include " <>) . PP.doubleQuotes . PP.text) fttbl_hdrs)
        $$$ PP.text code
        $$$ PP.text "#undef MAXLEN"
