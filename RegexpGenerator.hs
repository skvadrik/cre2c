module RegexpGenerator
    ( newRegexp
    ) where


import Control.Monad.Random as R
import System.Random
import Data.Char


gen_regexp :: String -> String
gen_regexp "" = ""
gen_regexp (c:cs)
      | isSpace c = gen_regexp cs
      | isAlpha c = skip_name (c : cs)
gen_regexp ('(':cs) = skip_regexp cs
gen_regexp ('{':cs) = skip_int cs
gen_regexp ('|':cs) = '|' : gen_regexp (dropWhile (`elem` special_chars) cs)
gen_regexp (c:cs)   = gen_regexp cs

skip_regexp cs =
    let cs' = (span (/= ')') . dropWhile (== '(')) cs
    in  concat
            [ "("
            , gen_regexp (dropWhile (`elem` special_chars) (fst cs' ++ "a"))
            , ")"
            , gen_regexp (snd cs')
            ]

skip_int cs =
    let cs' = dropWhile (`elem` special_chars) cs
        n   = (show . (+ 1) . (`mod` 10) . ord) (head cs)
    in  concat
            [ "{,"
            , n
            , "}"
            , gen_regexp cs'
            ]

skip_name cs =
    let (name, rest) = span isAlphaNum cs
    in  concat
            [ "\""
            , name
            , "\""
            , gen_regexp rest
            ]


special_chars :: [Char]
special_chars = "(){|"


common_chars :: [Char]
common_chars = ['a' .. 'z']


weighted_chars :: [(Char, Rational)]
weighted_chars = map (\c -> (c, 5)) special_chars ++ map (\c -> (c, 1)) common_chars


newRegexp :: Int -> IO String
newRegexp n = do
    random_string <- evalRandIO . sequence . replicate n . R.fromList $ weighted_chars
    return $ gen_regexp $ concat
        [ "\"a\""
        , random_string
        , "a"
        ]
