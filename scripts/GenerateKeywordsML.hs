-- Generate lexer patterns (reimplementation)
-- input := tokenlist
-- tokenlist := tokenspec [,\n] tokenlist |
-- tokenspec := (tletters | tctor tletters+) @__?
-- tctor     := ctor | (composite ctor)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import System.IO
import System.Environment
import Data.List as L
import Data.Ord

parseInput :: String -> [[String]]
parseInput = map (map T.unpack) .
             catMaybes .
             map (parseTokenSpec . T.strip) .
             T.split (','==) . T.intercalate (T.pack ",") .
             T.lines . T.pack

parseTokenSpec t | T.null t = Nothing
                 | T.head t == '(' =
                   let (tokexpr,tokenstr) = T.break (==')') t
                   in Just $ parseTokenSpec' (T.snoc tokexpr ')') (T.words $ T.tail tokenstr)
                 | otherwise =
                   let (tokexpr:tokens) = T.words t
                   in Just $ parseTokenSpec' tokexpr tokens

parseTokenSpec' tokexpr tokenlist =
  case T.unpack (last (tokexpr:tokenlist)) of
    "@__" -> addReservedTokens (parseTokenSpec'' tokexpr (init tokenlist))
    _     -> parseTokenSpec'' tokexpr tokenlist

parseTokenSpec'' tokexpr [] = [tokexpr, tokexpr]
parseTokenSpec'' tokexpr ts = tokexpr : ts

addReservedTokens [tokexpr, tok] = tokexpr : [us `T.append` tok, tok, us `T.append` tok `T.append` us ]
  where us = T.pack "__"
addReservedTokens list = error $ "addReservedTokens" ++ show list

expandInput = sortBy (comparing (map toLower . dropWhile (=='_') . snd)) . concatMap expand
  where
    expand (t:ts) = [ (t,t') | t' <- ts ]

genOutput1 (ttok,tstr) =
  "    | " ++ pattern ++ " => " ++ escape_sml (map toLower $ genTok ttok)
  where
    genTok ('(':_) = concatMap (\c -> let c' = toLower c in if c' >= 'a' && c' <= 'z' || c' >= '0' && c' <= '9' then [c'] else []) tstr
    genTok ts = ts
    pattern = "\"" ++ tstr ++ "\""
    escape_sml body3 =
             if body3 `elem` ["case", "do", "else", "for", "if", "struct", "while", "return"] then
               body3 ++ "0"
             else
               body3

genOutput2 (_,tstr) =
  "  " ++ pattern ++ ","
  where
    pattern = "\"" ++ tstr ++ "\""

run ifile = do
  inp <-readFile ifile
  let tokens = expandInput $ parseInput inp
  mapM_ putStrLn (map genOutput1 tokens)
  putStrLn "(**)"
  mapM_ putStrLn (map genOutput2 tokens)

main = do
  arguments <- getArgs
  let (ifile) =
        case arguments of
          [a]-> a
          _ -> error "Usage: GenerateKeywordsML.hs tokenlist.txt"
  run ifile
