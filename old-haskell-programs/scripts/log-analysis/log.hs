import Data.Map as Map

import List
import Char
import IO
import System.Environment
import Data.List

type PhraseCounterKey = String
type PhraseCounterValue = Integer
type PhraseCounter = Map PhraseCounterKey PhraseCounterValue

phraseCounter_to_string :: PhraseCounter -> String
phraseCounter_to_string my_map =
        concat [(conv x)|x <- sorted_list]
        where
            list :: [(PhraseCounterKey, PhraseCounterValue)]
            list = (toList my_map)
            sorted_list :: [(PhraseCounterKey, PhraseCounterValue)]
            sorted_list = sortBy (\a -> \b -> (compare (fst a) (fst b))) list
            conv :: (PhraseCounterKey,PhraseCounterValue) -> PhraseCounterKey
            conv (str,myint) = (show myint) ++ "\t" ++ str ++ "\n"

addPhrase :: PhraseCounterKey -> PhraseCounter -> PhraseCounter
addPhrase key my_map = insertWith (+) key 1 my_map

church_N 0 f x = x
church_N n f x = (f (church_N (n-1) f x))

mysplit :: Eq a => [a] -> [a] -> [[a]]
mysplit separator [] = [[]]
mysplit separator base =
    let len = (length separator)
    in (if ((take len base) == separator)
        then []:(mysplit separator (church_N len tail base))
        else let ret = (mysplit separator (tail base))
             in (head(base):head(ret)) : tail(ret)
       )

-- TODO : Handle the empty line at the end (due to the trailing "\n")
string_to_phraseCounter :: String -> PhraseCounter
string_to_phraseCounter mystring = (Map.fromList phrases) where
    phrases = (List.map line_to_tuple
        (List.filter (\x -> (length x) > 0) (lines mystring))
        )
    line_to_tuple :: String -> (PhraseCounterKey,PhraseCounterValue)
    line_to_tuple mystr = (key,val) where
        (valString, key_with_tab) = List.break (\c -> c=='\t') mystr
        key = tail key_with_tab
        val = (read valString)

is_ws ' ' = True
is_ws '\n' = True
is_ws '\t' = True
is_ws _ = False


add_line :: String -> PhraseCounter -> PhraseCounter
add_line line pc = Data.List.foldl' rap pc (words line) where
    rap a b = addPhrase b a

analyze_file :: Handle -> PhraseCounter -> IO PhraseCounter
analyze_file fh pc =
    do line <- (hGetLine fh)
       eof <- hIsEOF fh
       if eof
          then return pc
          else do new_pc <- (analyze_file fh (add_line line pc))
                  return new_pc

-- For testing.

create_empty :: PhraseCounter
create_empty = empty

pc1 :: PhraseCounter
pc1 = (Map.insert "Hello" 5 empty)

pc2 :: PhraseCounter
pc2 = (Map.insert "AFish" 100 pc1)

pc3 = addPhrase "Hello" pc2

main :: IO()
-- main = do putStr (phraseCounter_to_string pc3)

main = do
    args <- getArgs
    fh <- openFile (args !! 0) ReadMode
    final_pc <- (analyze_file fh empty)
    putStr (phraseCounter_to_string final_pc)
    hClose fh

