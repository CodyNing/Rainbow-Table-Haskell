import RainbowAssign
import System.Random ()
import qualified Data.Map as Map
import Data.Maybe as Maybe

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

pwReduce :: Hash  -> Passwd
pwReduce hash = map toLetter baseLst
    where
        baseConv :: Int -> Int -> [Int] -> [Int]
        baseConv 0 _ lst = lst
        baseConv digit 0 lst = 0 : baseConv (digit - 1) 0 lst
        baseConv digit hash' lst = baseConv (digit- 1) (hash' `div` nLetters) lst ++ [hash' `mod` nLetters]
        intHash = fromEnum hash
        baseLst = baseConv pwLength intHash []


rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable tbWidth pwds = Map.fromList $ zip (map (finalHash tbWidth) pwds) pwds
    where
        finalHash 0 pwd = pwHash pwd
        finalHash width' pwd = finalHash (width' - 1) (pwReduce $ pwHash pwd)


generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename


findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table tbWidth hash = do
    let rows = findRows [] (Map.lookup hash table) tbWidth hash
    getFirstEntry rows tbWidth hash
    where
        findRows :: [Passwd] -> Maybe Passwd -> Int -> Hash -> [Passwd]
        findRows acc _ (-1) _ = acc
        findRows acc value width' hash'
            | isNothing value = findRows acc (Map.lookup newHash table) (width' - 1) newHash
            | otherwise = findRows (acc ++ [fromJust value]) (Map.lookup newHash table) (width' - 1) newHash
            where
                newHash = pwHash $ pwReduce hash'
        findEntry :: Passwd -> Int -> Hash -> Maybe Passwd
        findEntry _ (-1) _ = Nothing 
        findEntry rowPw width' hash'
            | pwHash rowPw == hash' = Just rowPw
            | otherwise = findEntry (pwReduce $ pwHash rowPw) (width' - 1) hash'
        getFirstEntry :: [Passwd] -> Int -> Hash -> Maybe Passwd
        getFirstEntry [] _ _ = Nothing 
        getFirstEntry (r : rs) width' hash'
            | isNothing entry = getFirstEntry rs width' hash'
            | otherwise = entry
            where entry = findEntry r width' hash'

test1 :: IO (Maybe Passwd)
test1 = do
  table <- readTable filename
  return (Map.lookup 0 table)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)

main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res