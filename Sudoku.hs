module Main where

import Data.Ord (comparing)
import Data.List ((\\))
import Data.Char (intToDigit, digitToInt)
import Data.Maybe (isNothing)
import qualified Data.Vector as V
import qualified Data.BitSet.Word as S
import qualified Data.Vector.Mutable as MV

import Control.Monad
import Control.Monad.ST

type Board = V.Vector (S.BitSet Int)


readBoard :: String -> Board
readBoard = V.fromList . map charToPlace
  where
    charToPlace '0' = S.fromList [1..9]
    charToPlace '.' = S.fromList [1..9]
    charToPlace c   = S.singleton $ digitToInt c

showBoard :: Board -> String
showBoard = V.toList . V.map showPart
  where
    showPart set =
        if S.size set == 1 then
            intToDigit $ head $ S.toList set 
        else '?'

toIndex :: (Int, Int) -> Int
toIndex (x, y) = y * 9 + x

fromIndex :: Int -> (Int, Int)
fromIndex i =
    let x = i `mod` 9
        y = i `div` 9
    in (x, y)
       
currentRow :: Int -> [Int]
currentRow i =
    let (x, y) = fromIndex i in
    map (\r -> toIndex (r, y)) $ [0..8] \\ [x]
    
currentColumn :: Int -> [Int]
currentColumn i = 
    let (x, y) = fromIndex i in
    map (\r -> toIndex (x, r)) $ [0..8] \\ [y]

currentBlock :: Int -> [Int]
currentBlock i = do
    let (x, y) = fromIndex i 
    let (bx, by) = (3 * (x `div` 3), 3 * (y `div` 3))
    xs <- [bx .. (bx + 2)]
    ys <- [by .. (by + 2)]
    guard (xs /= x || ys /= y)
    let pairs = (xs, ys)
    return $ toIndex pairs

validBoard :: Board -> Bool
validBoard = isNothing . V.find S.null

constraintProp :: Board -> Board
constraintProp = V.modify inplaceProp

inplaceProp :: MV.STVector a (S.BitSet Int) -> ST a ()
inplaceProp v = do
    checked <- MV.replicate 81 False
    update checked 0 False
    where
      diffAll set indices = 
          forM_ indices $ \i -> do
              s <- MV.read v i
              MV.write v i (S.difference s set)

      update c 81 True = update c 0 False
      update _ 81 False = return ()
      update checked i done = do
          c   <- MV.read checked i
          set <- MV.read v i
          if c || S.size set > 1 then 
              update checked (i + 1) done
          else do
              MV.write checked i True
              diffAll set (currentRow i)
              diffAll set (currentColumn i)
              diffAll set (currentBlock i)
              update checked (i + 1) True

solved :: Board -> Bool
solved =
    V.all (\s -> S.size s == 1)
    
solve :: Board -> [Board]
solve board =
    if validBoard board then
        dfs $ constraintProp board 
    else
        []
    where
      fixIndex vec idx item =
          V.update vec $ V.singleton (idx, S.singleton item)

      cmp set =
          let s = S.size set in
          if s == 1 then 10 else s
                                 
      dfs b 
          | solved b = [b]
          | not $ validBoard b = []
          | otherwise =
            let idx = V.minIndexBy (comparing cmp) b
                set = b V.! idx 
            in concatMap (solve . fixIndex b idx) $ S.toList set

prettyBoard :: Board -> String
prettyBoard board = V.toList $
                    V.concatMap (V.fromList . pretty) $
                    V.indexed board
  where
    padding x y 
        | x == 3 || x == 6 = " | "
        | x == 0 && (y == 3 || y == 6) = "\n----------------------\n "
        | x == 0 = "\n "
        | otherwise = " "
    prettyElem idx item =
        let (x, y) = fromIndex idx
        in padding x y ++ show item
    pretty (idx, set) = prettyElem idx $ head $ S.toList set

    
main :: IO ()
main = do
    board <- liftM readBoard getLine
    case solve board of
        [] -> putStrLn "No solution"
        (sln:_) -> putStrLn $ prettyBoard sln
        
    main
