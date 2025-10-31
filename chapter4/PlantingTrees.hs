{-# LANGUAGE LambdaCase #-}

module PlantingTrees where

import Data.Matrix

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

data BinaryTreePath = LeftB | RightB deriving (Eq)

-- binary tree may not be balanced so we need to calc depth

height :: BinaryTree a -> Int
height = \case
  Leaf -> 0
  (Branch l _ r) -> 1 + max (height l) (height r)

extraRowsList :: [Int]
extraRowsList = 1 : 1 : [1,3..]

calcExtraRows :: Int -> Int
calcExtraRows n = sum $ take n extraRowsList

emptyMatrix :: Int -> Matrix (Maybe a)
emptyMatrix height' =
  fromList (max 1 ((height' - 1) + calcExtraRows height'))
           (1 + 2^height')
           (repeat Nothing)

calcJ :: Int -> Int -> Int
calcJ height' offset = (2^ max 0 (height' - 1)) + offset + 1

-- | Returns list of "/" characters to represent left branching
mkLeftBranches :: (Int,Int) -> Int -> [(Maybe String,Int,Int)]
mkLeftBranches (i,j) levels = case levels of
  0 -> []
  _ -> (Just "/",i+1,j-1) : mkLeftBranches (i+1,j-1) (levels-1)

-- | Returns list of "/" characters to represent left branching
mkRightBranches :: (Int,Int) -> Int -> [(Maybe String,Int,Int)]
mkRightBranches (i,j) levels = case levels of
  0 -> []
  _ -> (Just "\\",i+1,j+1) : mkRightBranches (i+1,j+1) (levels-1)

matrixUpdates :: Show a       =>
                 Int          ->
                 Int          ->
                 Int          ->
                 BinaryTree a ->
                 [(Maybe String,Int,Int)]
matrixUpdates depth height' offset = \case
  Leaf -> []
  (Branch l c r) -> do
    let i = depth+1
        j = calcJ height' offset
        gap = if height' > 1 then extraRowsList!!(height'-1) else 0
        lBranch = matrixUpdates (depth + gap + 1) (height' - 1) offset l
        rBranch = matrixUpdates (depth + gap + 1) (height' - 1) (offset + 2^(height' - 1)) r
        lBranches = if null lBranch then [] else mkLeftBranches (i,j) gap
        rBranches = if null rBranch then [] else mkRightBranches (i,j) gap

    (Just (show c), i, j) : lBranch <> rBranch <> rBranches <> lBranches


mkMatrixTree :: Show a => BinaryTree a -> Matrix (Maybe String)
mkMatrixTree t = do
  let height' = height t
      emptyMatrix' = emptyMatrix height'
      updates = matrixUpdates 0 height' 0 t
  foldr (\(e,i,j) acc -> setElem e (i,j) acc) emptyMatrix' updates

showNode :: Maybe String -> String
showNode = \case
  Nothing -> " "
  Just s  -> s

showStringTree :: Show a => BinaryTree a -> String
showStringTree t = do
  let m = mkMatrixTree t
      ll = toLists m
  unlines $ map (foldr (\cur acc -> showNode cur <> acc) "") ll

-- | Creates new BinaryTree from `t` with location at `path` updated with `x`
updateTree :: a -> [BinaryTreePath] -> BinaryTree a -> BinaryTree a
updateTree x path t = if null path
  then Branch Leaf x Leaf
  else do
    let (h:rest) = path
    case t of
      Leaf           -> Branch Leaf x Leaf
      (Branch l c r) -> if h == LeftB
                        then Branch (updateTree x rest l) c r
                        else Branch l c (updateTree x rest r)

addElementToIntTree' :: [BinaryTreePath] -> BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree' path t x = case t of
  Leaf -> updateTree x path t
  (Branch l c r) -> if x >= c
                    then Branch l c $ addElementToIntTree' (RightB : path) r x
                    else Branch (addElementToIntTree' (LeftB : path) l x) c r

-- | Add `x` to tree `t` in sorted order
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree = addElementToIntTree' []

addElementsToIntTree :: BinaryTree Int -> [Int] -> BinaryTree Int
addElementsToIntTree t = \case
  [] -> t
  (x:xs) -> addElementsToIntTree (addElementToIntTree t x) xs

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist t x = case t of
  Leaf           -> False
  (Branch l c r) -> x == c || if x > c then doesIntExist r x else doesIntExist l x
