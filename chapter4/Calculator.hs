module Calculator where

import Text.Read (readEither)
import GHC.StgToJS.Types (StaticLit(BoolLit))

data Expr = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

eval :: Expr -> Int
eval expr =
  case expr of
    Lit num       -> num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
      eval' operator arg1 arg2 =
        operator (eval arg1) (eval arg2)

safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num       -> Right num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> case arg2 of
                       Lit num -> if num == 0
                                  then Left "Error: division by zero"
                                  else eval' div arg1 arg2
                       _       -> eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
      eval' operator arg1 arg2 = case (safeEval arg1, safeEval arg2) of
        (Left s, _)         -> Left s
        (_, Left s')        -> Left s'
        (Right i, Right i') -> Right $ operator i i'

parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err       -> Left err
    Right (e,[])   -> Right e
    Right (_,rest) -> Left $ "Found extra tokens: " <> (unwords rest)
  where
    parse' :: [String] -> Either String (Expr, [String])
    parse' [] = Left "unexpected end of expression"
    parse' (token:rest) =
      case token of
        "+" -> parseBinary Add rest
        "*" -> parseBinary Mul rest
        "-" -> parseBinary Sub rest
        "/" -> parseBinary Div rest
        lit ->
          case readEither lit of
            Left err -> Left err
            Right lit' -> Right (Lit lit', rest)
      where
        parseBinary ::
          (Expr -> Expr -> Expr)
          -> [String]
          -> Either String (Expr, [String])
        parseBinary exprConstructor args =
          case parse' args of
            Left err -> Left err
            Right (firstArg,rest') ->
              case parse' rest' of
                Left err -> Left err
                Right (secondArg,rest'') ->
                  Right (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let answer = show $ eval expr'
      in "The answer is: " <> answer

prettyPrint :: Expr -> String
prettyPrint expr = do
  let rhs = eval expr
      lhs = prettyPrint' expr
  lhs <> " = " <> show rhs
  where
    prettyPrint' :: Expr -> String
    prettyPrint' expr' = case expr' of
        Lit num       -> show num
        Add arg1 arg2 -> print' "+" arg1 arg2
        Sub arg1 arg2 -> print' "-" arg1 arg2
        Mul arg1 arg2 -> print' "×" arg1 arg2
        Div arg1 arg2 -> print' "÷" arg1 arg2
        where
          needsParens :: Expr -> Expr -> Bool
          needsParens _e1 e2 = case e2 of
            Lit n2 -> False
            _      -> True

          print' :: String -> Expr -> Expr -> String
          print' operator arg1 arg2 = do
            let s = prettyPrint' arg1 <> " " <> operator <> " "
            if needsParens arg1 arg2
            then s <> "( " <> prettyPrint' arg2 <> " )"
            else s <> prettyPrint' arg2
