module Main where
  import Control.Monad
  import Data.Char

  term :: Char -> IO Char
  term lookahead =
    if isDigit lookahead
      then do
        putChar lookahead
        getChar
      else do
        putStrLn ""
        ioError $ userError "syntax error"
  
  matchTerm :: IO Char
  matchTerm = do
    lookahead <- getChar
    term lookahead

  rest' :: Char -> IO ()
  rest' c = do
    lookahead <- matchTerm
    putChar c
    rest lookahead

  rest :: Char -> IO ()
  rest lookahead =
    if lookahead == '+'
      then rest' '+'
      else when (lookahead == '-') $ rest' '-'

  expr :: IO ()
  expr = do
    lookahead <- matchTerm
    _ <- rest lookahead
    return ()

  main :: IO ()
  main = do
    expr
    putStrLn ""