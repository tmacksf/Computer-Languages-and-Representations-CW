module Main where

import System.Environment
import Compiler
import Interpreter


--TODO Task 3.4
main :: IO ()
main = do
    (x:xs) <- getArgs
    putStrLn (show(ccomp (read x::Com)))
