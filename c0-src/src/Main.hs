module Main where

import Parser
import Lexer
import AST
import Typecheck
import IntermCode
import FinalCode
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do
  txt <- getContents
  let str = (alexScanTokens txt)
  putStrLn "--------------"
  putStrLn "-Tokens:"
  print str
  let program = parser $ str
  putStrLn "--------------"
  putStrLn "-Program:"
  print program
  putStrLn "--------------"
  putStrLn "-Env:"
  let env = checkProgram Map.empty program
  print env
  putStrLn "--------------"
  putStrLn "-Codigo intermedio:"
  let interm = runState (transProgram program Map.empty) (0,0)
  print interm
  let final = finalTransProg (fst interm)
  putStrLn "--------------"
  putStrLn "-Codigo final:"
  putStrLn final

buildTree :: String -> Program
buildTree str = parser $ alexScanTokens str
