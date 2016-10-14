
-- Assignment 4 Part I, CSCE-314
-- Section: PUT YOUR TEAM MEMBERS' SECTIONS (202 or 502) HERE
-- Matt Gaikema: 923008006
-- Jason Zuang: 

module Main where

import Prelude hiding (lookup)

import Test.HUnit
import System.Exit

-- Haskell data types for W
data WValue = VInt Int 
            | VBool Bool
            | VMarker
              deriving (Eq, Show)

data WExp = Val WValue
          | Var String

          | Plus WExp WExp
          | Mult WExp WExp

          | Equals      WExp WExp
          | NotEqual    WExp WExp
          | Less        WExp WExp
          | Greater     WExp WExp
          | LessOrEq    WExp WExp
          | GreaterOrEq WExp WExp

          | And  WExp WExp
          | Or   WExp WExp
          | Not  WExp
            deriving Show

data WStmt = Empty
           | VarDecl String WExp
           | Assign  String WExp
           | If      WExp   WStmt WStmt
           | While   WExp   WStmt
           | Block  [WStmt]
             deriving Show

type Memory = [(String, WValue)]
marker = ("|", VMarker)
isMarker (x, _) = x == "|"

--helper to convert the WExp to WValue
wplus :: WValue -> WValue ->WValue
wplus (VInt a) (VInt b) = VInt(a+b)

-- eval function
eval :: WExp -> Memory -> WValue
-- I guess just define the operations for each type thingy.
eval (Val a) m = a
--eval (Var a) m = a
eval (Plus (Val a) (Val b)) m = (wplus a b)


eval (Mult (Val a) (Val b)) m = VInt ((asInt a)*(asInt b))
-- Cast as a bool or whatever.
eval (Equals a b) m = VBool (a == b)
eval (NotEqual a b) m = VBool (a /= b)
eval (Less a b) m = VBool (a < b)
eval (Greater a b) m = VBool (a > b)
eval (LessOrEq a b) m = VBool (a <= b)
eval (GreaterOrEq a b) m = VBool (a >= b)
-- Idk if theres a better way than all these nested value constructors.
eval (And (Val (VBool a)) (Val (VBool b) )) m = VBool (a && b)
eval (Or (Val (VBool a)) (Val (VBool b) )) m = VBool (a || b)
eval (Not (Val (VBool a))) m = VBool (not a)

-- exec function
exec :: WStmt -> Memory -> Memory
exec Empty m = m
-- Assign a variable.
-- Lookup the variable a in the stack thing.
-- If it doesn't exist, throw an error.
-- Otherwise, add it to the stack thing.
exec (Assign a b) m | lookup a m == Nothing = error "This value does not exist."
                    | otherwise = (a, eval b m):m
-- Declare a Val.
exec (VarDecl a b) m | lookup a m == Nothing = (a, eval b m):m
                     | otherwise = error "This value is already declared."


-- example programs
prog1 = Block
   [
     VarDecl "x" (Val (VInt 0)),
     VarDecl "y" (Val (VInt 1)),
     VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
     If (Or (Var "b") (Not (GreaterOrEq (Var "x") (Val (VInt 0)))))
        ( Block [ Assign "x" (Val (VInt 1)),
                  Assign "y" (Plus (Var "y") (Val (VInt 1)))
                ] 
        )
        ( Assign "x" (Val (VInt 2)) )
  ]

factorial = Block
  [
     VarDecl "acc" (Val (VInt 1)),
     While (Greater (Var "arg") (Val (VInt 0)))
     ( Block
       [ Assign "acc" (Mult (Var "acc") (Var "arg")),
         Assign "arg" (Plus (Var "arg") (Val (VInt (-1))))         
       ]
     ),
     Assign "result" (Var "acc")
  ]

-- some useful helper functions
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs

asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"

-- unit tests
myTestList =
  TestList [
    test $ assertEqual "prog1 test" [] (exec prog1 []),

    let res = lookup "result" (
                exec factorial [("result", VInt (-1)), ("arg", VInt 10)])
    in test $ assertBool "factorial of 10" (3628800 == asInt (fromJust res))
    ]    

-- main: run the unit tests  
main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          if (errs + fails /= 0) then exitFailure else return ()

