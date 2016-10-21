
-- Assignment 4 Part I, CSCE-314
-- Section: PUT YOUR TEAM MEMBERS' SECTIONS (202 or 502) HERE
-- Matt Gaikema: 923008006
-- Jason Zuang: 

module Main where

import Prelude hiding (lookup)
import Data.List hiding (lookup)
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


-- eval function
eval :: WExp -> Memory -> WValue
-- Added error patterns so we wouldn't get non-exhaustive pattern matching error upon runtime, 
-- but this isn't perfect because the W program doesn't work.

eval (Val a) m = a

eval (Var a) m | lookup a m == Nothing = error $"Undefined variable."++ a
               | otherwise = eval (Val (fromJust(lookup a m))) m
--ahhhh this makes sense
--eval ((Plus (Val a) (Val b))) m = VInt ((asInt (eval (Val a) m) ) + (asInt (eval (Val b) m)))
eval ((Plus a b )) m = VInt ((asInt (eval a m)) + (asInt (eval b m)))
eval (Plus _ _) m = error "Bad Plus"

eval (Mult a b) m = VInt ((asInt (eval a m) ) * (asInt (eval b m)))
eval (Mult _ _) m = error "Bad mult"

eval (Equals a b) m = VBool ((asInt (eval a m)) == (asInt (eval b m)))
eval (Equals _ _) m = error "Bad Equals"

eval (NotEqual a b) m = VBool ((asInt (eval a m)) /= (asInt (eval b m)))
eval (NotEqual _ _) m = error "Bad Not Equal"

eval (Less a b) m = VBool ((asInt (eval a m)) < (asInt (eval b m)))
eval (Less _ _) m = error "Bad Less"

eval (Greater a b) m = VBool ((asInt (eval a m)) > (asInt (eval b m)))
eval (Greater _ _) m = error "Bad Greater"

eval (LessOrEq a b) m = VBool ((asInt (eval a m)) <= (asInt (eval b m)))
eval (LessOrEq _ _) m = error "Bad LessorEq"

--eval (GreaterOrEq (Val a)(Val b)) m = VBool ((asInt (eval (Val a) m)) >= (asInt (eval (Val b) m)))
eval (GreaterOrEq a b) m = VBool ((asInt (eval a m)) >= (asInt (eval b m)))
eval (GreaterOrEq _ _) m = error "Bad Greater or Eq"

eval (And (Val (VBool a)) (Val (VBool b) )) m = VBool ((asBool (eval (Val (VBool a) ) m)) && (asBool (eval (Val (VBool b) ) m)))
eval (And _ _) m = error "Bad And"

--eval (Or (Val (VBool a)) (Val (VBool b) )) m = VBool ((asBool (eval (Val (VBool a) ) m)) || (asBool (eval (Val (VBool b) ) m)))
eval (Or a b) m = VBool((asBool (eval a m)) || (asBool (eval b m)))
eval (Or _ _) m = error "Bad or"

--eval (Not (Val (VBool a))) m = VBool (not (asBool (eval (Val (VBool a) ) m)))
eval (Not a) m = VBool (not (asBool (eval a m)))
eval (Not _) m = error "Bad Not"

-- stupid helper function which deals with block statements.
--im trying to write a function that looks for the ("|",VMarker) and then deletes everything up to it

-- exec function
exec :: WStmt -> Memory -> Memory

exec Empty m = m

-- Assign a variable.
-- Lookup the variable a in the stack thing.
-- If it doesn't exist, throw an error.
-- Otherwise, add it to the stack thing.
exec (Assign a b) m | lookup a m == Nothing = error "This value does not exist."
                    | otherwise = replace a b m
                    where replace _ _ [] = []
                          replace a b (x@(k,_):xs)| a == k = (k,(eval b m)):xs
                                                  | otherwise = x:replace a b xs

-- Declare a variable.
exec (VarDecl a b) m | lookup a m == Nothing = (a, eval b m):m
                     | otherwise = error "This value is already declared."

-- If statement                     
exec (If w s1 s2) m | eval w m == VBool(True) = exec s1 m
                    | otherwise = exec s2 m

exec (While w s) m | eval w m == VBool(True) = exec (While w s) (exec s m)
                   | eval w m == VBool(False) = m
                   | otherwise = error "Error"
                                 
-- Execute a block of code.
-- Execute the first statement in the block,
-- and then call exec on the rest of the block and the resulting memory.

--exec (Block (x:xs) ) m = exec (Block xs) (exec x m)
-- Add a marker then call sexec.
exec (Block xs) m = popMarker $ foldr exec (marker:m) (reverse xs)
                  where popMarker [] = []
                        popMarker (x:xs) | isMarker x = xs
                                         | otherwise = popMarker xs

--testcases
--plus
t1 = Plus(Val(VInt 1)) (Val(VInt 1))
--mult
t2 = Mult(Val (VInt 2)) (Val(VInt 4))


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
--used for tests
prog02 = Block
  [
  VarDecl "a" (Val(VInt 1)),
 While(Not (Greater (Var "a") (Val(VInt 3))))
  (
    Assign "a" (Plus (Var "a") (Val (VInt 1)))
  )
  ]
fibonacci = Block
  [
      VarDecl "counter" (Val(VInt 1)),
      VarDecl "a" (Val(VInt 0)),
      VarDecl "b" (Val(VInt 1)),
      VarDecl "c" (Val(VInt 0)),
      While(LessOrEq (Var "counter")(Var "n"))
      (
        Block
        [
          Assign "c" (Plus (Var "a") (Var "b")),
          Assign "b" (Var "a"),
          Assign "a" (Var "c"),
          Assign "counter" (Plus (Var "counter") (Val (VInt (1))))
        ]
      ),
      Assign "n" (Var "c")
   ]
-- some useful helper functions
-- lookup :: String -> Memory -> Maybe WValue
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

