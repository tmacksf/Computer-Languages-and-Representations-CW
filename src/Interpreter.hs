module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp = 
    N Int |
    V String |
    Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N x) state = x
aval (V x) state = state ! x
aval (Plus x y) state = (aval x state) + (aval y state)

--TODO Task 2.1
data BExp =
    Bc Bool |
    Not BExp |
    And BExp BExp |
    Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc x) state = x
bval (Not x) state = not (bval x state)
bval (And (Bc x) (Bc y)) state = x && y
bval (Less x y) state = (aval x state) < (aval y state)

--TODO Task 2.1
data Com =
    Assign String AExp |
    Seq Com Com |
    If BExp Com Com |
    While BExp Com |
    SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State

eval (Assign v x) state = insert v (aval x state) state
eval (Seq x y) state = eval y (eval x state)
eval (If (b) (t) (f)) state = if (bval b state) then (eval t state) else (eval f state)
eval (While bool x) state = if (bval bool state) then (eval (While bool x) (eval x state)) else state
eval SKIP state = state