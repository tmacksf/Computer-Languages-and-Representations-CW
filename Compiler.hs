module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]
acomp (V v) = [LOAD v]
acomp (Plus x y) = acomp x ++ acomp y ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b) bool int = if (b == bool) then [JMP int] else []
bcomp (Not b) bool int = bcomp b (not bool) int
bcomp (And a b) bool int = 
    let x = bcomp a False (if bool then length (bcomp b bool int) else length (bcomp b bool int) + int) in (x ++ (bcomp b bool int))
bcomp (Less a b) bool int = acomp a ++ acomp b ++ if bool then [JMPLESS int] else [JMPGE int]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign a b) = acomp b ++ [STORE a]
ccomp (Seq a b) = ccomp a ++ ccomp b
ccomp (If bexp a b) = bcomp bexp False (length (ccomp a) + 1) ++ (ccomp a) ++ JMP (length (ccomp b)) : (ccomp b)
ccomp (While b c) = do
    let cc = ccomp c 
    let cb = bcomp b False (length cc + 1)
    cb ++ cc ++ [JMP (- (length cb + length cc + 1))]
ccomp (SKIP) = []