
module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = String

--TODO Task 1.2
type Val = Int

--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr =
        LOADI Val |
        LOAD Vname |
        ADD |
        STORE Vname |
        JMP Val |
        JMPLESS Val |
        JMPGE Val
        deriving (Eq, Read, Show)

--TODO Task 1.5
--type Stack = [Int]
type Stack = [Val]

--TODO Task 1.6
--type Config = (Int, State, [Int])
type Config = (Val, State, [Val])

--TODO Task 1.7
--loads x onto the top of the stack using :
--loads the corresponding value of v onto the stack 
--adds two values and drops the two values added from the stack using Prelude.drop
--Stores the top value of the stack to v and drops that value again using Prelude.drop
--Increases the program counter by an additional i
-- if the top of the stack is greater than the next value, the program counter is increased by i and those values are dropped from the stack
-- not sure if i should have the drop 2 for this one
-- if the top of the stack is less than or equal to the next value, the program counter is increased by i and those values are dropped from the stack

iexec :: Instr -> Config -> Config
iexec (LOADI x) (counter, state, stack) = 
        (counter + 1, state, x : stack)
iexec (LOAD v) (counter, state, stack) = 
        (counter + 1, state, (state ! v) : stack)
iexec (ADD) (counter, state, stack) = 
        (counter + 1, state, ((stack !! 0) + (stack !! 1)) : Prelude.drop 2 stack)
iexec (STORE v) (counter, state, stack) = 
        (counter + 1, (insert v (stack !! 0) state), Prelude.drop 1 stack)

iexec (JMP i) (counter, state, stack) = 
        (counter + 1 + i, state, stack)
-- not sure if need to drop 2 no matter what
iexec (JMPLESS i) (counter, state, stack) = 
        if (head stack > stack !! 1) then (counter + i + 1, state, stack) else (counter + 1, state, Prelude.drop 2 stack)
iexec (JMPGE i) (counter, state, stack) = 
        if (stack !! 1 >= head stack) then (counter + i + 1, state, Prelude.drop 2 stack) else (counter + 1, state, Prelude.drop 2 stack)


--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] config = config
exec (x:xs) config = exec xs (iexec x config)

