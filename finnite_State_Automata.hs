--making finite state automata
-- each state in finite state automata will take the form Node startstate transition endstate
-- e.g. Node 0 a 1 <- going from node 0 to node 1 via transition a 
data Fin_State = Empty | Node Int Char Int
    deriving(Show, Eq, Ord)

-- addState

-- addTransition

--accepts

--Closure

--next

--determinnistic

-- toDFA