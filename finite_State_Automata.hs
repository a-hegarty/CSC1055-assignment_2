--making finite state automata
import Data.List

type FSA q = ([q], Alphabet, [q], [q], [Transition q])
type Alphabet = [Char]
type Transition t = (t, Char, t)

m1 :: FSA Int
m1 = ([0, 1, 2, 3, 4],
    ['a', 'b'],
    [0],
    [4],
    [(0,'a',1), 
    (0,'b',1), 
    (0,'a',2), 
    (0,'b',2),
    (1,'b',4), 
    (2,'a',3), 
    (2,'b',3), 
    (3,'b',4),
    (4,'a',4), 
    (4,'b',4)])

m2 :: FSA Int
m2 = ([0, 1, 2, 3, 4, 5, 6, 7, 8 ,9, 10],
    ['a', 'b'],
    [0],
    [10],
    [(0, 'e', 1),
    (0, 'e', 7),
    (1, 'e', 2),
    (1, 'e', 4),
    (2, 'a', 3),
    (4, 'b', 5),
    (3, 'e', 6),
    (5, 'e', 6),
    (6, 'e', 1),
    (6, 'e', 7), 
    (7, 'a', 8), 
    (8, 'b', 9),
    (9, 'b', 10)])

-- returns states of a FSA
states :: FSA q -> [q]
states (q, _, _, _, _) = q

-- returns alphabet
alpha :: FSA a -> Alphabet
alpha (_, a, _, _, _) = a

--returns list of start state
start :: FSA q -> [q]
start (_, _, q, _, _) = q

--returns list of end states
final :: FSA q -> [q]
final (_, _, _, q, _) = q

--returns list of transitions
trans :: FSA q -> [Transition q]
trans (_, _, _, _, q) = q

delta :: (Eq q) => FSA q -> q -> Char -> [q]
delta m q c = 
    [q1 | (q0, c0, q1) <- trans m, q0 == q, c0 == c]

-- function adds state to list of states in first element of data type FSA
addState :: (Eq q) => FSA q -> q -> [q]
addState (q, _, _, _, _) s = s:q

-- function adds a transition to the established list of transitions
addTransition :: FSA q -> Transition q -> [Transition q]
addTransition (_, _, _, _, q) t = t:q

--function returns bool value of whether a given string will be accepted by the FSA
-- i.e. if the string provides a valid path through the states of the machine
accepts :: (Eq q) => FSA q -> String -> Bool
accepts (_, _, ss, fs, ts) "" = or[q `elem` ss | q <- fs]
accepts m (x:xs) = accepts (step m x) xs

--function displays the list of states that can be accessed from a given state only using empty transitions
-- function must be called in the form closure FSA [State]
closure :: (Eq q) => FSA q -> [q] -> [q]
closure (qs, as, ss, fs, ts) s =
    let next_State = [q1| (q0, a, q1) <- ts, q0 `elem` qs, a == 'e'] 
        further_state = nub(s ++ next_State)
    in if further_state == s
        then s
        else closure (qs, as, ss, fs, ts) further_state

--function
next :: (Eq q) => [Transition q] -> Char -> [q] -> [q]
next trans x ss = 
    [q1 | (q, y, q1) <- trans, x == y, q `elem` ss]

--function applys the transitions for given symbol x to move to start states
step :: (Eq q) => FSA q -> Char -> FSA q
step (qs, as, ss, fs, ts) x = (qs, as, (next ts x ss), fs, ts) 

--function returns whether or not a given fsm is deterministic or not
deterministic :: (Eq q) => FSA q -> Bool
deterministic (qs, as, ss, fs, ts) = 
    (length ss == 1) 
    && 
    and [r == q1 | (q, a, q1) <- ts, r <- qs, (q, a, r) `elem` ts]

--function takes a nfa and turns it to a dfa
toDFA :: (Ord q) => FSA q -> FSA q
toDFA m = m

reach :: (Ord q) => (Char -> [q] -> [q]) -> [Char] -> [q] -> [[q]]
reach step as ss =
    let add qss qs = if qs `elem` qss 
        then qss
        else foldl add (qs : qss) [ canonical $ step s qs | s <- as ]
    in add [] (canonical ss)

canonical :: (Ord q) => [q] -> [q]
canonical q = sort(nub q)