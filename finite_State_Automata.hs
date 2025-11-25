--making finite state automata
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

-- returns states of a FSA
states :: FSA q -> [q]
states (q, _, _, _, _) = q

-- returns alphabet
alpha :: FSA a -> Alphabet
alpha (_, a, _, _, _) = a

start :: FSA q -> [q]
start (_, _, q, _, _) = q

final :: FSA q -> [q]
final (_, _, _, q, _) = q

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

--Closure

--function
next :: (Eq q) => [Transition q] -> Char -> [q] -> [q]
next trans x ss = 
    [q1 | (q, y, q1) <- trans, x == y, q `elem` ss]

--function applys the transitions for given symbol x to move to start states
step :: (Eq q) => FSA q -> Char -> FSA q
step (qs, as, ss, fs, ts) x = (qs, as, (next ts x ss), fs, ts) 

--deterministic
deterministic :: (Eq q) => FSA q -> Bool
deterministic (qs, as, ss, fs, ts) = 
    (length ss == 1) 
    && 
    and [r == q1 | (q0, a, q1) <- ts, r <- qs, (q0, a, r) `elem` ts]

--toDFA
--toDFA :: (Ord q ) => FSA q -> FSA [q]