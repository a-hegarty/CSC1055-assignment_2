--making finite state automata
type FSA q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition t = (t, Char, t)

m1 :: FSA Int
m1 = ([0, 1, 2, 3, 4],
    ['a', 'b'],
    0,
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

start :: FSA q -> q
start (_, _, q, _, _) = q

final :: FSA q -> [q]
final (_, _, _, q, _) = q

trans :: FSA q -> [Transition q]
trans (_, _, _, _, q) = q

-- addState

-- addTransition

--accepts

--Closure

--next

--determinnistic

-- toDFA