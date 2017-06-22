-- Patrick Millais (pm515)

------------------------- State

type Variable = String

type State = [(Variable,Integer)]

type Output = [Integer]

empty :: State
empty = []

set :: Variable -> Integer -> State -> State
set v n [] = [(v,n)]
set v n ((w,m):xs) | v == w = (v,n) : xs
                   | otherwise = (w,m) : set v n xs

get :: Variable -> State -> Integer
get v [] = 0
get v ((w,m):xs) | v == w = m
                 | otherwise = get v xs

------------------------- Sample program

-- factorial :: Comm
-- factorial = ("y" :=: Num 1) :>:
--             While (Num 1 :<=: Var "x")
--               ("y" :=: (Var "y" :*: Decr "x"))

-- runFactorial :: Integer -> Integer
-- runFactorial i = get "y" s
--   where
--     s = evalC factorial (set "x" i empty) 

------------------------- Arithmetic expressions

data Aexp = Num Integer
          | Var Variable
          | Aexp :+: Aexp
          | Aexp :-: Aexp
          | Aexp :*: Aexp
          | Incr Variable
          | Decr Variable
          | PreIncr Variable
          | PreDecr Variable

evalA :: Aexp -> State -> (State, Integer)
evalA (Num n) s   = (s, n)
evalA (Var v) s   = (s, (get v s))
evalA (a :+: b) s = (s2, x + y)
		where
			(s1, x) = evalA a s
			(s2, y) = evalA b s1  
evalA (a :*: b) s = (s2, x * y)
		where 
			(s1, x) = evalA a s
			(s2, y) = evalA b s1
evalA (a :-: b) s = (s2, x - y)
		where 
			(s1, x) = evalA a s
			(s2, y) = evalA b s1
evalA (Incr a) s = (t, n)
		where
			n = (get a s)
			t = set a (n+1) s
evalA (Decr a) s = (t, n)
		where
			n = (get a s)
			t = set a (n-1) s
evalA (PreIncr a) s = (s2, t)
		where 
			s2 = (set a ((get a s)+1) s)
			t = get a s2
evalA (PreDecr a) s = (s2, t)
		where
			s2 = (set a ((get a s)-1) s)
			t = get a s2

------------------------- Boolean expressions

data Bexp = Boolean Bool
          | Aexp :==: Aexp
          | Aexp :<=: Aexp
          | Neg Bexp
          | Bexp :&: Bexp
          | Bexp :|: Bexp
          | Bexp :&&: Bexp
          | Bexp :||: Bexp

evalB :: Bexp -> State -> (State, Bool)
evalB (Boolean b) s = (s, b)
evalB (a :==: b) s = (s2, x==y)
		where 
			(s1, x) = evalA a s
			(s2, y) = evalA b s1
evalB (a :<=: b) s = (s2, x <= y)
		where
			(s1, x) = evalA a s
			(s2, y) = evalA b s1
evalB (Neg b)	s = (s1, x)
		where
			(s1, b2) = evalB b s
			x = not b2
evalB (a :&: b) s = (s2, x && y)
		where
			(s1, x) = evalB a s
			(s2, y) = evalB b s1
evalB (a :|: b) s = (s2, x || y)
		where
			(s1, x) = evalB a s
			(s2, y) = evalB b s1
evalB (a :&&: b) s = (s2, y)
		where
			(s1, x) = evalB a s
			(s2, y) | (x==False) = (s1, x)
					| otherwise = evalB b s1
evalB (a :||: b) s = (s2, y)
		where
			(s1, x) = evalB a s
			(s2, y) | (x==True) = (s1, x)
					| otherwise = evalB b s1

------------------------- Commands

data Comm = Skip
          | Variable :=: Aexp
          | Comm :>: Comm
          | If Bexp Comm Comm
          | While Bexp Comm
          | Print Aexp

evalC :: Comm -> State -> (State, Output)
evalC Skip        s = (s, [])
evalC (v :=: a)   s = (s3, []) where s3 = set v a2 s2  where (s2, a2) = evalA a s
evalC (c :>: d)   s = (u, ab)
			where
				(t, a) = evalC c s
				(u, b) = evalC d t
				ab = a++b
evalC (If b c d)  s | o = evalC c t
					| otherwise = evalC d t
					where
						(t, o) = evalB b s
evalC (While b c) s | o = (v ,o1++o2)
					| otherwise = (t, [])
					where
						(t, o) = evalB b s
						(u, o1) = evalC c t
						(v, o2) = evalC (While b c) u
evalC (Print a)   s = (s2, a3)
			where 
				(s2, a2) = evalA a s
				a3 = [a2]


------------------------- Evaluation of States, b1, b2, b3, b4
st :: State
st = [("x",6), ("y",5), ("z",0)]

b1 :: Bexp
b1 = Boolean False

b2 :: Bexp
b2 = Var "x" :==: PreIncr "y"


{- The evaluation of the expressions (b1 and b2) produces 
the same boolean but returns it in different states due to short-circuiting

*Main> evalB(b1 :&: b2) st
([("x",6),("y",6),("z",0)],False)

*Main> evalB(b1 :&&: b2) st
([("x",6),("y",5),("z",0)],False) -}


b3 :: Bexp
b3 = (Boolean True :|: (Var "x" :==: PreIncr "y")) :&: (Var "y" :==: Num 5)

b4 :: Bexp
b4 = (Boolean True :||: (Var "x" :==: PreIncr "y")) :&&: (Var "y" :==: Num 5)


{- The evaluation of the identical expressions (excluding different operators)
(b3 and b4) produces different booleans with different states

*Main> evalB b3 st
([("x",6),("y",6),("z",0)],False)
*Main> evalB b4 st
([("x",6),("y",5),("z",0)],True) -}

-------------------------