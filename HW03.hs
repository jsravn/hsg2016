module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name val x = if x == name then val else state x

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var name)    = st name
evalE _ (Val value)    = value
evalE st (Op le op re) = case op of
    Plus   -> l + r
    Minus  -> l - r
    Times  -> l * r
    Divide -> l `div` r
    Gt     -> toInt $ l > r
    Ge     -> toInt $ l >= r
    Lt     -> toInt $ l < r
    Le     -> toInt $ l <= r
    Eql    -> toInt $ l == r

    where
        l = evalE st le
        r = evalE st re
        toInt b = if b then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name e)  = DAssign name e
desugar (Incr name)      = DAssign name (Op (Var name) Plus (Val 1))
desugar (If e s1 s2)     = DIf e (desugar s1) (desugar s2)
desugar (While e s)      = DWhile e (desugar s)
desugar (For s1 e s2 s3) =
    DSequence
        (desugar s1)
        (DWhile e (DSequence (desugar s3) (desugar s2)))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip             = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st ds = case ds of
    DAssign name e      -> extend st name (evalE st e)
    DIf e ds1 ds2
        | evalE' e == 1 -> evalSimple' ds1
        | otherwise     -> evalSimple' ds2
    DWhile e ds1
        | evalE' e == 1 -> evalSimple' $ DSequence ds1 (DWhile e ds1)
        | otherwise     -> st
    DSequence ds1 ds2   -> evalSimple (evalSimple' ds1) ds2
    DSkip               -> st

    where
        evalE'      = evalE st
        evalSimple' = evalSimple st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
