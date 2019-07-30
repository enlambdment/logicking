data P    = P    deriving (Eq, Show)  --p
data D    = D    deriving (Eq, Show)  --`
data Neg  = Neg  deriving (Eq, Show)  --negation symbol
data Impl = Impl deriving (Eq, Show)  --implication symbol

data PSym =      --propositional symbol
   Init D P      --consists of p with
 | Rec  D PSym   --1 or more dashes, `
   deriving (Eq, Show)

data Wff t =                
   S           t            --prop. sym. is a w.f.f
 | Negation    Neg  (Wff t) --for A w.f.f, ~A w.f.f.
 | Implication (Wff t) Impl (Wff t) --A, B w.f.f => A -> B w.f.f
   deriving (Eq, Show)

--finite, small set of prop. sym.s to play around with
data MySym = P1 | P2 | P3
   deriving (Eq, Show)

--now you can have concrete types e.g.
--data PWff = Wff MySym
--with prop. symbols coming from a type of your choosing

type BVal t = (->) t Bool  --this breaks with 'data' instead of 'type'!

--given a BVal t, how to produce a BVal (Wff t)?
phi :: BVal t -> BVal (Wff t)
phi f = \w -> case w of
   S v
    | f v == True         -> True
    | otherwise           -> False
   Negation Neg w'
    | (phi f) w' == True  -> False
    | otherwise           -> True
   Implication w1 Impl w2
    | (phi f) w1 == False -> True
    | (phi f) w2 == True  -> True
    | otherwise           -> False


