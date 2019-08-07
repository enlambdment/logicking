import Numeric.Natural
import Data.Tree

data Wff t = 
   Sym t
 | Neg  (Wff t)
 | Conj (Wff t) (Wff t)
 | Disj (Wff t) (Wff t)
 | Impl (Wff t) (Wff t)
  deriving (Eq, Show)

--Chapter II. Analytic Tableaux (p. 15)

--product type for signed wff's
--I need record syntax w/ field accessors
--for something that I'll do later
data SignedWff t = Swff {
  bool :: Bool ,
  wff  :: (Wff t) 
  }
  deriving (Eq, Show)

--examples
p = Sym 'p'
q = Sym 'q'
r = Sym 'r'

w1 = p 
w2 = Conj p (Neg p)
w3 = Conj p q
w4 = Disj (Neg p) (Neg q)

sw1 = Swff True w1
sw2 = Swff True w2
sw3 = Swff False w3
sw4 = Swff True w4
sw5 = Swff False (Impl (Neg w4) (w3))

mySw = Swff False
  (Impl
   (Impl p 
    (Impl q r))
   (Impl (Impl p q)
    (Impl p r))
  )

--'we begin by noting that under any interpretation
--the following eight facts hold (for any formulas
--X, Y): '

--given a term :: SignedWff t,
--either return its simplification if there is one,
--or else if none available, Nothing
-- reduce :: SignedWff t -> Maybe (Wff (SignedWff t))
-- reduce (Swff b w) =
--  case (b, w) of
--   (_, Sym x)          -> Nothing
--   (_, Neg w)          -> Just $ Sym  (Swff (not b) w)
--   (True, Conj x y)    -> Just $ Conj (Sym (Swff True x))   (Sym (Swff True y))
--   (False, Disj x y)   -> Just $ Conj (Sym (Swff False x))  (Sym (Swff False y))
--   (False, Impl x y)   -> Just $ Conj (Sym (Swff True x))   (Sym (Swff False y))
--   (True, Disj x y)    -> Just $ Disj (Sym (Swff True x))   (Sym (Swff True y))
--   (False, Conj x y)   -> Just $ Disj (Sym (Swff False x))  (Sym (Swff False y))
--   (True, Impl x y)    -> Just $ Disj (Sym (Swff False x))  (Sym (Swff True y))

--this feels sort of ad hoc but I can't think
--of a better way
type Box t = Either [t] (t, t) 

--data Either [t] (t, t) = 
 --Left [t]      |
 --Right (t, t)

--to construct a *term* of type :: Box t,
--you need to use either
 --Left,       on a [t], or
 --Right,      on a (t,t)

--we will need a function for fleshing out a subforest,
--based upon contents of the immediately preceding 'rootLabel'
--of the node

--trying out some argument capture w/ record syntax
stick_node :: Monoid a => a -> Tree a -> Tree a
stick_node x n@(Node {rootLabel = rl}) =
 n{rootLabel = rl `mappend` x}

--the behavior here has to depend on whether
--(subForest n) = [] or is non-empty
stick_leaves :: Monoid a => a -> Tree a -> Tree a
stick_leaves x n@(Node {subForest = sf}) =
 case sf of
  []               ->  n{subForest = sf ++ [Node x []]}   --use (<>), not (++)
  otherwise        ->  n{subForest = map (stick_node x) sf}
     

--for some simple examples, make a monoid instance
--for Natural

instance Semigroup Natural where
 (<>)      = (+)

instance Monoid Natural where
 mempty    = 0
 mappend   = (<>)

nats_tree :: Tree Natural
nats_tree = 
 Node 4
  [Node 5 [],
   Node 6 [],
   Node 7 []] 


--examples
ls_tree :: Tree [Natural]
ls_tree =
 Node [4,5,6]
 [Node [1,3,5] [],
  Node [1,2,3] [],
  Node [5,4,3] []]

box1 :: Box Natural
box1 = Left [10,11,12]

box2 :: Box Natural
box2 = Right (14,15)


--examples, for a tree of elements types [Swff t]

bxswf1 :: Box (SignedWff Char)
bxswf1 = Left 
 [Swff True $ Sym  'p',
  Swff False $ Sym 'q',
  Swff True $ Sym  'r']

bxswf2 :: Box (SignedWff Char)
bxswf2 = Right 
 (Swff True  $ Sym 'q',
  Swff False $ Sym 'p')

swffs_tree :: Tree [SignedWff Char]
swffs_tree =
 Node [Swff True $ Sym 'a', Swff False $ Sym 'b'] 
  [Node [Swff True $ Sym 'c', Swff False $ Sym 'd'] [],
   Node [Swff True $ Sym 'c', Swff False $ Sym 'd', Swff True $ Sym 'f'] [],
   Node [Swff True $ Sym 'd', Swff False $ Sym 'f'] []]



reduce :: SignedWff t -> Box (SignedWff t)
reduce s@(Swff b w) =
 case (b, w) of 
  (_, Sym x)          -> Left  [s]
  (_, Neg v)          -> Left  [Swff (not b) v]
  --cases on Conj
  (True, Conj u v)    -> Left  [Swff b u,     Swff b v]
  (False, Conj u v)   -> Right (Swff False u, Swff False v)
  --cases on Disj
  (True, Disj u v)    -> Right (Swff True u,  Swff True v)
  (False, Disj u v)   -> Left  [Swff False u, Swff False v]
  --cases on Impl
  (True, Impl u v)    -> Right (Swff False u, Swff True v)
  (False, Impl u v)   -> Left  [Swff True u,  Swff False v]

--suppose you already know the shape of some reduced
--swff s, via 'reduce s :: Box Swff', and you want to
--do the appropriate thing to the tree you're working on
step ::  Box q -> Tree [q] -> Tree [q]
step b n@(Node rl sf) =
 case (b, sf) of
  (Left ws, _)            -> stick_leaves ws n 
  --debug this case.
  (Right (v, w), _)       --I can't think of a neater solution
   | null sf         ->  n{subForest = sf ++ [Node [v] []] ++ [Node [w] []]}
   | otherwise       ->  n{subForest = concat [map (stick_node [v]) sf,
                                               map (stick_node [w]) sf ] }

-- model one step of the evolution process
-- for an analytic tableau
next_level :: Tree [SignedWff t] -> Tree [SignedWff t]
next_level n@(Node rl sf) = 
 foldr step n (map reduce rl)

--doesn't work for t1, t2
t1 :: Tree [SignedWff Char]
t1 = 
 Node [Swff True p] []

t2 =
 Node [Swff True (Conj p q)] []

--does work for t3
t3 =
 Node [Swff True (Conj p q)]
  [Node [Swff True  r] 
        [],
   Node [Swff False r] 
        []
        ] 
    
--compare / contrast behavior in these cases
t4 =
 Node [Swff True (Conj p q),
       Swff True r]       
   [] 

t4' =
 Node [Swff True r,
       Swff True (Conj p q)]       
   [] 

t5 = 
 Node [Swff True (Disj p q)] []
--should get: length $ subForest $ next_level t5 = 2

t6 =
 Node [Swff True (Disj p q), Swff True r, Swff False q] []

t7 =
 Node [Swff True  (Disj p q),
       Swff False (Conj p (Neg r)),
       Swff True (Impl r q)]
      []

--                  isTauto                    
--the main process can now be described as follows:
--starting with some          f :: SignedWff t,
--build the tree              t = Node [f] []
--and determine whether, at root-label level,
--[f] consists of nothing but symbols :: Sym t
--if so, search it for contradictions
--otherwise,
   --1. t <- next_level t, to get t'
   --2. over the resulting subforest, subForest t',
   --   determine whether all root-labels are just
   --   list of symbols;
   --3a. if True, isTauto f = True iff 
   --             no root-label contains a contradiction;
   --                       = False otherwise
   --3b. if False, continue applying next_level 
   --but do it over the entire sub-forest this time

isSignSymList :: [SignedWff t] -> Bool
isSignSymList []                 = True
isSignSymList (z@(Swff b w):zs)  =
 case w of
  Sym _      -> isSignSymList zs
  _          -> False


--STOP FOR NOW (8/4/2019)
-- --need to check if a list of signed sym's 
-- --contains any contradictions
-- hasContra :: [SignedWff t] -> Bool
-- hasContra []      = False
-- hasContra (w:ws)  =




--completely expand a signed wff into its tree
--with leaf-level nodes (i.e. having null sub-forest)
--all satisfying 
--   isSignSymList $ rootLabel n == True

--when calling this function it's assumed that sf is null
expand :: Tree [SignedWff t] -> Tree [SignedWff t]
expand t@(Node rl sf)
 | isSignSymList rl          = t
 | otherwise                 =
     let t' = next_level t
     in
      t'{subForest = map expand (subForest t') }

--auxiliary function for use with drawTree
--need to turn every node into a 'String' ?

--'drawTree' has type :: Tree String -> String,
--so first, need to be able to turn Tree [SignedWff t]
--into Tree String  
mapTree :: (a -> b) -> Tree a -> Tree b 
mapTree f (Node rl sf) = Node (f rl) (map (mapTree f) sf)

stringifyTree :: (Show t) => Tree t -> Tree String
stringifyTree = mapTree show

--start from a SignedWff t; show its resulting,
--fully expanded tree via tableau method, using
--'drawTree'
--It does but I wanted, but the tree printing
--doesn't work well with record syntax ...
drawTableau :: (Show t) => SignedWff t -> String
drawTableau swff = 
  let 
    t_0 = Node [swff] []
  in
    drawTree $ stringifyTree $ expand t_0
