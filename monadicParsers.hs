import Data.Tree

type Parser a = String -> [(a, String)]

--primitive parsers
result :: a -> Parser a --i.e. a -> String -> [(a, String)]
result v = \inp -> [(v, inp)]
--a trivial parser which always succeeds on its input string;
--takes any input string & returns this paired with v

--a trivial parser which always fails; there is instead
--a 'zero result', where there is nothing in the returned
--list regardless of the input string
zero :: Parser a
zero = \inp -> []

--the final primitive is item, which successfully consumes
--the first character if the input string is non-empty, and
--fails otherwise:
item :: Parser Char
item = \inp -> 
            case inp of
             []     -> []        --equiv., zero inp
             (x:xs) -> [(x, xs)] --equiv., result x xs

--parser combinators
--in earlier (non-monadic) accounts of combinator parsing,
--sequencing of parsers was usually captured by a combinator
sq     :: Parser a -> Parser b -> Parser (a,b)
--i.e.:
--       :: (String -> [(a, String)]) 
--            -> (String -> [(b, String)])
--              -> (String -> [((a,b), String)])
p `sq` q =
 \inp -> [ ((v,w) , inp'') 
           |  (v, inp')  <- p inp    --n.b.: order matters!
            , (w, inp'') <- q inp']  
--this applies one parser, p, followed by the other,
--q, with the results from the two parsers being combined
--as pairs. (that is - the two parser-consummands of type
--a, b are paired together & shown alongside each other,
--while inp -> inp' -> inp'' gives back only the final
--form of the string after we've applied both parsers.)

--the problem of nested tuples can be avoided by adopting
--a *monadic* sequencing combinator (commonlyh known as bind)
--which integrates the sequencing of parsers with the 
--processing of their result values:
bind     :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = 
 \inp -> concat [f v inp' | (v, inp') <- p inp]

--you need 'concat' in the body of the function. 
--why?
--p inp :: [(a, String)]
--map (\r -> f (fst r) (snd r)) (p inp) :: [[(b, String)]]

--w/o 'concat', you get an error:
--couldn't match expected type  '(b, String)'
--with actual type              '[(b, String)]'
--because                  'f v inp'' :: [(b, String)],
--but is enclosed in [...], so we expected (b, String)
--to match with output type [(b, String)] of Parser b
--and not                   [[(b, String)]] 
