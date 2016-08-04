 {-# LANGUAGE GADTs #-} 
import Text.ParserCombinators.Parsec
import RegExpr.Operation
import Data.Either
import Control.Monad
import Data.List(splitAt,elemIndex)
import System.Environment (getArgs)
import Debug.Trace

echars = "\\^*+?|().[]-{}"
--achars = "wWdDsS"
w= cut ws
ws ="[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz]"
digi = cut digis
digis ="[0123456789]"
s= cut ss
ss = "[\t\n\v\f\r]"


cut:: String -> String
cut str = slice 1 (length str -1) str

slice :: Int -> Int -> [a] -> [a]
slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

            
rng2set:: Char -> Char -> String -> String
rng2set a b str = do
    let si =   fromJust $ elemIndex a str 
    let ei =   fromJust $ elemIndex b str
    slice si (ei+1) str  
    
    
rparse:: String -> RE
rparse  s  = do
 case parse re "" s of
      Left e -> trace ("error" ++ show e ) Phi
      Right r -> r

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x 

repeatC :: RE -> Int -> RE
repeatC r 0 = Empty
repeatC r 1 = r
repeatC r n = Choice (repeatC r $ n-1 ) (repeatN r n)


repeatN :: RE -> Int -> RE
repeatN r 0 = Empty
repeatN r 1 = r
repeatN r n = Seq (repeatN r $ n-1 ) r

    
re:: GenParser Char st RE
re =  try union <|> sre

union::GenParser Char st RE
union =  do
    a <- sre
    char '|'
    b <- re
    pure (Choice a b)

sre:: GenParser Char st RE
sre =  try concatenation <|> bre

concatenation:: GenParser Char st RE
concatenation = liftM2 Seq (bre) (sre)

bre:: GenParser Char st RE
bre =  try estar <|> try eplus <|>try eqm<|>try erange <|> try erange' <|> ere

ere:: GenParser Char st RE
ere = group <|> anyC <|> literal <|>try set <|> try escape  <|> alias

group:: GenParser Char st RE
group = between (char '(') (char ')') re

set:: GenParser Char st RE
set = try nset <|> pset

anyC :: GenParser Char st RE
anyC = char '.' >> pure (Any)


literal :: GenParser Char st RE
literal = liftM L (noneOf echars)

escape :: GenParser Char st RE
escape = char '\\' >> liftM L (oneOf echars)

ec :: GenParser Char st Char
ec = char '\\' >> oneOf echars     

alias :: GenParser Char st RE
alias = try dgt <|> try ndgt <|> try word <|> try nword <|> try sps <|> nsps
    
    
dgt:: GenParser Char st RE 
dgt = char '\\' >>  char 'd' >> pure(rparse digis) 
ndgt:: GenParser Char st RE 
ndgt = char '\\' >>  char 'D' >> pure(Not digi)

word:: GenParser Char st RE 
word = char '\\' >>  char 'w' >> pure(rparse ws) 
nword:: GenParser Char st RE 
nword = char '\\' >>  char 'W' >> pure(Not w)
sps:: GenParser Char st RE 
sps = char '\\' >>  char 's' >> pure(rparse ss) 
nsps:: GenParser Char st RE 
nsps = char '\\' >>  char 'S' >> pure(Not s)

alias' :: GenParser Char st [Char]
alias' = try dgt' <|>  try word' <|>  try sps'

dgt':: GenParser Char st [Char] 
dgt' = char '\\' >>  char 'd' >>  pure digi 

word':: GenParser Char st [Char]  
word' = char '\\' >>  char 'w' >>pure w 

sps':: GenParser Char st [Char]  
sps' = char '\\' >>  char 's' >>pure s

pset :: GenParser Char st RE
pset = between (char '[') (char ']') setItems

nset :: GenParser Char st RE
nset = do
   char '['
   char '^'
   s <- notItems
   char ']'
   pure (Not $ flatten s)
   
notItems :: GenParser Char st [[Char]]  
notItems = many1 $ try nrngItems  <|> try alias' <|> notItems' 

notItems' :: GenParser Char st [Char]
notItems'  = many1 $try neitem

rng:: GenParser Char st RE
rng = try rangew <|>try ranges <|> ranged

setItems :: GenParser Char st RE --refactoring to use optionmaybe and manyuntil
setItems = liftM2 Choice (try rng<|>literal<|>try escape <|> try alias) (remainingItems)

remainingItems :: GenParser Char st RE
remainingItems = setItems <|> (return Phi)   

neitem::GenParser Char st Char   
neitem = do
    c <- noneOf echars <|> ec
    notFollowedBy $ char '-'
    pure c 

nrngItems = try nrangew <|> try nranged <|> nranges 


nrangew:: GenParser Char st [Char]
nrangew = do
    s <- oneOf w
    char '-'
    e <- oneOf w
    pure(rng2set s e w)

nranged:: GenParser Char st [Char]
nranged = do
    s <- oneOf digi
    char '-'
    e <- oneOf digi
    pure(rng2set s e digi)


nranges:: GenParser Char st [Char]
nranges = do
    st <- oneOf s
    char '-'
    e <- oneOf s
    pure(rng2set st e s)


rangew:: GenParser Char st RE
rangew = do
    s <- oneOf w
    char '-'
    e <- oneOf w
    let r = "[" ++ (rng2set s e w)++ "]"
    pure (rparse r)

ranged:: GenParser Char st RE
ranged = do
    s <- oneOf digi
    char '-'
    e <- oneOf digi
    let r = "[" ++ (rng2set s e digi)++ "]"
    pure (rparse r) 

ranges:: GenParser Char st RE
ranges = do
    st <- oneOf s
    char '-'
    e <- oneOf s
    let r = "[" ++ (rng2set st e s)++ "]"
    pure (rparse r)             
    
estar  :: GenParser Char st RE
estar =
    do e <- ere
       char '*'
       pure (Star e)

eplus  :: GenParser Char st RE
eplus =
    do e <- ere
       char '+'
       pure (Seq e (Star e))

eqm  :: GenParser Char st RE
eqm =
    do e <- ere
       char '?'
       pure (Choice e Empty)
       
erange::GenParser Char st RE
erange = do
     e <- ere
     char '{'
     base <- fmap read $ many1 digit
     char ','
     high <- fmap read $ many1 digit
     char '}'
     pure (Seq (repeatN e base) $ Choice Empty (repeatC e (high-base)))

erange'::GenParser Char st RE
erange' = do
     e <- ere
     char '{'
     base <- fmap read $ many1 digit
     string ",}"
     pure (Seq (repeatN e base) (Star e))
 
main = do
    args <- getArgs
    case args of
               ["-c",r1,r2] -> do
                   myPrint $  r2 ++ " contains " ++ r1
                  -- myPrint $ show  (rparse   r1) ++ " contains " ++ show (rparse   r2)
                   myPrint $ contains (rparse   r1)  (rparse   r2)
               
               ["-e",r1,r2] -> do
                   myPrint $  r2 ++ " equals to " ++ r1
                  -- myPrint $ show  (rparse   r1) ++ " equals to " ++ show (rparse   r2)
                   myPrint $ equality (rparse  r1)  (rparse   r2)
               
               ["-i",r1,r2] -> do
                    myPrint $  r2 ++ " intersects with " ++ r1
            --        myPrint $ show  (rparse   r1) ++ " intersects with " ++ show (rparse   r2)
                    myPrint $ intersect (rparse   r1)  (rparse   r2)   
               _ -> putStrLn "Contains : -c re1 re2 \nEquality: -e re1 re2\nIntersection: -i re1 re2 "
