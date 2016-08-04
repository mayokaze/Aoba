 {-# LANGUAGE GADTs #-}

 module RegExpr.Operation where

 import Data.List hiding (intersect)
 import Debug.Trace
 import Data.Char (chr)



 data RE = Phi
  | Empty
  | L Char
  | Choice RE RE
  | Seq RE RE
  | Star RE
  | Any
  | Not [Char]
  | Var  Int
  deriving (Eq)

 type Word a = [a]


 instance Show (RE) where
     show Phi = "{}"
     show Empty = "<>"
     show (L c) = show c
     show (Choice r1 r2) = ("(" ++ show r1 ++ "|" ++ show r2 ++ ")")
     show (Seq r1 r2) = ("<" ++ show r1 ++ "," ++ show r2 ++ ">")
     show (Star r) = (show r ++ "*")
     show Any  = "."
     show (Not ls) = "[^" ++ show ls ++ "]"
 resToRE :: [RE] -> RE
 resToRE (r:res) = foldl Choice r res
 resToRE [] = Phi


 myPrint :: Show a => a -> IO ()
 myPrint = putStrLn . myShow

 myShow :: Show a => a -> String
 myShow x = con (show x) where
  con :: String -> String
  con [] = []
  con li@(x:xs) | x == '\"' = '\"':str++"\""++(con rest)
                | x == '\'' = '\'':char:'\'':(con rest')
                | otherwise = x:con xs where
                  (str,rest):_ = reads li
                  (char,rest'):_ = reads li




 sigmaRE :: RE -> [Char]
 sigmaRE r = let s = (sigmaREsub r)
             in s `seq` nub s

 sigmaREsub (L l) = [l]
 sigmaREsub Any = [chr 66666] --map chr [0 .. 255]
 sigmaREsub (Not cs) =  [chr 66667]++cs --filter (\c -> not (c `elem` cs)) (map chr [0 .. 255])
 sigmaREsub (Seq r1 r2) = (sigmaREsub r1) ++ (sigmaREsub r2)
 sigmaREsub (Choice r1 r2) = (sigmaREsub r1) ++ (sigmaREsub r2)
 sigmaREsub (Star r) = sigmaREsub r
 sigmaREsub Phi = []
 sigmaREsub Empty = []


-- Testing if a regular expression is empty (empty word)

 isEmpty :: RE  -> Bool
 isEmpty Phi = False
 isEmpty Empty = True
 isEmpty (Choice r1 r2) = (isEmpty r1) || (isEmpty r2)
 isEmpty (Seq r1 r2) = (isEmpty r1) && (isEmpty r2)
 isEmpty (Star r) = True
 isEmpty (L _) = False
 isEmpty Any = False
 isEmpty (Not _) = False



 isPhi :: RE -> Bool
 isPhi Phi = True
 isPhi Empty = False
 isPhi (Choice r1 r2) = (isPhi r1) && (isPhi r2)
 isPhi (Seq r1 r2) = (isPhi r1) || (isPhi r2)
 isPhi (Star r) = False
 isPhi (L _) = False
 isPhi Any = False
 isPhi (Not _) = False


 deriv :: RE -> Char -> RE
 deriv Phi _ = Phi
 deriv Empty _ = Phi
 deriv Any _ = Empty
 deriv (Not ls) l
     | l `elem` ls = Phi
     | otherwise = Empty
 deriv (L l1) l2
     | l1 == l2  = Empty
     | otherwise = Phi
 deriv (Choice r1 r2) l =
   Choice (deriv r1 l) (deriv r2 l)
 deriv (Seq r1 r2) l =
   if isEmpty r1 -- if r1 is an empty word
   then Choice (Seq (deriv r1 l) r2) (deriv r2 l)
   else Seq (deriv r1 l) r2
 deriv (this@(Star r)) l =
   Seq (deriv r l) this


 partDeriv :: RE -> Char -> [RE]
 partDeriv Phi l = []
 partDeriv Empty l = []
 partDeriv Any l = [Empty]
 partDeriv (Not ls) l
     | l `elem` ls = []
     | otherwise = [Empty]
 partDeriv (L l') l
     | l == l'   = [Empty]
     | otherwise = []
 partDeriv (Choice r1 r2) l = nub ((partDeriv r1 l) ++ (partDeriv r2 l))
 partDeriv (Seq r1 r2) l
     | isEmpty r1 =
           let s1 = [ (Seq r1' r2) | r1' <- partDeriv r1 l ]
               s2 = partDeriv r2 l
           in nub (s1 ++ s2)
     | otherwise = [ (Seq r1' r2) | r1' <- partDeriv r1 l ]
 partDeriv (Star r) l = [ (Seq r' (Star r)) | r' <- partDeriv r l ]



 type Env = [((RE, RE), RE)]


 convert :: Int -> RE -> RE
 convert x r = let (r1,r2) = convert2 x r
               in Seq (Star r1) r2

 convert2 :: Int -> RE -> (RE , RE)
 convert2 x Empty = (Empty, Empty)
 convert2 x (Var y)
        | x == y    = (Empty,Phi)
        | otherwise = (Empty, Var y) 
 convert2 x (r@(Seq l r1))
        | mentions x r1 = let (r2,r3) = convert2 x r1
                          in (Seq l r2, r3)
        | otherwise = (Empty, r)
 convert2 x (r@(L _)) = (Empty, r)
 convert2 x (Choice r1 r2) = let (r1', r1'') = convert2 x r1
                                 (r2', r2'') = convert2 x r2
                             in (Choice r1' r2', Choice r1'' r2'')
 convert2 x Phi = trace ("convert2: " ++ show x ) (Phi,Phi)

 mentions :: Int -> RE -> Bool
 mentions x (Var y) = x == y
 mentions x (Seq r1 r2) = mentions x r1 || mentions x r2
 mentions x (Star r) = mentions x r
 mentions x (Choice r1 r2) = mentions x r1 || mentions x r2
 mentions x _ = False

 intersect :: RE -> RE  -> RE
 intersect r1 r2 = intersectC 1 [] r1 r2

 intersectC :: Int -> Env -> RE -> RE -> RE
 intersectC cnt env r1 r2
  | r1 == Phi || r2 == Phi = Phi

  | r1 == Empty = if isEmpty r2
                  then Empty
                  else Phi
  | r2 == Empty = if isEmpty r1
                  then Empty
                  else Phi

  | otherwise =
      case lookup (r1,r2) env of
       Just r -> r
       Nothing ->
         let letters = sigmaRE (r1 `Choice` r2)
             env' = ((r1,r2),Var cnt):env
             r1l l = resToRE $ partDeriv r1 l
             r2l l = resToRE $ partDeriv r2 l
             r' = resToRE $ map (\l -> Seq (L l) (intersectC (cnt+1) env' (r1l l) (r2l l))) letters
             r =  if (isEmpty r1) && (isEmpty r2)
                  then Choice r' Empty
                  else r'
         in convert cnt r

 type EnvEq = [(RE, RE)]

 equality ::RE  -> RE -> Bool
 equality r1 r2 = eqREC [] r1 r2

 eqREC :: EnvEq  -> RE  -> RE  -> Bool
 eqREC env r1 r2
   | isEmpty r1 && (not (isEmpty r2)) = False
   | isPhi r1 && (not (isPhi r2)) = False
   | isEmpty r2 && (not (isEmpty r1)) = False
   | isPhi r2 && (not (isPhi r1)) = False
   | otherwise =
      if elem (r1,r2) env
      then True
      else let letters = sigmaRE (r1 `Choice` r2)
               env' = (r1,r2):env
               r1l l = resToRE $ partDeriv r1 l
               r2l l = resToRE $ partDeriv r2 l
           in and $ map (\l -> eqREC env' (r1l l) (r2l l)) letters -- all true / added parent r1 r2 to env but not pd of r1l and r2l(check if r1l and r2l already in env)


 containsRECheap ::  RE  -> RE -> Bool
 containsRECheap r1 r2 = equality r1 (intersect r1 r2)


 contains ::  RE -> RE  -> Bool
 contains r1 r2 = containsC [] r1 r2

 containsC :: EnvEq  -> RE  -> RE  -> Bool
 containsC env r1 r2
   | r1 == Empty = isEmpty r2
   | r1 == Phi = True
   | r2 == Phi = equality r1 Phi
   | r2 == Empty = equality r1 Empty
   | elem (r1,r2) env = True
   | otherwise =
         let letters = sigmaRE (r1 `Choice` r2)
             env' = (r1,r2) :env
             r1l l = resToRE $ partDeriv r1 l
             r2l l = resToRE $ partDeriv r2 l
             b = and $ map (\l -> containsC env' (r1l l) (r2l l)) letters
         in b && ((isEmpty r1) `implies` (isEmpty r2)) --check if r1 contains emptyword


 implies :: Bool -> Bool -> Bool
 implies a b = (not a) || b -- r1 is not emptyword or r2 is empty




