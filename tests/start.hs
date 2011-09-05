-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
--
-- File Name : start.hs
--
-- Purpose :
--
-- Creation Date : 05-09-2011
--
-- Last Modified : Mon 05 Sep 2011 10:35:58 PM EEST
--
-- Created By : Greg Liras <gregliras@gmail.com>
--
--_._._._._._._._._._._._._._._._._._._._._.

getInt :: IO Int
getInt = 
  do
    c <- getLine
    return (read c)


getRoboCode :: IO [String] --(Int,[(Char,Int)])
getRoboCode =
  do
    inp <- getLine
    return (split inp ' ')
 

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
         where
                rest = split cs delim

parseRoboCode                       :: [String] -> [String] -> [Integer] -> [Integer] -> ([String],[Integer],[Integer])
parseRoboCode []          accP accO accB = (reverse accP,reverse accO,reverse accB)
parseRoboCode ("B":y:xys) accP accO accB = parseRoboCode xys ("B":accP) accO (((read y)::Integer):accB)
parseRoboCode ("O":y:xys) accP accO accB = parseRoboCode xys ("O":accP) (((read y)::Integer):accO) accB

--countSteps (cur,rent) (ne,xt) counter []        = counter
--countSteps (cur,rent) (ne,xt) counter (p:l:st)  = countSteps (ne,xt) counter st
tics              :: Integer -> [Integer] -> [Integer] -> [Integer]
tics prev []   acc = reverse acc 
tics prev (l:ist) acc = tics l ist ((abs (l - prev)):acc)



runBot                                                    ::  [String]->[Integer]->[Integer]->Integer->Integer
runBot []             _             _             counter = counter   
runBot ("O":turnList) (o:oTickList) (b:bTickList) counter = runBot turnList oTickList ((max 0 (b-o-1)):bTickList) (o+1+counter) 
runBot ("O":turnList) (o:oTickList) []            counter = runBot turnList oTickList []                        (o+1+counter) 
runBot ("B":turnList) (o:oTickList) (b:bTickList) counter = runBot turnList ((max 0 (o-b-1)):oTickList) bTickList (b+1+counter) 
runBot ("B":turnList) []            (b:bTickList) counter = runBot turnList []                        bTickList (b+1+counter) 

myShow :: ([String],[Integer],[Integer]) -> String
myShow (s,o,b) = 
  do
      x <- (show s)++"("++ (show o) ++","++(show b)++")"
      return x

myMap f x = 
  do
    myMapH f x []
    where myMapH f [] acc = acc
          myMapH f (x:xs) acc = myMapH f xs ((f x):acc)

getAll x cases
  | x<= cases =
    do
      first <- getRoboCode
      let (s,o,b) = parseRoboCode (tail first) [] [] []
      putStrLn ("Case #"++(show x)++": "++(show (runBot s (tics 1 o []) (tics 1 b []) 0)))
      getAll (x+1) cases
  | otherwise = return ()

main = 
  do
    cases <- getInt
    --putStrLn (show cases)
    getAll 1 cases
