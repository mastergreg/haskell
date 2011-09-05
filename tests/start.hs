-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
--
-- File Name : start.hs
--
-- Purpose :
--
-- Creation Date : 05-09-2011
--
-- Last Modified : Mon 05 Sep 2011 01:59:27 PM EEST
--
-- Created By : Greg Liras <gregliras@gmail.com>
--
--_._._._._._._._._._._._._._._._._._._._._.

getInt :: IO Int
getInt = readLn
main = 
  do
    cases <- getInt
    putStrLn (show cases)
