module Data
(dat)
where

import           Data.List.Split

dat :: IO ([[Double]],[[Double]])
dat =  do
           file <- readFile "nyc-east-river-bicycle-counts.csv"
           let ls = tail $ lines file
           let tokens = map (splitOn ",") ls
           let num_str = map(\x -> snd (splitAt 3 x)) tokens
           let pairs = map (splitAt 7) $ map (map(\a -> read a :: Double)) num_str
           let normalized = map( \(a,b) -> ( norm_inp a, map(1.0/10000*) b)) pairs
           return $ (\x -> (map fst x, map snd x)) normalized

norm_inp :: [Double] -> [ Double]
norm_inp inp = zipWith (\f a -> f a) fs  inp
                                              where
                                                fs = [(1.0/100*),(1.0/100*), (1.0*)] ++ repeat (1.0/10000*)
