module Types where

data Node = Node (Double,Double) [(Double, (Int,Int))] deriving Show
data Net  = Net [[Node]]

data Node2 a = Node2 a [(Double, (Int,Int))]
data Net2 a = Net2 [[Node2 a]]

type Design = [((Int,Int),[(Double,(Int,Int))])]

newtype DT a = D (Design -> (a, Design))

sequ :: [IO a] -> IO [a]
sequ [] = return []
sequ (x:xs) = do
                     n_x <-x
                     n_xs <- sequ xs
                     return (n_x:n_xs)

app :: Net -> [[Node]]
app (Net values) = values
