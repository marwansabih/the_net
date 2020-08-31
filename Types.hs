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

toFastNet :: Net -> [[(Double,Double,Double,[(Double,Double,Int,Int)])]]
toFastNet net = map ( map (\(Node (_,_) ts) -> (0.0,0.0,0.0, mapTs ts) ))  $ app net
  where
    mapTs xs = map (\(w,(y,x)) -> (w,0.0,y+1,x)) xs

fromFastNet :: [[(Double,Double,Double,[(Double,Double,Int,Int)])]] -> Net
fromFastNet net = Net $ map (map (\(_,_,_,ts) -> Node (0,0) (mapTs ts))) $ net
  where
    mapTs xs = map (\(w,_,y,x) -> (w,(y-1,x))) xs
