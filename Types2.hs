module Types2 where
import qualified Data.Vector as V

data Node = Node (Double,Double) (V.Vector (Double, (Int,Int))) deriving Show
data Net  = Net (V.Vector (V.Vector Node))

data Node2 a = Node2 a (V.Vector (Double, (Int,Int)))
data Net2 a = Net2 (V.Vector ( V.Vector (Node2 a)))

type Design =  V.Vector ((Int,Int), (V.Vector (Double,(Int,Int))))

newtype DT a = D (Design -> (a, Design))

sequ :: [IO a] -> IO [a]
sequ [] = return []
sequ (x:xs) = do
                     n_x <-x
                     n_xs <- sequ xs
                     return (n_x:n_xs)

app :: Net -> V.Vector (V.Vector Node)
app (Net values) = values
