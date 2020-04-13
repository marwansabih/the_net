module Graphprinter
  (
      output_graph
  )
where

import           Graphics.Image as I
import           Line.Draw
import           Network
import           Types

circle :: Double -> (Int,Int) -> (Double,Double,Double)
circle radius (x,y) = if x' ** 2 + y' ** 2 <= 0.75*radius ** 2
                                    then (0,1,0)
                                    else  (0,0,0)
                              where
                                x' = fromIntegral x - radius
                                y' = fromIntegral y - radius

circle_image :: Int ->  Image VU RGB Double
circle_image radius = makeImageR VU (2*radius, 2*radius) (\(i, j) -> let (r,g,b) = circle (fromIntegral radius) (i,j) in PixelRGB r g b)

incBy ::  Int -> (Int,Int) -> (Int,Int)
incBy radius (fm, fn)  = (radius*2 * fm, radius* 2 * fn)

net_image :: Int ->  (Int,Int) -> Image VU RGB Double
net_image radius (width, depth) = canvasSize Wrap (incBy radius (width, depth)) (circle_image radius)


create_line :: Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
create_line radius from to = bresenham n_from n_to
                                  where
                                    new_pos (x,y) = (radius + 2*radius*y,radius+2*radius*x)
                                    n_from = new_pos from
                                    n_to = new_pos to


neighbour :: (Int,Int) -> [(Int,Int)] -> Bool
neighbour (x,y) xs = Prelude.or $ Prelude.map (`elem` xs) [(a,b) | a <-[x-1..x+1], b <-[y-1..y+1]]

add_lines :: [(Int,Int)] -> Image VU RGB Double -> Image VU RGB Double
add_lines line image = imap (\x p -> if x `neighbour` line then PixelRGB 1 0 0 else p) image

net_to_lines :: Int -> Net -> [(Int,Int)]
net_to_lines radius net = concatMap lines_node design
                        where
                          design = net_to_design net
                          lines_node ((x,y),ts) = concatMap (\(_,(a,b)) -> create_line radius (x,y) (a+x+1,b)) ts


output_graph :: Int -> String -> Net -> IO()
output_graph node_radius filename net = do
                           let pixels = net_to_lines node_radius net
                           let d = length $ app net
                           let w = ( length $ head $ app net)
                           writeImage filename $ add_lines pixels $ net_image node_radius (w,d)

test :: IO()
test = do
       --generate_random_net width depth nr_neuron nr_con
       net <- generate_random_net 4 5 3 3
       output_graph 10 "test_net.png" net
