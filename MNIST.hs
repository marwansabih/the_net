module MNIST
(
    render_mnist,
    draw_mnist_training_batch,
    draw_mnist_test,
    mnist_set,
    draw_mnist_training_batch_by_ids
)
where

import           Codec.Compression.GZip      (decompress)
import           Control.Monad
import           Control.Parallel            (par, pseq)
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy        as BS
import           Data.Functor
import           System.Random

import           Data.List                   (maximumBy)
import           Data.Ord                    (comparing)

import           IMGNetwork
import           Network
--import           RunnerClassic
import           System.IO
import           Types
new_map = parMap rpar

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

render n = let s = " .:oO@" in s !! (floor  n * length s `div` 256)

softmax :: [Double] -> [Double]
softmax xs = map (1/norm*) e_xs
                where
                   x_max = maximum xs
                   e_xs = new_map (\x -> exp (x-x_max)) xs
                   norm = sum e_xs

render_mnist :: ([[Double]] , [Double]) -> IO()
render_mnist (img, nr) = do
                          putStr $ unlines $ map ( map render)  img
                          print $ maxIndex nr




s' = decompress <$> BS.readFile "mnist/train-images-idx3-ubyte.gz"
l' = decompress <$> BS.readFile "mnist/train-labels-idx1-ubyte.gz"

mnist_set :: IO [([[Double]], [Double])]
mnist_set =  sequence $  map (draw_mnist_training_by_nr s' l' ) [0..10]

draw_mnist_training_batch_by_ids :: [Int] -> IO [([[Double]], [Double])]
draw_mnist_training_batch_by_ids ids = sequence $ map (draw_mnist_training_by_nr s' l' ) ids



draw_mnist_training_by_nr :: IO BS.ByteString -> IO BS.ByteString -> Int -> IO ([[Double]] , [Double])
draw_mnist_training_by_nr ss' ll' n = do
  s <- ss'
  l <- ll'
  let img =  [(fromIntegral . BS.index s . (fromIntegral n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let one_hot = to_one_hot $ fromIntegral (  BS.index l (fromIntegral n + 8))
  return (img, one_hot)

draw_mnist_training_batch :: Int -> IO [([[Double]], [Double])]
draw_mnist_training_batch n = sequence $ replicate n draw_mnist_training


draw_mnist_training :: IO ([[Double]] , [Double])
draw_mnist_training = do
  s <- s'
  l <- l'
  n <- (`mod` 60000) <$> randomIO
  let img =  [(fromIntegral . BS.index s . (n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let one_hot = to_one_hot $ fromIntegral (  BS.index l (n + 8))
  return (img, one_hot)






draw_mnist_test :: Int -> IO ([[Double]] , [Double])
draw_mnist_test n = do
  s <- decompress <$> BS.readFile "mnist/t10k-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "mnist/t10k-labels-idx1-ubyte.gz"
  let img =  [(fromIntegral . BS.index s . (fromIntegral n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let one_hot = to_one_hot $ fromIntegral (   BS.index l (fromIntegral n + 8))
  return (img, one_hot)

to_one_hot :: Double -> [Double]
to_one_hot nr = xs ++ [1] ++ ys
                where
                  (xs, y:ys) = splitAt (round  nr) $ replicate 10 0



test :: IO()
test = do
  s <- decompress <$> BS.readFile "mnist/train-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "mnist/train-labels-idx1-ubyte.gz"
  n <- (`mod` 60000) <$> randomIO
  --putStr . unlines $
  --  [(render . BS.index s . (n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  print $ BS.index l (n + 8)
  (img, nr) <- draw_mnist_training
  render_mnist (img, nr)
  (draw_mnist_test 9999) >>= render_mnist
  draw_mnist_training_batch 10 >>= ( sequence_ . ( map render_mnist ))
