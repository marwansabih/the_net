module RunnerClassic
(
        training_batches_classic,
        training_normed_batches_classic,
        find_best_random_net_classic,
        update_random_net_classic,
        update_random_net_con_classic,
        update_add_del_net_con_classic,
        update_add_net_con_classic,
        update_del_net_con_classic,
        output_classic,
        wrong_test_predictions,
        wrong_test_predictionss,
        correct_train_prediction_by_nr,
        wrong_train_predictions,
        wrong_train_predictions_,
        draw_wrong_mnist_training_batch
) where

import           Backpropagation
import           Codec.Compression.GZip (decompress)
import           Control.Monad
import qualified Data.ByteString.Lazy   as BS
import           Data.List              (maximumBy)
import           Data.Ord               (comparing)
import           Data.Time
import           IMGNetwork
import           MNIST
import           Network
import           System.IO
import           TestVectors
import           Types

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

to_one_hot :: Double -> [Double]
to_one_hot nr = xs ++ [1] ++ ys
                where
                  (xs, y:ys) = splitAt (round  nr) $ replicate 10 0

s' = decompress <$> BS.readFile "mnist/train-images-idx3-ubyte.gz"
l' = decompress <$> BS.readFile "mnist/train-labels-idx1-ubyte.gz"

draw_wrong_mnist_training_batch :: Int -> Int -> IO [([[Double]], [Double])]
draw_wrong_mnist_training_batch start nr = sequence $ map draw_wrong_mnist_training  [start..(start+nr-1)]

draw_wrong_mnist_training :: Int -> IO ([[Double]] , [Double])
draw_wrong_mnist_training n' = do
  s <- s'
  l <- l'
  --n' <- (`mod` (length not_learned)) <$> randomIO
  let n = ([1..100] !! n')
  let img =  [(fromIntegral . BS.index s . (n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let one_hot = to_one_hot $ fromIntegral (  BS.index l (n + 8))
  return (img, one_hot)

wrong_test_predictions :: Net -> IO [Int]
wrong_test_predictions net = undefined
{--
  do
                                            let xs = [0..9999]
                                            predis <- sequence $ new_map (  correct_test_prediction_by_nr net) xs
                                            return $ filter(\x -> not ( predis !! x )) xs
--}

wrong_test_predictionss :: Net -> IO()
wrong_test_predictionss net = do
  let (short_design, simple_net) = net_to_simple_net net
  net <- netFromList $ toFastNet simple_net
  wrong_test_predictions'' [0..9999] 0 0 net

wrong_test_predictions'' :: [Int] -> Int  -> Int-> FNet -> IO()
wrong_test_predictions'' [] times correct net = print "finished"
wrong_test_predictions'' (x:xs) times correct net = do
  right <- correct_test_prediction_by_nr net x
  let n_correct = if right then correct +1 else correct
  let n_times = times + 1
  putStrLn $ show (100 - (fromIntegral n_correct /( fromIntegral n_times)) *100) ++ " %"
  wrong_test_predictions'' xs n_times n_correct net

correct_test_prediction_by_nr :: FNet -> Int -> IO Bool
correct_test_prediction_by_nr net n = do
  s <- decompress <$> BS.readFile "mnist/t10k-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "mnist/t10k-labels-idx1-ubyte.gz"
  let img =  [(fromIntegral . BS.index s . (fromIntegral n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let target = BS.index l (fromIntegral n + 8)
  let one_hot = to_one_hot $ fromIntegral (   target )
  result <- output (concat img) net -- output_classic net (concat img)
  --putStrLn "Result: "
  --print (maxIndex result)
  --putStrLn "Target: "
  --print target
  return  $ (maxIndex result) ==  fromIntegral target


wrong_train_predictions_ :: [Int] -> Net -> IO [Int]
wrong_train_predictions_ xs net = do
                                            predis <- sequence $ new_map (  correct_train_prediction_by_nr_ net) xs
                                            return $ concat $ zipWith(\p x -> if not p then [x] else []) predis xs


correct_train_prediction_by_nr_ :: Net -> Int -> IO Bool
correct_train_prediction_by_nr_ net n = do
  s <- decompress <$> BS.readFile "mnist/train-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "mnist/train-labels-idx1-ubyte.gz"
  let img =  [(fromIntegral . BS.index s . (fromIntegral n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let target = BS.index l (fromIntegral n + 8)
  let one_hot = to_one_hot $ fromIntegral (   target )
  let result = output_classic net (concat img)
  let correct = (maxIndex result) ==  fromIntegral target
  if correct then putStr "T, " else  putStr  "F, "
  let prob_correct = Backpropagation.softmax result !! fromIntegral target
  putStr (show prob_correct ++ " ")
  hFlush stdout
  return   $ if  correct  && prob_correct > 0.5 then True else False

wrong_train_predictions :: Net -> IO [Int]
wrong_train_predictions net = do
                                            let xs = [0..59999]
                                            predis <- sequence $ new_map (  correct_train_prediction_by_nr net) xs
                                            return $ filter(\x -> not ( predis !! x )) xs

correct_train_prediction_by_nr :: Net -> Int -> IO Bool
correct_train_prediction_by_nr net n = do
  s <- decompress <$> BS.readFile "mnist/train-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "mnist/train-labels-idx1-ubyte.gz"
  let img =  [(fromIntegral . BS.index s . (fromIntegral n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
  let target = BS.index l (fromIntegral n + 8)
  let one_hot = to_one_hot $ fromIntegral (   target )
  let result = output_classic net (concat img)
  let correct = (maxIndex result) ==  fromIntegral target
  if correct then putStr "T, " else  putStr  "F, "
  hFlush stdout
  return  correct

format :: [([[Double]], [Double])]  -> ([[Double]], [[Double]])
format xs = format' xs ([],[])

format' :: [([[Double]], [Double])] -> ([[Double]], [[Double]]) -> ([[Double]], [[Double]])
format' [] found           = found
format' ((a,b):xs) (as,bs) =format' xs  ( ( map ((1.0/255.0)*) (concat a) ) :as, b:bs)

training_normed_batches_classic :: Int -> Int -> Net ->([[Double]], [[Double]])-> Double -> IO Net
training_normed_batches_classic nr_times bs net sample s = do
        let (short_design, simple_net) = net_to_simple_net net
        t_net <- training_normed_batches_classic' nr_times bs simple_net sample s
        return $ simple_net_to_net short_design t_net

training_normed_batches_classic' :: Int -> Int -> Net -> ([[Double]],[[Double]]) -> Double -> IO Net
training_normed_batches_classic' 0 _ net  _ _  = return net
training_normed_batches_classic' nr_times bs net sample s = do
                                                            (inp,out) <- get_random_batch bs sample
                                                            let net' = training_normed_batch_classic net sample (s / fromIntegral bs)
                                                            getCurrentTime >>= print
                                                            let (a,b,c) =  calculate_error net' $ n_from_sample 10 sample
                                                            print $ (a,b,c)
                                                            file <- openFile "error.log" AppendMode
                                                            hSetBuffering file NoBuffering
                                                            hPutStrLn file (show b ++"\n" )
                                                            print nr_times
                                                            training_normed_batches_classic' (nr_times-1) bs net' sample s


training_batches_classic ::  Net ->[[([Double],[Double])]] -> String ->Double -> IO Net
training_batches_classic net samples filepath s = do
                                                                                    print "in batch"
                                                                                    let (short_design, simple_net) = net_to_simple_net net
                                                                                    fnet <- netFromList $ toFastNet simple_net
                                                                                    training_batches_classic'' fnet samples filepath s
                                                                                    --t_net <- training_batches_classic'' net samples s
                                                                                    t_net <- fromFastNet <$> ( netToList fnet)
                                                                                    return $ simple_net_to_net short_design t_net
                                                                                    --return t_net

{--
training_batches_classic' ::  Net -> [([Double],[Double])] -> Double -> IO Net
training_batches_classic'  net  [] _  = return net
training_batches_classic' net (sample:xs) s = do
                                                            let net' = training_batch_classic net sample (s / fromIntegral (length sample))
                                                            getCurrentTime >>= print
                                                            let (a,b,c) =  calculate_error net' $ n_from_sample 10 sample
                                                            print $ (a,b,c)
                                                            file <- openFile "error.log" AppendMode
                                                            hSetBuffering file NoBuffering
                                                            hPutStrLn file (show c )
                                                            training_batches_classic' net' xs s

--}

training_batches_classic'' ::  FNet -> [[([Double],[Double])]] -> String-> Double -> IO ()
training_batches_classic''  net  [] _ _ = return ()
training_batches_classic'' fnet (sample:xs) filepath s = do
                                                            --fnet <- netFromList $ toFastNet net
                                                            netList <-netToList fnet
                                                            sample' <- format <$> draw_mnist_training_batch 10
                                                            --let xxs = Prelude.map (\(xs,_) -> xs) sample
                                                            --let yys = Prelude.map (\(_,ys) -> ys) sample
                                                            --let sample' = (xxs,yys)
                                                            (_,_,c') <-  calculate_error2 fnet sample'
                                                            --(a,b,c) <- calculate_error2 fnet sample'
                                                            trainBatch sample (s / fromIntegral (length sample)) fnet
                                                            --let net' <- training_batch_classic net sample (s / fromIntegral (length sample))
                                                            (a,b,c) <- calculate_error2 fnet sample'
                                                            print $ "Error before: " ++ show c'
                                                            print $ "Error after: " ++ show c
                                                            getCurrentTime >>= print
                                                            if True -- c' >  c
                                                                   then
                                                                     do
                                                                            print $ (a,b,c)
                                                                            file <- openFile filepath AppendMode
                                                                            hSetBuffering file NoBuffering
                                                                            hPutStrLn file (show c )
                                                                            --net' <- fromFastNet <$> ( netToList fnet)
                                                                            training_batches_classic'' fnet xs  filepath s
                                                                   else
                                                                     do
                                                                           print "unsuccesful"
                                                                           fnet <- netFromList $ netList
                                                                           training_batches_classic'' fnet xs filepath s

n_from_sample :: Int ->  ([[Double]],[[Double]]) -> ([[Double]],[[Double]])
n_from_sample n (as,bs) = (take n as, take n bs)


output_classic :: Net -> [Double] -> [Double]
output_classic net input = map(\(Node (a,_) _) -> a) layer --softmax $ map(\(Node (a,_) _) -> a) layer
                    where
                         r_net = reset net
                         f_net = f_propagate $set_input r_net input
                         layer =  last $ app f_net

max_index :: [Double] -> Int
max_index xs = max_index' xs (-infinity) 0 0
        where
                infinity = (read "Infinity") :: Double

max_index' :: [Double] -> Double -> Int -> Int -> Int
max_index' xs mVal idx  f_idx  | idx == length xs = f_idx
max_index' xs mVal idx  f_idx  =
        if  xs !! idx > mVal
                then max_index' xs ( xs !! idx) (idx+1)  idx
                else max_index' xs mVal (idx+1) f_idx
--calculate_error :: Net ->  ([[Double]],[[Double]])   ->Double

calculate_error2 net (inp,out) = do
                                            preds' <-  mapM (\x -> output x net) inp
                                            let preds = map max_index preds'
                                            let dist' =zipWith (\x y -> if x == y then 1.0::Double else 0) preds ( map max_index out)
                                            let dist = zipWith( \x y -> 1 - sum ( zipWith(\a b -> (a*b)) x y ))  preds' out
                                            return ((zip preds $ map max_index out) , (sum dist') / ( fromIntegral ( length out )), (sum dist) /( fromIntegral ( length out )))

calculate_error net (inp,out) =  ((zip preds $ map max_index out) , (sum dist') / ( fromIntegral ( length out )), (sum dist) /( fromIntegral ( length out )) )
                                        where
                                            preds' =  map (output_classic net) inp
                                            preds = map max_index preds'
                                            dist' =zipWith (\x y -> if x == y then 1.0::Double else 0) preds ( map max_index out)
                                            dist = zipWith( \x y -> 1 - sum ( zipWith(\a b -> (a*b)) x y )) (map Backpropagation.softmax preds') out

find_best_random_net_classic :: Int -> Int -> Int -> Int -> Int  -> [([[Double]],[[Double]])] -> Double -> IO Net
find_best_random_net_classic nr_nets width depth nr_neuron nr_con sample s =
     do
         x <- generate_random_net width depth nr_neuron nr_con
         xs <- sequ $replicate (nr_nets-1) $ generate_random_net width depth nr_neuron nr_con
         (err,b_net) <- foldM (\x y -> compete_batch s (snd x) y sample)  (0::Double,x) xs
         print(err)
         time <-getCurrentTime
         print time
         return b_net


{-
update_random_net_con_classic :: Int  ->[([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_random_net_con_classic nr_con sample net s = do
                                                            time <-getCurrentTime
                                                            print time
                                                            n_net <-  update_random_net_con' nr_con sample net s
                                                            net' <- update_random_net_con_classic nr_con sample n_net s
                                                            return net'
                                                            -}


update_del_net_con_classic ::  Int  -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_del_net_con_classic  _ _ net _ = return net
update_del_net_con_classic  nr_con sample net s = do
                                                            time <-getCurrentTime
                                                            print time
                                                            n_net <-  update_del_net_con'  nr_con sample net s
                                                            net' <- update_del_net_con_classic nr_con sample n_net s
                                                            return net'

update_add_net_con_classic ::  Int  -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_add_net_con_classic _ _ net _ = return net
update_add_net_con_classic nr_con sample net s = do
                                                            time <-getCurrentTime
                                                            print time
                                                            n_net <-  update_add_net_con' nr_con sample net s
                                                            net' <- update_add_net_con_classic nr_con sample n_net s
                                                            return net'


update_add_del_net_con_classic :: Int  -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_add_del_net_con_classic _ _ net _ = return net
update_add_del_net_con_classic nr_con sample net s = do
                                                            time <-getCurrentTime
                                                            print time
                                                            n_net' <-  update_del_net_con' nr_con sample net s
                                                            n_net <-  update_add_net_con' nr_con sample n_net' s
                                                            net' <- update_add_del_net_con_classic nr_con sample n_net s
                                                            return net'

update_del_net_con' :: Int -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_del_net_con' nr_con sample net s  = do
                                                              (net1,net2) <- removing_connections nr_con net
                                                              (error', net') <- compete_batch s net1 net2 sample
                                                              print error'
                                                              return net'


update_add_net_con' :: Int  -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_add_net_con' nr_con sample net s  = do
                                                              (net1,net2) <- adding_connections nr_con net
                                                              (error', net') <- compete_batch s net1 net2 sample
                                                              print error'
                                                              return net'

update_random_net_con_classic :: Int -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_random_net_con_classic nr_con sample net s  = do
                                                              let (short_design, simple_net) = net_to_simple_net net
                                                              (net1',net2') <- alter_connections nr_con simple_net
                                                              let net1 = simple_net_to_net short_design net1'
                                                              let net2 = simple_net_to_net short_design net2'
                                                              --(net1,net2) <- alter_connections nr_con net
                                                              (error', net') <- compete_batch s net1 net2 sample
                                                              print error'
                                                              return net'

update_random_net_classic ::  Int -> Int -> Int -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_random_net_classic _ _ _ [] net _ = return net
update_random_net_classic img_width nr_alt_neuron nr_con sample net s = do
                                                            time <-getCurrentTime
                                                            print time
                                                            n_net <-  update_random_net'  img_width nr_alt_neuron nr_con sample net s
                                                            --net' <- update_random_net_classic img_width nr_alt_neuron nr_con sample n_net s
                                                            return n_net

update_random_net' :: Int  -> Int -> Int -> [([[Double]],[[Double]])] -> Net -> Double ->IO Net
update_random_net' img_width nr_alt_neuron nr_con sample net s  = do
                                                              (net1,net2) <- alter_img_neurons img_width nr_alt_neuron nr_con net
                                                              (error', net') <- compete_batch s net1 net2 sample
                                                              print error'
                                                              return net'

compete_batch :: Double -> Net -> Net -> [([[Double]],[[Double]])] -> IO (Double, Net)
compete_batch s net1 net2 sample  = undefined
  {-- do
                                                        (error1, n_net1) <- train_measure_quality s net1 sample
                                                        putStrLn $ "Error original: " ++ (show error1)
                                                        (error2, n_net2) <- train_measure_quality s net2 sample
                                                        putStrLn $ "Error original: " ++ (show error2)
                                                        if (error1 < error2) then return (error1, n_net1) else return (error2, n_net2)
                                                        --}


--train_measure_quality nr_trainings bs s net sample =undefined

train_measure_quality :: Double -> Net -> [([[Double]], [[Double]])] -> IO (Double, Net)
train_measure_quality s net sample = undefined
{--
  do
                                            --let predi1 = map (output_classic net) $ fst sample
                                            let (a,b,err1) = calculate_error net (head sample)
                                            n_net <- training_batches_classic net sample s
                                            --let predi = map (output_classic n_net) $ fst sample
                                            let (a,b,err) =  calculate_error n_net (head sample) --1/fromIntegral (length (snd sample)) $ sum $ zipWith (\a b -> 1 - (sum  $(zipWith( \x y -> x*y) a b))) predi (snd sample)
                                            let infinity = (read "Infinity")::Double
                                            let error' = if isNaN err then infinity else err
                                            let error'' = if error' > err1 then error' +9999 else error'
                                            return (error'', n_net)
                                            --}
