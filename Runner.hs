module Runner
(
        training_batches,
        find_best_fully_connected_net,
        find_best_random_net ,
        update_random_net,
        output
) where

import           Control.Monad
import           Data.Time
import           Net
import           Network
import           Types

training_batches :: Int -> Int -> Net -> ([[Double]],[[Double]]) -> Double -> IO Net
training_batches 0 _ net  _ _  = return net
training_batches nr_times bs net sample s = do
                                                            (inp,out) <- get_random_batch bs sample
                                                            let net' = training_batch net sample (s / fromIntegral bs)
                                                            print $ calculate_error net' $ sample
                                                            training_batches (nr_times-1) bs net' sample s

calculate_error :: Net ->  ([[Double]],[[Double]])   ->Double
calculate_error net (inp,out) = (sum dist) / ( fromIntegral ( length out ))
                                        where
                                            preds = map (output net) inp
                                            dist = concat $ zipWith( zipWith(\a b -> (a-b)^2)) preds out


output :: Net -> [Double] -> [Double]
output net input = map(\(Node (a,_) _) -> a) layer
                    where
                         r_net = reset net
                         f_net = f_propagate $set_input r_net input
                         layer = last $ app f_net

find_best_fully_connected_net :: Int -> Int -> Int  -> Int -> Int -> ([[Double]],[[Double]]) -> Double -> IO Net
find_best_fully_connected_net nr_nets training_steps bs width depth sample s =
            do
                x <- generate_fully_connected_net width depth
                xs <- sequ $replicate (nr_nets-1) $ generate_fully_connected_net width depth
                (err,b_net) <- foldM (\x y -> compete_batch training_steps bs s (snd x) y  sample) (0::Double,x) xs
                print err
                time <-getCurrentTime
                print time
                return $ b_net



find_best_random_net :: Int -> Int -> Int -> Int -> Int -> Int -> Int  -> ([[Double]],[[Double]]) -> Double -> IO Net
find_best_random_net nr_nets training_steps bs width depth nr_neuron nr_con sample s =
     do
         x <- generate_random_net width depth nr_neuron nr_con
         xs <- sequ $replicate (nr_nets-1) $ generate_random_net width depth nr_neuron nr_con
         (err,b_net) <- foldM (\x y -> compete_batch training_steps bs s (snd x) y sample)  (0::Double,x) xs
         print(err)
         time <-getCurrentTime
         print time
         return b_net

update_random_net :: Int ->Int -> Int -> Int -> Int -> ([[Double]],[[Double]]) -> Net -> Double ->IO Net
update_random_net 0 _ _ _ _ _ net _ = return net
update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s = do
                                                            time <-getCurrentTime
                                                            print time
                                                            n_net <-  update_random_net' nr_trainings bs nr_alt_neuron nr_con sample net s
                                                            net' <- update_random_net (nr_times-1) nr_trainings bs nr_alt_neuron nr_con sample n_net s
                                                            return net'

update_random_net' :: Int -> Int -> Int -> Int -> ([[Double]],[[Double]]) -> Net -> Double ->IO Net
update_random_net' nr_trainings bs nr_alt_neuron nr_con sample net s  = do
                                                              (net1,net2) <- alter_neurons nr_alt_neuron nr_con net
                                                              (error', net') <- compete_batch nr_trainings bs s net1 net2 sample
                                                              print error'
                                                              return net'

compete_batch :: Int -> Int  -> Double -> Net -> Net -> ([[Double]],[[Double]]) -> IO (Double, Net)
compete_batch nr_trainings bs s net1 net2 sample  = do
                                                        (error1, n_net1) <- train_measure_quality nr_trainings bs s net1 sample
                                                        (error2, n_net2) <- train_measure_quality nr_trainings bs s net2 sample
                                                        if (error1 < error2) then return (error1, n_net1) else return (error2, n_net2)

train_measure_quality :: Int -> Int -> Double -> Net -> ([[Double]], [[Double]]) -> IO (Double, Net)
train_measure_quality nr_trainings bs s net sample =  do
                                            n_net <- training_batches nr_trainings bs net sample s
                                            let predi = map (output n_net) $ fst sample
                                            let err =  sum $ zipWith (\a b -> sum  $(zipWith( \x y -> (x-y)^2)) a b) predi (snd sample)
                                            let infinity = (read "Infinity")::Double
                                            let error' = if isNaN err then infinity else err
                                            return (err, n_net)
