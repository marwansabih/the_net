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
                                                            let net' = training_batch net inp out (s / fromIntegral bs)
                                                            print $ calculate_error net' $ sample
                                                            training_batches (nr_times-1) bs net' sample s

calculate_error :: Net ->  ([[Double]],[[Double]])   ->Double
calculate_error net (inp,out) = (sum dist) / ( fromIntegral ( length out ))
                                        where
                                            preds = map (output net) inp
                                            dist = concat $ zipWith( zipWith(\a b -> (a-b)^2)) preds out

train_data_out :: Int -> Net -> ([[Double]],[[Double]]) -> Double -> IO Net
train_data_out 0  net  _ _  = return net
train_data_out nr_times net sample s = do
                                                            (inp,out) <- get_random_batch 1 sample
                                                            let net' = train net (head inp) (head out) s
                                                            putStr $ show s ++ " "
                                                            print $ calculate_error net' $ sample
                                                            train_data_out (nr_times-1) net' sample s


train_data :: Net -> [[Double]] -> [[Double]] -> Double ->  Net
train_data net input targets s = foldr (\(i,t) net' -> train net' i t s) net i_t
                                          where i_t = zip input targets

output :: Net -> [Double] -> [Double]
output net input = map(\(Node (a,_) _) -> a) layer
                    where
                         r_net = reset net
                         f_net = f_propagate $set_input r_net input
                         layer = last $ app f_net

find_best_fully_connected_net :: Int -> Int -> Int -> Int -> ([[Double]],[[Double]]) -> Double -> IO Net
find_best_fully_connected_net nr_nets nr_steps width depth sample s =
            do
                xs <- generate_fully_connected_net width depth
                x <- sequ $replicate (nr_nets-1) $ generate_fully_connected_net width depth
                let b_net = foldr (\x y -> snd  $ compete x y nr_steps sample s) xs x
                return b_net

find_best_random_net :: Int -> Int -> Int -> Int -> Int -> Int -> ([[Double]],[[Double]]) -> IO Net
find_best_random_net nr_nets nr_steps width depth nr_neuron nr_con sample =
     do
         xs <- generate_random_net width depth nr_neuron nr_con
         x <- sequ $replicate (nr_nets-1) $ generate_random_net width depth nr_neuron nr_con
         (err,b_net) <- foldM (\x y -> compete_batch nr_steps 5 0.1 (snd x) y sample) (0::Double, xs) x
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
compete_batch nr_trainings bs s net1 net2 (inp, out) = do
                                                        (error1, n_net1) <- train_measure_quality nr_trainings bs s net1 inp out
                                                        (error2, n_net2) <- train_measure_quality nr_trainings bs s net2 inp out
                                                        if (error1 < error2) then return (error1, n_net1) else return (error2, n_net2)

train_measure_quality :: Int -> Int -> Double -> Net -> [[Double]] -> [[Double]] -> IO (Double, Net)
train_measure_quality nr_trainings bs s net inp out =  do
                                            n_net <- training_batches nr_trainings bs net (inp,out) s
                                            let predi = map (output n_net) inp
                                            let err =  sum $ zipWith (\a b -> sum  $(zipWith( \x y -> (x-y)^2)) a b) predi out
                                            let infinity = (read "Infinity")::Double
                                            let error' = if isNaN err then infinity else err
                                            return (err, n_net)

compete :: Net -> Net -> Int -> ([[Double]],[[Double]]) -> Double -> (Double, Net)
compete net1 net2 nr_steps sample s = if (error1 < error2) then (error1, n_net1) else  (error2, n_net2)
                                                    where
                                                        inp = take nr_steps (fst sample)
                                                        out = take nr_steps (snd sample)
                                                        n_net1 = train_data net1 inp out s
                                                        n_net2 = train_data net2 inp out s
                                                        predi1 = map (output n_net1) inp
                                                        predi2 = map (output n_net2) inp
                                                        err1 =  sum $ zipWith (\a b -> sum  $(zipWith( \x y -> (x-y)^2)) a b) predi1 out
                                                        err2 =  sum $ zipWith (\a b -> sum  $(zipWith( \x y -> (x-y)^2)) a b) predi2 out
                                                        infinity = (read "Infinity")::Double
                                                        error1 = if isNaN err1 then infinity else err1
                                                        error2 = if isNaN err2 then infinity else err2
