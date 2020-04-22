import           Data
import           Graphprinter
import           IMGNetwork
import           Memory
import           MNIST
import           Network
import           Runner
import           Types
--ghc -O2 -optc-O3 -optc-ffast-math -o main.out main.hs -fprof-auto  -fprof-cafs -fforce-recomp
--ghc -O2 -optc-O3  -threaded -optc-ffast-math -fexcess-precision -funfolding-use-threshold=16 -o main.o the_net.hs  -fprof-auto  -fprof-cafs -fforce-recomp

-- training_batches nr_times bs net sample s
-- find_best_fully_connected_net nr_nets training_steps bs width depth sample s
-- find_best_random_net nr_nets training_steps bs width depth nr_neuron nr_con sample s
-- update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
-- update_random_net_con nr_times nr_trainings bs nr_con sample net s
-- update_add_del_net_con nr_times nr_trainings bs nr_con sample net s
-- update_add_net_con nr_times nr_trainings bs nr_con sample net s
-- update_del_net_con nr_times nr_trainings bs nr_con sample net s
-- output net input

-- alter_neurons nr_neuron nr_cons net
-- alter_connections nr_con net
-- generate_fully_connected_net width depth
-- generate_random_net width depth nr_neuron nr_con

-- save_net filename net
-- load_net filename

--output_graph node_radius filename net

run_and_save_image :: Int -> String -> (([[Double]], [[Double]]) -> Net -> Double ->IO Net) -> Net -> Double -> IO Net
run_and_save_image 0 _ _  net _ = return net
run_and_save_image times filename f  net s = do
                                                               set <- draw_mnist_training_batch 500
                                                               let the_set = format set
                                                               net <- f the_set net s
                                                               save_net filename net
                                                               run_and_save_image  (times-1) filename f net s


run_and_save :: Int -> String -> (Net -> Double ->IO Net) -> Net -> Double -> IO Net
run_and_save 0 _ _  net _ = return net
run_and_save times filename f  net s = do
                                                               net <- f net s
                                                               save_net filename net
                                                               run_and_save (times-1) filename f net s


run_and_save_two :: Int -> String -> (Net -> Double ->IO Net) -> (Net -> Double ->IO Net) -> Net -> Double -> IO Net
run_and_save_two  0 _ _ _ net _ = return net
run_and_save_two times filename f g  net s = do
                                                               net' <- f net s
                                                               net'' <- f net' s
                                                               save_net filename net''
                                                               run_and_save_two  (times-1) filename f g net'' s


format :: [([[Double]], Double)]  -> ([[Double]], [[Double]])
format xs = format' xs ([],[])

format' :: [([[Double]], Double)] -> ([[Double]], [[Double]]) -> ([[Double]], [[Double]])
format' [] found           = found
format' ((a,b):xs) (as,bs) = ( ( map ((1.0/255.0)*) (concat a) ) :as, ([b]):bs)

main::IO()
main = do
          -- Example 1: Best fully connected net out of 12 - afterwards trained
          (inp,outs) <- dat
          -- find_best_fully_connected_net nr_nets training_steps bs nr_steps width depth sample s
          --best_f_net <- find_best_fully_connected_net 20 100 10  7 6 (inp,outs) 0.001
          -- training_batches nr_times bs net sample s
          --trained_f_net <- training_batches 100000 10 best_f_net (inp,outs) 0.001
          putStrLn "Fully Connected Network"
          putStr "Prediction of first input from sample: "
          --print $ output trained_f_net (inp !! 0)
          putStr "Expected Output: "
          print  $ outs !! 0

         -- Example 2: a random network is created and trained
         -- alternative for finding a good network:
         --  find_best_random_net nr_nets training_steps bs width depth nr_neuron nr_con sample s
         -- alternatives for training:
         -- update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
         -- update_random_net_con nr_times nr_trainings bs nr_con sample net s
          -- random_net <- generate_random_net 7 10 30 4
          random_net <- generate_image_net 28 28 10 200 7
          --random_net <- load_net "image_net"
          let g =(\x y -> training_batches 100 1 x (inp,outs) y)
          let f = update_random_net_con 1 100 2 30
          trained_random_net <- run_and_save_image 1000 "image_net" f  random_net  0.001
          --random_net <- load_net "change_able_net"
          --print random_net
          --output_graph node_radius filename net
          --output_graph 30 "neuron_3cons.png"  random_net
          -- update_random_net_con nr_times nr_trainings bs nr_con sample net s
          -- let f = update_add_net_con 5 1000 10 4 (inp,outs)
          -- let g = update_del_net_con 5 1000 10 1 (inp,outs)
          --let f = update_random_net_con 5 100 10 4  (inp,outs)
          --let h = update_random_net 5 100 10 1 4  (inp,outs)
          --let g =(\x y -> training_batches 100 10 x (inp,outs) y)
          --trained_random_net <- run_and_save_two 1000 "change_able_net" f f random_net  0.001
          --trained_random_net <- run_and_save 3000 "saved_net_4_con_n" g random_net  0.0001
          putStrLn "Random Network"
          putStr "Prediction of first input from sample: "
          --print $ output trained_random_net (inp !! 0)
          putStr "Expected Output: "
          print  $ outs !! 0
