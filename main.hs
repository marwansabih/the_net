import           Data
import           Memory
import           Network
import           Runner

--ghc -O2 -o main.out main.hs -fprof-auto  -fprof-cafs -fforce-recomp

-- training_batches nr_times bs net sample s
-- find_best_fully_connected_net nr_nets training_steps bs width depth sample s
-- find_best_random_net nr_nets training_steps bs width depth nr_neuron nr_con sample s
-- update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
-- update_random_net_con nr_times nr_trainings bs nr_con sample net s
-- output net input

-- alter_neurons nr_neuron nr_cons net
-- alter_connections nr_con net
-- generate_fully_connected_net width depth
-- generate_random_net width depth nr_neuron nr_con

-- save_net filename net
-- load_net filename

main::IO()
main = do
          -- Example 1: Best fully connected net out of 12 - afterwards trained
          (inp,outs) <- dat
          -- find_best_fully_connected_net nr_nets training_steps bs nr_steps width depth sample s
          best_f_net <- find_best_fully_connected_net 20 100 10  7 6 (inp,outs) 0.001
          -- training_batches nr_times bs net sample s
          trained_f_net <- training_batches 100000 10 best_f_net (inp,outs) 0.001
          putStrLn "Fully Connected Network"
          putStr "Prediction of first input from sample: "
          print $ output trained_f_net (inp !! 0)
          putStr "Expected Output: "
          print  $ outs !! 0

         -- Example 2: a random network is created and trained
         -- alternative for finding a good network:
         --  find_best_random_net nr_nets training_steps bs width depth nr_neuron nr_con sample s
         -- alternatives for training:
         -- update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
         -- update_random_net_con nr_times nr_trainings bs nr_con sample net s
          random_net <- generate_random_net 7 10 30  7
          -- update_random_net_con nr_times nr_trainings bs nr_con sample net s
          trained_random_net <- update_random_net_con 100 100 10 3 (inp,outs) random_net  0.001
          putStrLn "Random Network"
          putStr "Prediction of first input from sample: "
          print $ output trained_random_net (inp !! 0)
          putStr "Expected Output: "
          print  $ outs !! 0
