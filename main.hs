import           Data
import           Network
import           Runner

--ghc -O2 -o main.out main.hs -fprof-auto  -fprof-cafs -fforce-recomp

main::IO()
main = do
          (inp,outs) <- dat
          --bf_net <- find_best_fully_connected_net 250 250 4 30  (take 10000 outputs, take 10000 inputs)
          putStrLn "Fully Connected Network"
          putStrLn "Prediction:"
          --print $ let f_net = train_data bf_net (take 1500 outputs) (take 1500 inputs)
            --    in output f_net (inp !! 0)
          putStrLn "Expected Output:"
          print  $ outs !! 0
          -- generate_random_net witdh depth nr_neuron nr_con
          --find_best_random_net nr_nets nr_steps width depth nr_neuron nr_con sample
          putStrLn "Random Generated Network"
          putStrLn "Prediction:"
          --the_net <- find_best_random_net 100 1000 4 30 40 12 (outs, inp)
          --train and update net, while altering neurons
          --update_random_net nr_times nr_steps nr_alt_neuron nr_con sample net
          --b_net <- update_random_net 1 1000 10 8 (take 10000 inputs, take 10000 outputs) the_net
          --the_net <- generate_fully_connected_net 7 7
          the_net <- generate_random_net 7 30 100  7
          n_net <- change_out_net 1 the_net
          --update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
          b_net <- update_random_net 1000 5000 10 5  7 (inp,outs) n_net 0.01
          -- training_batches nr_times bs net sample s
          --b_net <- training_batches 10000 10 n_net (inp,outs) 0.01
          print $ output b_net (outs !! 0)
          putStrLn "Expected Output:"
          print  $ outs !! 0
