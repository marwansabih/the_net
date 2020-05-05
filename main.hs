import           Backpropagation
import           Data
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Graphprinter
import           IMGNetwork
import           Memory
import           MNIST
import           Network
import           Runner
import           RunnerClassic
import           Types
--ghc -O2 -optc-O3 -optc-ffast-math -o main.out main.hs -fprof-auto  -fprof-cafs -fforce-recomp
--ghc -O2 -optc-O3  -threaded -fexcess-precision -funfolding-use-threshold=16 -o main.out main.hs  -fprof-auto  -fprof-cafs -fforce-recomp



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
                                                               --set <- draw_mnist_training_batch 1000
                                                               set <- mnist_set
                                                               let the_set = format set
                                                               print "start training"
                                                               net' <- f the_set net s
                                                               print "start saving"
                                                               save_net filename net'
                                                               --if ( (times < 1000) && (mod times 100 == 0) )
                                                               --   then save_net filename net
                                                               --   else print "not saving"
                                                               run_and_save_image  (times-1) filename f net' s


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


format :: [([[Double]], [Double])]  -> ([[Double]], [[Double]])
format xs = format' xs ([],[])

format' :: [([[Double]], [Double])] -> ([[Double]], [[Double]]) -> ([[Double]], [[Double]])
format' [] found           = found
format' ((a,b):xs) (as,bs) =format' xs  ( ( map ((1.0/255.0)*) (concat a) ) :as, b:bs)

main::IO()
main = do
          getCurrentTime >>= print
          random_net <- generate_image_net 28 28 200 10 4000 40
          --random_net <- load_net  "image_net" --"image_net_2020-04-30_23-00"
          getCurrentTime >>= print
          analyse_network random_net
          -- update_random_net_con nr_times nr_trainings bs nr_con sample net s
          -- update_random_net_con_classic nr_times nr_trainings bs nr_con sample net s
          let f x y = training_batches_classic 100 2 y x --training_normed_batches_classic 3 2 y x
          --let f = update_random_net_con_classic 1 100 2 10
          trained_random_net <- run_and_save_image 1000 "image_net" f  random_net  0.01
          (img, num) <-draw_mnist_test 10
          let result = output_classic trained_random_net (concat img)
          render_mnist (img, result)
