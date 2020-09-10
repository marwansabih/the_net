import           Backpropagation
import           Data
import           Data.List
import           Data.Time.Clock    (diffUTCTime, getCurrentTime)
import           Graphprinter
import           IMGNetwork
import           Memory
import           MNIST
import           Network
--import           Runner
import           Config
import           Control.Monad
import           Data.List
import           RunnerClassic
import           System.Directory
import           System.Environment
import           System.Random
import           Types

--ghc -O2 -optc-O3 -optc-ffast-math -o main.out main.hs -fprof-auto  -fprof-cafs -fforce-recomp
--ghc -O2 -optc-O3  -threaded -fexcess-precision -funfolding-use-threshold=0 -o main.out main.hs -fforce-recomp -rtsopts

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

defaultConfig = "image_net_28_28_100_10_100_100"

config :: IO Config
config = do
  a <- getArgs;
  if a == [] then
    do
      conf <- readConfig defaultConfig
      putStrLn $ "DefaultConfig path: " ++ path conf
      return conf
  else
    do
      conf <- readConfig (a !! 0)
      putStrLn $ "Config path: " ++ (path conf)
      return conf


draw_sample_ids :: Int -> Int -> [Int] -> Net -> IO ([Int],[[Int]])
draw_sample_ids nr bs ids net = draw_sample_ids' nr bs ids ([],[]) net

draw_sample_ids' :: Int -> Int -> [Int] -> ([Int],[[Int]]) -> Net -> IO ([Int],[[Int]])
draw_sample_ids' 0 bs ids found net = return found
draw_sample_ids' _ bs [] found net = return found
draw_sample_ids' nr bs ids (_,xs) net = do
  (f,ids') <- draw_ids bs ids net
  draw_sample_ids' (nr-1) bs ids' (ids',f:xs) net



draw_ids :: Int -> [Int] -> Net -> IO ([Int],[Int])
draw_ids nr ids net = draw_ids' nr [] ids net

draw_ids' :: Int -> [Int] ->  [Int] -> Net ->IO ([Int],[Int])
draw_ids' 0  found rest net= return (found, rest)
draw_ids' _  found [] net= return (found, [])
draw_ids' nr found rest net= do
  idx <- randomRIO (0::Int, (length rest -1))
  let (xs,f:ys) = splitAt idx rest
  draw_ids' (nr-1) (f:found) (xs ++ ys) net
  --isCorrect <- correct_train_prediction_by_nr net f
  --if isCorrect
    --then draw_ids' nr found (xs ++ ys) net
    --else draw_ids' (nr-1) (f:found) (xs ++ ys) net

draw_sample_ids_ :: Int -> Int -> [Int] -> Net -> IO ([Int],[[Int]])
draw_sample_ids_ nr bs ids net = draw_sample_ids'_ nr bs ids ([],[]) net

draw_sample_ids'_ :: Int -> Int -> [Int] -> ([Int],[[Int]]) -> Net -> IO ([Int],[[Int]])
draw_sample_ids'_ 0 bs ids found net = return found
draw_sample_ids'_ _ bs [] found net = return found
draw_sample_ids'_ nr bs ids (_,xs) net = do
  (f,ids') <- draw_ids_ bs ids net
  draw_sample_ids'_ (nr-1) bs ids' (ids',f:xs) net



draw_ids_ :: Int -> [Int] -> Net -> IO ([Int],[Int])
draw_ids_ nr ids net = draw_ids'_ nr [] ids net

draw_ids'_ :: Int -> [Int] ->  [Int] -> Net ->IO ([Int],[Int])
draw_ids'_ 0  found rest net= return (found, rest)
draw_ids'_ _  found [] net= return (found, [])
draw_ids'_ nr found rest net= do
  idx <- randomRIO (0::Int, (length rest -1))
  let (xs,f:ys) = splitAt idx rest
  draw_ids'_ (nr-1) (f:found) (xs ++ ys) net

mapDraw2 :: [[Int]] -> IO [[([Double],[Double])]]
mapDraw2 xs = mapDraw2' xs []

mapDraw2' []  xs = return xs
mapDraw2' (x:xs) ys = do
  y <- draw_mnist_training_batch_by_ids x
  mapDraw2' xs ((format2 y):ys)

format2 y = map (\(a,b) -> (concat a, b)) y

mapDraw xs = mapDraw' xs []

mapDraw' []  xs = return xs
mapDraw' (x:xs) ys = do
  y <- draw_mnist_training_batch_by_ids x
  mapDraw' xs ((format y):ys)

update_and_save_image :: [Int] -> Int -> String -> Net -> Double -> IO Net
update_and_save_image [] _ _  net _ = return net
update_and_save_image ids times filename  net s = do
                                                               --set <- if (odd times) then draw_mnist_training_batch 10
                                                              --                                 else draw_wrong_mnist_training_batch nr 10
                                                               steps <- steps <$> config
                                                               batchSize <- batchSize <$> config
                                                               (set, ids') <- draw_sample_ids_ steps batchSize ids net
                                                               sample <- mapDraw ids'
                                                               --let sample = map format sample'
                                                               --set <- mnist_set
                                                               print "start training"
                                                               net' <- update_random_net_con_classic steps sample net s
                                                               print "start saving"
                                                               if mod times 2 == 0
                                                                 then save_net filename net'
                                                                 else save_net (filename ++ "2") net'
                                                               --if ( (times < 1000) && (mod times 100 == 0) )
                                                               --   then save_net filename net
                                                               --   else print "not saving"
                                                               print $ (times +1 ) * 1000
                                                               update_and_save_image  set (times + 1) filename net' s

run_and_save_image :: [Int] -> Int -> String -> ( Net -> [[([Double],[Double])]]  -> String -> Double ->IO Net) -> Net -> Double -> IO Net
run_and_save_image [] _ _ _  net _ = return net
run_and_save_image ids times filename f  net s = do
                                                               --set <- if (odd times) then draw_mnist_training_batch 10
                                                              --                                 else draw_wrong_mnist_training_batch nr 10

                                                              steps <- steps <$> config
                                                              batchSize <- batchSize <$> config
                                                              s <- learningRate <$> config
                                                              path <- path <$> config
                                                              print s
                                                              (set, ids') <- draw_sample_ids steps batchSize ids net
                                                              sample <- mapDraw2 ids'
                                                              print "start training"
                                                              net' <- f  net sample (path ++ "/error.log") s
                                                              print "start saving"
                                                              if mod times 2 == 0
                                                                then save_net (filename) net'
                                                                else save_net (filename ++ "_2") net'
                                                              print $ (times+1) * 1000
                                                              run_and_save_image  set (times + 1) filename f net' s


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


axs = [0..59999]

netExists :: IO Config -> IO Bool
netExists config = do
  path <- path <$> config
  dirs <- listDirectory path
  let nets = filter(\x -> isPrefixOf "image_net" x) dirs
  return $ length nets > 0

readOrCreateNet config = do
  width <- imgWidth <$> config
  height <- imgHeight <$> config
  layers <- nrLayers <$> config
  classes <- nrClasses <$> config
  neurons <- nrNeurons <$> config
  cons <- nrCons <$> config
  file <- filepath <$> config
  exists <- netExists config
  if exists
    then load_net  file
    else generate_image_net width height layers classes neurons cons



main::IO()
main = do
          getCurrentTime >>= print
          file <- filepath <$> config
          random_net <- readOrCreateNet config
          s <- learningRate <$> config
          random_net <- run_and_save_image axs 0 file training_batches_classic  random_net  s
          --random_net <- run_and_save_image hs 0 "save/image_net" training_batches_classic  random_net  0.001
          random_net <- run_and_save_image axs 0 file  training_batches_classic  random_net  s
          --random_net <- run_and_save_image hs 0 "save/image_net" training_batches_classic  random_net  0.001
          random_net <- run_and_save_image axs 0 file  training_batches_classic  random_net  s
          --random_net <- run_and_save_image hs 0 "save/image_net" training_batches_classic  random_net  0.001
          --random_net <- run_and_save_image axs 0 "save/image_net" training_batches_classic  random_net  0.001
          --trained_random_net <- run_and_save_image xs 0 "save/image_net" training_batches_classic  random_net  0.001
          --trained_random_net <- run_and_save_image [0..59999] 0 "save/image_net" training_batches_classic  random_net  0.01
          (img, num) <-draw_mnist_test 10
          let result = output_classic random_net (concat img)
          render_mnist (img, result)
