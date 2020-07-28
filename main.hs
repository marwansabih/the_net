import           Backpropagation
import           Data
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Graphprinter
import           IMGNetwork
import           Memory
import           MNIST
import           Network
--import           Runner
import           Control.Monad
import           Data.List
import           RunnerClassic
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

test_error_1 =[7,8,9,38,61,104,111,118,124,125,126,149,185,194,215,247,252,256,257,259,271,273,281,290,291,312,315,320,321,324,326,344,358,359,381,391,403,404,406,414,432,445,456,457,490,495,530,551,567,575,582,583,619,627,644,646,658,673,684,689,717,738,795,813,833,837,844,874,882,924,932,939,965,990,992,1003,1014,1032,1039,1044,1074,1090,1096,1103,1105,1114,1138,1154,1181,1182,1184,1186,1194,1204,1224,1226,1232,1242,1247,1256,1260,1266,1287,1309,1319,1325,1328,1355,1364,1372,1395,1414,1422,1425,1429,1433,1444,1469,1500,1504,1522,1530,1549,1551,1553,1554,1559,1562,1565,1568,1584,1587,1594,1620,1621,1626,1671,1678,1686,1702,1709,1712,1716,1717,1734,1737,1743,1745,1754,1772,1788,1790,1808,1817,1819,1832,1839,1855,1871,1878,1883,1899,1901,1903,1911,1941,1944,1952,1968,1982,1987,2023,2033,2044,2047,2048,2049,2053,2054,2070,2091,2098,2109,2115,2118,2135,2144,2161,2182,2185,2192,2198,2203,2216,2224,2232,2266,2272,2279,2286,2292,2296,2299,2326,2387,2390,2406,2408,2414,2426,2433,2448,2488,2512,2514,2526,2542,2544,2548,2550,2559,2560,2570,2578,2582,2597,2598,2604,2607,2618,2630,2648,2654,2656,2660,2708,2720,2749,2751,2780,2810,2817,2823,2834,2845,2871,2877,2896,2921,2926,2939,2940,2941,2945,2953,2976,2979,2988,3001,3005,3021,3033,3044,3060,3062,3073,3090,3106,3115,3117,3130,3136,3167,3185,3189,3207,3215,3216,3218,3225,3242,3251,3254,3284,3310,3316,3330,3339,3369,3375,3392,3405,3422,3426,3439,3446,3478,3503,3506,3511,3520,3529,3549,3550,3558,3559,3567,3574,3581,3597,3600,3604,3618,3634,3640,3662,3674,3681,3687,3710,3711,3727,3732,3749,3751,3757,3767,3773,3780,3796,3798,3801,3808,3811,3818,3821,3838,3853,3854,3869,3893,3924,3941,3950,3951,3967,3968,3984,3985,3995,4000,4002,4007,4027,4063,4065,4075,4093,4102,4103,4114,4116,4123,4124,4125,4134,4140,4151,4154,4163,4165,4174,4176,4180,4211,4224,4248,4250,4255,4265,4289,4329,4344,4363,4365,4369,4374,4380,4384,4391,4400,4403,4411,4415,4425,4439,4442,4444,4457,4480,4500,4515,4534,4536,4547,4552,4554,4567,4571,4585,4601,4671,4673,4683,4693,4706,4720,4724,4731,4735,4740,4746,4751,4757,4776,4807,4814,4823,4837,4838,4876,4879,4880,4886,4890,4910,4956,4966,4981,4990,4997,5015,5017,5026,5060,5067,5078,5135,5138,5140,5199,5201,5250,5331,5401,5434,5493,5495,5522,5546,5564,5569,5600,5611,5620,5631,5634,5641,5642,5643,5672,5676,5677,5734,5745,5749,5753,5771,5835,5854,5856,5858,5868,5871,5885,5887,5891,5899,5908,5913,5937,5955,5972,5973,5981,5982,5985,5986,5995,5997,6015,6024,6028,6043,6045,6056,6059,6065,6071,6093,6166,6168,6173,6243,6303,6344,6347,6400,6426,6434,6451,6452,6453,6471,6490,6532,6538,6558,6560,6574,6576,6590,6597,6598,6599,6605,6608,6614,6621,6651,6692,6735,6741,6747,6784,6817,6827,6841,6850,6927,6956,7089,7182,7185,7197,7216,7219,7248,7258,7268,7345,7404,7432,7434,7459,7492,7505,7539,7551,7565,7580,7587,7619,7651,7683,7691,7795,7797,7828,7830,7838,7874,7886,7893,7917,7921,7991,8057,8059,8077,8094,8095,8102,8126,8136,8181,8183,8196,8207,8223,8263,8272,8273,8288,8294,8311,8339,8362,8372,8375,8383,8408,8413,8431,8496,8504,8508,8522,8523,8556,8578,8584,8615,8645,8656,8849,8865,8868,8882,8974,9009,9019,9024,9026,9141,9158,9214,9252,9280,9316,9439,9449,9517,9534,9536,9587,9607,9613,9634,9636,9664,9669,9679,9696,9698,9700,9712,9716,9729,9740,9749,9755,9765,9770,9792,9808,9829,9831,9832,9839,9855,9856,9867,9874,9888,9890,9901,9904,9905,9910,9916,9918,9940,9941,9944,9982,9992]

test_error_2 =[35,38,65,84,110,115,124,149,179,184,217,221,232,247,259,264,266,268,273,274,290,315,316,318,320,321,326,344,389,399,404,412,421,432,434,445,446,448,457,470,488,495,530,538,543,582,583,606,610,627,629,641,646,650,659,664,674,684,685,714,718,729,738,760,775,781,793,800,806,813,833,839,844,846,876,878,882,893,899,900,914,922,924,951,965,983,990,992,995,1000,1002,1003,1007,1012,1014,1018,1026,1039,1044,1074,1090,1093,1096,1103,1107,1112,1138,1178,1181,1182,1192,1202,1219,1224,1226,1228,1229,1232,1242,1247,1248,1249,1252,1260,1274,1281,1287,1299,1319,1320,1326,1328,1331,1344,1353,1364,1394,1395,1398,1414,1415,1422,1433,1443,1444,1446,1464,1475,1476,1477,1494,1500,1522,1523,1524,1525,1527,1530,1541,1545,1549,1551,1553,1559,1562,1568,1583,1584,1587,1594,1620,1626,1664,1669,1671,1678,1681,1682,1686,1694,1702,1717,1719,1734,1754,1756,1773,1774,1790,1813,1823,1839,1850,1851,1855,1871,1878,1880,1893,1899,1901,1903,1909,1911,1940,1941,1943,1952,1955,1961,1970,1973,1982,1984,1987,2004,2047,2049,2053,2054,2056,2063,2070,2073,2093,2098,2107,2109,2118,2119,2135,2142,2148,2168,2177,2179,2182,2195,2203,2208,2224,2242,2272,2279,2289,2293,2297,2309,2369,2382,2387,2393,2395,2406,2414,2419,2425,2430,2433,2467,2469,2488,2496,2512,2520,2521,2526,2542,2544,2550,2561,2570,2582,2589,2596,2597,2598,2600,2604,2607,2617,2620,2630,2636,2637,2640,2648,2651,2654,2658,2708,2720,2724,2739,2750,2751,2758,2771,2778,2780,2802,2810,2823,2834,2859,2860,2877,2896,2915,2921,2930,2939,2945,2952,2953,2960,2970,3002,3023,3033,3046,3060,3062,3064,3073,3100,3112,3115,3117,3130,3139,3153,3157,3158,3168,3189,3206,3213,3216,3218,3229,3246,3252,3254,3262,3279,3284,3289,3316,3323,3339,3342,3345,3367,3369,3374,3405,3422,3440,3442,3492,3503,3509,3514,3519,3520,3521,3550,3552,3555,3558,3559,3563,3565,3567,3571,3583,3597,3599,3604,3617,3618,3626,3629,3635,3640,3662,3674,3681,3716,3727,3749,3751,3756,3757,3762,3763,3767,3773,3778,3780,3794,3801,3808,3810,3811,3816,3818,3838,3846,3853,3862,3869,3871,3876,3887,3893,3906,3926,3941,3946,3951,3968,3984,3985,3987,3988,3992,3994,3997,4000,4007,4017,4027,4037,4058,4063,4065,4068,4075,4078,4096,4102,4112,4116,4121,4123,4124,4140,4159,4165,4207,4209,4211,4224,4231,4238,4243,4248,4251,4255,4265,4271,4289,4314,4341,4344,4356,4363,4369,4374,4380,4384,4400,4411,4415,4419,4425,4434,4439,4449,4452,4489,4497,4500,4508,4511,4513,4523,4534,4536,4545,4547,4567,4572,4575,4579,4601,4608,4625,4639,4671,4673,4683,4692,4724,4731,4735,4737,4744,4750,4751,4759,4761,4795,4807,4814,4822,4823,4829,4838,4839,4849,4852,4861,4874,4879,4890,4952,4956,4975,4978,4981,4982,4986,4997,5118,5129,5138,5142,5199,5201,5331,5403,5434,5489,5493,5495,5522,5543,5547,5548,5557,5564,5569,5586,5605,5626,5634,5636,5641,5642,5643,5656,5676,5677,5719,5734,5749,5757,5771,5797,5802,5833,5835,5836,5855,5856,5858,5874,5876,5881,5887,5888,5891,5937,5940,5950,5951,5964,5972,5981,5985,5986,5995,5997,6024,6028,6042,6043,6045,6049,6053,6056,6059,6071,6093,6153,6166,6168,6174,6189,6212,6219,6235,6238,6248,6296,6317,6336,6343,6347,6359,6372,6376,6403,6404,6434,6538,6555,6559,6560,6571,6572,6574,6595,6597,6608,6612,6614,6617,6629,6651,6741,6755,6783,6806,6816,6817,6838,6839,6850,6864,7004,7016,7080,7182,7197,7208,7209,7216,7220,7254,7256,7258,7259,7277,7302,7339,7383,7402,7432,7455,7482,7502,7511,7514,7534,7615,7619,7633,7660,7666,7691,7694,7711,7720,7803,7812,7823,7858,7886,7915,7921,7924,7928,7991,7998,8068,8078,8094,8097,8183,8196,8255,8281,8311,8329,8408,8410,8413,8456,8477,8497,8502,8519,8522,8523,8556,8578,8584,8585,8615,8627,8639,8645,8649,8722,8931,8942,8974,9008,9009,9015,9019,9024,9046,9161,9168,9210,9225,9271,9280,9293,9316,9321,9326,9338,9354,9394,9402,9423,9433,9450,9482,9490,9532,9538,9587,9607,9625,9626,9632,9634,9636,9652,9664,9673,9679,9686,9692,9695,9698,9719,9726,9729,9733,9745,9749,9755,9764,9770,9777,9783,9834,9839,9855,9858,9867,9888,9892,9900,9904,9916,9922,9926,9934,9944,9959,9967]

draw_sample_ids :: Int -> Int -> [Int] -> Net -> IO ([Int],[[Int]])
draw_sample_ids nr bs ids net = draw_sample_ids' nr bs ids ([],[]) net

draw_sample_ids' :: Int -> Int -> [Int] -> ([Int],[[Int]]) -> Net -> IO ([Int],[[Int]])
draw_sample_ids' 0 bs ids found net = return found
draw_sample_ids' nr bs ids (_,xs) net = do
  (f,ids') <- draw_ids bs ids net
  draw_sample_ids' (nr-1) bs ids' (ids',f:xs) net



draw_ids :: Int -> [Int] -> Net -> IO ([Int],[Int])
draw_ids nr ids net = draw_ids' nr [] ids net

draw_ids' :: Int -> [Int] ->  [Int] -> Net ->IO ([Int],[Int])
draw_ids' 0  found rest net= return (found, rest)
draw_ids' nr found rest net= do
  idx <- randomRIO (0::Int, (length rest -1))
  let (xs,f:ys) = splitAt idx rest
  isCorrect <- correct_train_prediction_by_nr net f
  if isCorrect
    then draw_ids' nr found (xs ++ ys) net
    else draw_ids' (nr-1) (f:found) (xs ++ ys) net

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
                                                               (set, ids') <- draw_sample_ids 10 10 ids net
                                                               sample <- mapDraw ids'
                                                               --let sample = map format sample'
                                                               --set <- mnist_set
                                                               print $ times * 10
                                                               print "start training"
                                                               net' <- update_random_net_con_classic 10 sample net s
                                                               print "start saving"
                                                               if mod times 2 == 0
                                                                 then save_net filename net'
                                                                 else save_net (filename ++ "2") net'
                                                               --if ( (times < 1000) && (mod times 100 == 0) )
                                                               --   then save_net filename net
                                                               --   else print "not saving"
                                                               print times
                                                               update_and_save_image  set (times + 1) filename net' s

run_and_save_image :: [Int] -> Int -> String -> ( Net -> [([[Double]], [[Double]])] -> Double ->IO Net) -> Net -> Double -> IO Net
run_and_save_image [] _ _ _  net _ = return net
run_and_save_image ids times filename f  net s = do
                                                               --set <- if (odd times) then draw_mnist_training_batch 10
                                                              --                                 else draw_wrong_mnist_training_batch nr 10
                                                              (set, ids') <- draw_sample_ids 2 10 ids net
                                                              sample <- mapDraw ids'
                                                              print $ times * 100
                                                              --set <- mnist_set
                                                              print "start training"
                                                              net' <- f  net sample s
                                                              print "start saving"
                                                              if mod times 2 == 0
                                                                then save_net filename net'
                                                                else save_net (filename ++ "2") net'
                                                               --if ( (times < 1000) && (mod times 100 == 0) )
                                                               --   then save_net filename net
                                                               --   else print "not saving"
                                                              print times
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



main::IO()
main = do
          getCurrentTime >>= print
          --random_net <- generate_image_net 28 28 10 10 400 40
          random_net <- load_net "image_net2" --load_net  "00er" --"image_net_2020-04-30_23-00"
          --find_best_random_net_classic nr_nets training_steps bs height width depth nr_neuron nr_con sample s
          --random_net <- find_best_random_net_classic 50 100
          --save_net "image_net_500_fresh" random_net
          getCurrentTime >>= print
          --(net1, net2) <- alter_connections 10 random_net
          --analyse_network net1
          --analyse_network net2
          analyse_network random_net
          --wrong_train_predictions random_net >>= print
          --print $ length test_error_2
          --wrong_test_predictionss random_net -- >>= print
          -- update_random_net_con nr_times nr_trainings bs nr_con sample net s
          -- update_random_net_con_classic nr_times nr_trainings bs nr_con sample net s
          --let f x y = training_batches_classic 1 3 y x
          --let f x y = training_normed_batches_classic 3 2 y x
          --let f = update_random_net_con_classic 1 100 2 10
          --trained_random_net <- update_and_save_image [0..59999] 0 "image_net"  random_net  0.001
          trained_random_net <- run_and_save_image [0..59999] 0 "image_net" training_batches_classic  random_net  0.001
          (img, num) <-draw_mnist_test 10
          let result = output_classic trained_random_net (concat img)
          render_mnist (img, result)
