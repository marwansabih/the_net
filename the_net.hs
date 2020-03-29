import           Control.Monad
import           Data.List.Split
import           System.Random

--ghc -O2 -o main.o the_net.hs -fprof-auto  -fprof-cafs -fforce-recomp

dat :: IO ([[Double]],[[Double]])
dat =  do
           file <- readFile "nyc-east-river-bicycle-counts.csv"
           let ls = tail $ lines file
           let tokens = map (splitOn ",") ls
           let num_str = map(\x -> snd (splitAt 3 x)) tokens
           let pairs = map (splitAt 4) $ map (map(\a -> read a :: Double)) num_str
           let normalized = map( \(a,b) -> ( norm_inp a, map(1.0/10000*) b)) pairs
           return $ (\x -> (map fst x, map snd x)) normalized

norm_inp :: [Double] -> [ Double]
norm_inp inp = zipWith (\f a -> f a) [(1.0/100*),(1.0/100*), (1.0*), (1.0/10000*)] inp

data Node = Node (Double,Double) [(Double, (Int,Int))] deriving Show
data Net  = Net [[Node]]

data Node2 a = Node2 a [(Double, (Int,Int))]
data Net2 a = Net2 [[Node2 a]]

type Design = [((Int,Int),[(Double,(Int,Int))])]

poss_twin :: Eq a => [a] -> Bool
poss_twin []     = False
poss_twin (x:xs) = if elem x xs then True else poss_twin xs

check_entries :: Net -> String
check_entries net = concatMap showlayer1 checks
                        where
                              checks = map ( map (\(Node _ ls) -> (poss_twin (map (\(a,b) -> b)  ls ) ) ) )  (app net)
                              showlayer1 x = (concatMap (\a -> if a then "True\n" else "False\n")  x) ++"\n"

instance Show Net where
    show net = concatMap showlayer (app net)
                                   where
                                       nodeMap (Node x b) =  "N " ++ show x ++ " " ++ show (map snd b) ++ "\n"
                                       showlayer x = (concatMap nodeMap x) ++ "\n"

app2 :: Net2 a -> [[Node2 a]]
app2 (Net2 b) = b



empty_net :: Int -> Int -> Net2 Double
empty_net width depth = Net2 $ replicate depth  $  replicate width  ( Node2 0.0 [] )


net_to_design :: Net -> Design
net_to_design  net = zipWith(\(Node _ ts) x -> (x,ts) ) (concat nodes) (concat ids)
                where
                    nodes = app net
                    n = length nodes
                    m = length (head nodes)
                    ids = map( \x -> zip [0..(m-1)] (repeat x) ) [0..(n-1)]

generate_fully_connected_net :: Int -> Int -> IO Net
generate_fully_connected_net width depth =
                                                do
                                                    let net = empty_net width depth
                                                    weights <- sequ $ map(sequ) $ replicate depth (replicate width (randomRIO(-0.1::Double,0.1::Double)))
                                                    let next_layers = replicate depth  $zip (repeat (0::Int)) [0..(width-1)]
                                                    let cons =  map (replicate (width)) $ zipWith (zip) weights next_layers
                                                    let n_net = Net2 $ zipWith(\c d -> zipWith( \(Node2 a _ ) b -> Node2 a b) c d) (app2 net) cons
                                                    r_net <- add_bias n_net [(a,b)| a <-[1..(depth-1)], b <-[0..(width-1)]  ]
                                                    return $ convert_net2_net r_net

generate_random_net::  Int -> Int -> Int -> Int -> IO Net
generate_random_net width depth nr_neuron nr_con =
                                                        do
                                                            net2 <- generate_random_net2 width depth nr_neuron nr_con
                                                            return $ convert_net2_net net2

generate_random_net2:: Int -> Int -> Int -> Int -> IO (Net2 Double)
generate_random_net2 width depth nr_neuron nr_con =
                                                            do
                                                                let net = empty_net width depth
                                                                nodes <- generate_random_nodes net nr_neuron
                                                                design <- add_neighbours nodes nr_con
                                                                let gen_net = app_design net design
                                                                add_bias gen_net nodes

add_bias :: Net2 Double -> [(Int,Int)] -> IO (Net2 Double)
add_bias net nodes  = do
                                        let ns = app2 net
                                        let l = length ns
                                        let filtered =filter(\(a,b)-> (a /= 0) && (a /= l-1)) nodes
                                        ws <- randomList (length filtered)
                                        let cons = zipWith(\w (a,b) -> (w,(a-1,b))) ws filtered
                                        let n_h = Node2 1.0 cons
                                        let n_ns = ( head ns ++ [n_h] ):(tail ns)
                                        return $ Net2 n_ns

randomList :: Int -> IO([Double])
randomList 0 = return []
randomList n = do
                    r  <- randomRIO (-0.1::Double,0.1::Double)
                    rs <- randomList (n-1)
                    return (r:rs)

generate_nodes :: Net2 a -> [(Int, Int)] -> [(Int,Int)]
generate_nodes net pos =   f_pos ++ pos ++ l_pos
                                        where
                                            m = length $ head $ app2 net
                                            n = length$ app2 net
                                            f_pos =  zip (replicate m 0) (take m [0..])
                                            l_pos =  zip (replicate m (n-1)) (take m [0..])

generate_random_nodes :: Net2 a -> Int -> IO [(Int,Int)]
generate_random_nodes net nr = do
                                                      let
                                                         m = length $ head $ app2 net
                                                         n =  length $ app2 net
                                                         list = concatMap (\n' ->  zip (replicate m n' ) [0..]) [1..n-2]
                                                         in do
                                                             (xs,_) <- draw_n_pos nr ([] , list)
                                                             return $ generate_nodes net xs

draw_n_pos :: Int -> ([(Int,Int)],[(Int,Int)]) -> IO ([(Int,Int)],[(Int,Int)])
draw_n_pos 0 found  = return found
draw_n_pos n  xs       = do
                                         pos <- draw_pos $ snd xs
                                         draw_n_pos (n-1) ((fst pos):(fst xs), snd pos )

draw_pos :: [(Int,Int)] -> IO ((Int,Int),[(Int,Int)])
draw_pos list = do
                           n <- randomRIO ( 0,(length list-1))
                           let (xs,v:ys) = splitAt n list in return (list !! n  , xs++ys)

gen_nr_con ::Int -> Int -> [Int]
gen_nr_con nr nr_nodes = (replicate (nr-1) con_n) ++ (l_con_n:[])
                                        where
                                            con_n = quot nr  nr_nodes
                                            l_con_n = nr - con_n*nr_nodes

distance ::  (Int,Int) -> (Int,Int) -> Float
distance  (a,b) (c,d) = sqrt  $ fromIntegral $ (a-c)^2 + (b -d)^2

f_nodes :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
f_nodes (a,b) nodes = filter(\(c,d) -> c > a ) nodes

b_nodes :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
b_nodes (a,b) nodes = filter(\(c,d) -> c < a ) nodes

taken :: [((Int,Int),[(Double,(Int,Int))])] -> (Int,Int) -> [(Int,Int)]
taken (x:xs) (a,b) = if fst x == (a,b) then map(\(d,(c,e))-> (a+c+1,e)) (snd x) else taken xs (a,b)


gen_empty_design :: [(Int,Int)] -> [((Int,Int),[(Double,(Int,Int))])]
gen_empty_design xs = map(\(a,b) -> ((a,b),[])) xs

add_neighbours:: [(Int,Int)] -> Int -> IO [((Int,Int),[(Double,(Int,Int))])]
add_neighbours nodes nr_con = do
                                                    let e_d = gen_empty_design nodes
                                                    d <- draw_b_neighbours nodes nodes e_d
                                                    draw_f_neighbours nodes nodes d nr_con

draw_f_neighbours :: [(Int,Int)] -> [(Int,Int)] -> [((Int,Int),[(Double,(Int,Int))])] -> Int -> IO [((Int,Int),[(Double,(Int,Int))])]
draw_f_neighbours [] nodes design n_cons = return design
draw_f_neighbours (n:nodes) a_nodes design n_cons = do
                                                                let f_n = f_nodes n a_nodes
                                                                let t = taken design n
                                                                let rest = filter (\z -> notElem z t) f_n
                                                                x <- draw_many rest n (n_cons)
                                                                let n_design = foldr (\z y -> to_design y z) design x
                                                                draw_f_neighbours nodes a_nodes n_design n_cons

draw_many :: [(Int,Int)] -> (Int,Int) -> Int -> IO [([(Int,Int)],(Int,Int), [(Double, (Int,Int))])]
draw_many [] n b = return []
draw_many nodes n 1 = return []
draw_many ns@(no:nodes) n n_con = do
                                                                x <- draw_f_neighbour ns n
                                                                let l = (\(a,b,c) -> a) x
                                                                xs <- draw_many l n (n_con-1)
                                                                return (x : xs)


draw_f_neighbour :: [(Int,Int)] -> (Int,Int) -> IO ([(Int,Int)],(Int,Int), [(Double, (Int,Int))])
draw_f_neighbour nodes n@(b,a) = let
                                                ds = map(\x -> distance n x) nodes
                                                is = to_interval 0 ds
                                                in do
                                                        f <- randomRIO(0.0, last is)
                                                        w <- randomRIO(-0.1::Double,0.1::Double)
                                                        let idx = get_index f is 0
                                                        let m = maximum $ map(\(d,_) -> d) nodes
                                                        if m == b then
                                                            return (nodes,(b,a),[]) else
                                                            return $ gen_f_entry n nodes idx w

gen_f_entry:: (Int,Int) ->[(Int,Int)] -> Int -> Double -> ([(Int,Int)],(Int,Int), [(Double, (Int,Int))])
gen_f_entry (a,b) nodes idx w =  (n_nodes,(a,b), [(w, (c-a-1,d))])
                                              where
                                                    (xs,(c,d):ys) = splitAt idx nodes
                                                    n_nodes = xs ++ ys

draw_b_neighbours :: [(Int,Int)] -> [(Int,Int)] -> [((Int,Int),[(Double,(Int,Int))])] -> IO [((Int,Int),[(Double,(Int,Int))])]
draw_b_neighbours [] nodes design = return design
draw_b_neighbours (n:nodes) a_nodes design = do
                                                                  x <- draw_b_neighbour (b_nodes n a_nodes) n
                                                                  let n_design = to_design design x
                                                                  draw_b_neighbours nodes a_nodes n_design

to_design :: [((Int,Int),[(Double,(Int,Int))])] -> ([(Int,Int)],(Int,Int), [(Double, (Int,Int))]) -> [((Int,Int),[(Double,(Int,Int))])]
to_design [] _ = []
to_design (x@(a,b):xs) e@(_,c,d) = if a == c then (a,b++d):n_xs else x:n_xs
                                                where
                                                    n_xs = to_design xs e

draw_b_neighbour :: [(Int,Int)] -> (Int,Int) -> IO ([(Int,Int)],(Int,Int), [(Double, (Int,Int))])
draw_b_neighbour nodes (0,a) = return (nodes,(0,a),[])
draw_b_neighbour nodes n = let
                                                    ds = map(\x -> distance n x) nodes
                                                    is = to_interval 0 ds
                                                    in do
                                                        f <- randomRIO(0.0, last is)
                                                        w <- randomRIO(0.0::Double,0.1::Double)
                                                        let idx = get_index f is 0
                                                        return $ gen_entry n nodes idx w


gen_entry:: (Int,Int) ->[(Int,Int)] -> Int -> Double -> ([(Int,Int)],(Int,Int), [(Double, (Int,Int))])
gen_entry (a,b) nodes idx w =  (n_nodes,(c,d), [(w, (a-c-1,b))])
                                        where
                                            (xs,(c,d):ys) = splitAt idx nodes
                                            n_nodes = xs ++ ys

get_index :: Float -> [Float] -> Int -> Int
get_index f (d:dists) idx = if f <= d then idx else get_index f dists (idx+1)

to_interval:: Float -> [Float] -> [Float]
to_interval _ [] = []
to_interval v (d:dists) = r : (to_interval r dists)
                                where  r = v+1/ d

app_design ::  Net2  a -> [((Int,Int),[(Double,(Int,Int))])] -> Net2 a
app_design net2 [] = net2
app_design net2  (((n,m),cons):cs) = app_design n_net cs
                            where
                                (fl,l:ll) = splitAt n (app2 net2)
                                (xs,(Node2 a _):ys) = splitAt m l
                                n_layer = xs ++ [Node2 a cons] ++ ys
                                n_net = Net2 $ fl ++ [n_layer] ++ ll

instance Functor Net2 where
            -- fmap (a -> b)  -> Net a -> Net b
            fmap g net =Net2 $  map  (map (\(Node2 a t) -> Node2 (g a) t)) (app2 net)

instance Applicative Net2 where
  -- pure :: a -> Net2 a
      pure a =Net2  [[Node2 a []]]
  -- (<*>) :: (Net2 (a -> b) -> Net2 a -> Net2 b)
      gs <*> net =  Net2 ( concatMap (\fs ->  map (\xs ->  [Node2 (f x) ts | (Node2 f  _) <- fs,  (Node2 x ts) <- xs ] ) net') gs' )
                            where
                                gs' = app2 gs
                                net' = app2 net

test = pure (\x -> (x,5)) <*> simple_net2

nappend :: Net2 a -> Net2 a -> Net2 a
nappend net1 net2 = Net2 $ (app2 net1) ++ (app2 net2)

nhead :: Net2 a -> Net2 a
nhead net = Net2 $ [head (app2 net)]

nlast :: Net2 a -> Net2 a
nlast net = Net2 $ [last (app2 net)]

ntail :: Net2 a -> Net2 a
ntail  net = Net2 $ tail (app2 net)

to_net :: [a] -> Net2 a
to_net  xs = Net2 $ [ map(\x -> Node2 x [] ) xs]

set_input2 :: Net2 a -> [a] -> Net2 a
set_input2  net input = nappend n_h t
                    where
                        h = nhead net
                        t = ntail net
                        n_h = pure(\x y -> x) <*> (to_net input) <*> h

simple_net :: Net
simple_net = Net [
                                [ Node (10,0) [(0.1 , (0,0)), (0.1 , (1,0))] , Node (5,0) [(0.1,(0,1)),(0.1,(1,1))] ],
                                [ Node (0,0) [(0.1 , (0,0))]   , Node (0,0) [(0.1,(0,1))]  ],
                                [ Node (0,0) []   , Node (0,0) []  ]
                             ]

simple_net2 :: Net2 Double
simple_net2 = Net2 [
                                 [ Node2 0 [(0.1 , (0,0)), (0.1 , (1,0))] , Node2 0 [(0.1,(0,1)),(0.1,(1,1))] ],
                                 [ Node2 0 [(0.1 , (0,0))]   , Node2 0 [(0.1,(0,1))]  ],
                                 [ Node2 0 [(0.1 , (0,0))]   , Node2 0 [(0.1,(0,0))]  ]
                               ]

convert_net2_net :: Net2 Double -> Net
convert_net2_net net2 = Net $ map (map( \(Node2 a xs) -> Node (a,0) xs))  $app2 net2

showNet :: (Show a)  =>  Net2 a -> IO ()
showNet net = putStrLn $ "Network\n" ++  ( concat ([ "[" ++ showNodes xs ++"] \n" | xs <- (app2 net)]) ) ++ "\n"
                            where showNodes xs = concat [ " N " ++ show x ++ " " ++ show a | Node2 x a <- xs]

set_input :: Net -> [Double] -> Net
set_input (Net (layer : layers)) input = Net $  n_layer : layers
                                                where n_layer =  zipWith(\a (Node _ b) -> Node (a,0) b) (input++[1.0]) layer

reset :: Net -> Net
reset (Net layers) = Net $ map (map (\(Node _ b) -> Node (0,0) b)) layers

reset' :: Net -> Net
reset' (Net layers) = Net $ map (map (\(Node (a,_) b) -> Node (a,0) b)) layers

relu :: Node -> Node
relu (Node (x,_) ts) = if x >  0 then Node (x,x) ts else Node (x,0) ts

mult_relu' :: Node -> Node
mult_relu' (Node (a,z) ts) = if (a > 0) then Node (a,a*z) ts else Node (a,0) ts

f_propagate :: Net  -> Net
f_propagate (Net ([]))           = Net ([])
f_propagate (Net ([a]))      = Net ([map relu a])
f_propagate (Net (input:layers)) = Net $ n_input :app (f_propagate ( Net n_layers))
                                                      where
                                                           n_input = map relu input
                                                           n_layers = f_propagate_nodes n_input layers


f_propagate_nodes :: [Node] -> [[Node]] -> [[Node]]
f_propagate_nodes [] layers = layers
f_propagate_nodes (x:xs) layers = f_propagate_nodes xs n_layers
                                                    where
                                                       n_layers = f_propagate_node x layers


f_propagate_node :: Node ->  [[Node]] -> [[Node]]
f_propagate_node ( Node _ []) layers = layers
f_propagate_node ( Node (a,z) ( (w,(l,n)) : targets) ) layers = f_propagate_node (Node (a,z) targets)  (xs ++ (as ++ (Node (s+w*z,0) ts) : bs) :ys  )
                                                                      where
                                                                        (xs, t:ys) = splitAt l layers
                                                                        (as, (Node (s,_) ts):bs) = splitAt n t

app :: Net -> [[Node]]
app (Net values) = values


set_last_deltas :: [Double] -> Net -> Net
set_last_deltas targets net = Net $ layers ++ [n_layer]
                                        where
                                          layers =  init $ app net
                                          n_layer = zipWith(\t (Node (a,_) ts) ->  Node(a,a-t) ts) targets $ last $ app net


b_propagate :: Net -> Net
b_propagate (Net [])        = Net []
b_propagate (Net ( a:l:[])) = Net $ (b_propagate_nodes a [l]):l:[]
b_propagate (Net (x:layers)) = Net  $ (b_propagate_nodes x layers) : (app (b_propagate (Net layers)) )


b_propagate_nodes :: [Node] -> [[Node]] -> [Node]
b_propagate_nodes [] _ = []
b_propagate_nodes (x:nodes) layers = n_node :( b_propagate_nodes nodes layers)
                                                where n_node = b_propagate_node x layers

b_propagate_node :: Node -> [[Node]] -> Node
b_propagate_node (Node x []) layers = mult_relu' (Node x [])
b_propagate_node (Node (a,v) ((w,(l,n)):ts)) layers = b_propagate_node (Node (a, v + w*d ) ts) layers
                                    where
                                      (_,t:_) = splitAt l layers
                                      (ks,(Node (_,d) _):ls) = splitAt n t


train :: Net -> [Double] -> [Double] ->  Double ->Net
train net input targets s = update_weights s c_net
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net = f_propagate i_net
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net


apply :: (Node -> Node -> Node) -> Net -> Net -> Net
apply f net1 net2 = Net $ zipWith( zipWith( f)) layers1 layers2
                                       where
                                           layers1 = app net1
                                           layers2 = app net2

update_weights :: Double -> Net  -> Net
update_weights s (Net [a]) = Net  [a]
update_weights s (Net (x:xs)) = Net $ (update_weights_nodes x s xs) : (app $ update_weights s (Net xs))

update_weights_nodes :: [Node] -> Double -> [[Node]] -> [Node]
update_weights_nodes   xs s layers = map (\x -> update_weights_node x s layers)  xs

update_weights_node :: Node -> Double -> [[Node]] -> Node
update_weights_node (Node (z',d) ts) s layers = Node (z',d) n_ts
                                                                where
                                                                    zs = map (\(_, (l,n)) -> find_z layers (l,n)) ts
                                                                    n_ts = zipWith(\(w, t) z -> (w- (s*z*d), t)) ts zs


find_z :: [[Node]] -> (Int,Int) -> Double
find_z layers (l,n)  = z
                            where
                                (xs,t:ys) = splitAt l layers
                                (ks,(Node (z,_) _):ls) = splitAt n t


train_data :: Net -> [[Double]] -> [[Double]] ->  Net
train_data net input targets = foldr (\(i,t) net' -> train net' i t 0.0001) net i_t
                                          where i_t = zip input targets

output :: Net -> [Double] -> [Double]
output net input = map(\(Node (a,_) _) -> a) layer
                    where
                         r_net = reset net
                         f_net = f_propagate $set_input r_net input
                         layer = last $ app f_net

find_best_fully_connected_net :: Int -> Int -> Int -> Int -> ([[Double]],[[Double]]) -> IO Net
find_best_fully_connected_net nr_nets nr_steps width depth sample =
            do
                xs <- generate_fully_connected_net width depth
                x <- sequ $replicate (nr_nets-1) $ generate_fully_connected_net width depth
                let b_net = foldr (\x y -> compete x y nr_steps sample) xs x
                return b_net

find_best_random_net :: Int -> Int -> Int -> Int -> Int -> Int -> ([[Double]],[[Double]]) -> IO Net
find_best_random_net nr_nets nr_steps width depth nr_neuron nr_con sample =
     do
         xs <- generate_random_net width depth nr_neuron nr_con
         x <- sequ $replicate (nr_nets-1) $ generate_random_net width depth nr_neuron nr_con
         let b_net = foldr (\x y -> compete x y nr_steps sample) xs x
         return b_net

sequ :: [IO a] -> IO [a]
sequ [] = return []
sequ (x:xs) = do
                     n_x <-x
                     n_xs <- sequ xs
                     return (n_x:n_xs)

compete :: Net -> Net -> Int -> ([[Double]],[[Double]]) -> Net
compete net1 net2 nr_steps sample = if (error1 < error2) then n_net1 else  n_net2
                                                    where
                                                        inp = take nr_steps (fst sample)
                                                        out = take nr_steps (snd sample)
                                                        n_net1 = train_data net1 inp out
                                                        n_net2 = train_data net2 inp out
                                                        predi1 = map (output n_net1) inp
                                                        predi2 = map (output n_net2) inp
                                                        err1 =  sum $ zipWith (\a b -> sum  $(zipWith( \x y -> (x-y)^2)) a b) predi1 out
                                                        err2 =  sum $ zipWith (\a b -> sum  $(zipWith( \x y -> (x-y)^2)) a b) predi2 out
                                                        infinity = (read "Infinity")::Double
                                                        error1 = if isNaN err1 then infinity else err1
                                                        error2 = if isNaN err2 then infinity else err2

test2::IO()
test2 = do
    net <- generate_random_net 5 5 3 4
    print net
    putStrLn $ check_entries net
    print $ net_to_design $ net

main::IO()
main = do
          (inp,outs) <- dat
          let inputs = cycle inp
          let outputs = cycle outs
          bf_net <- find_best_fully_connected_net 500 500 4 30  (take 10000 inputs, take 10000 outputs)
          putStrLn "Fully Connected Network"
          putStrLn "Prediction:"
          print $ let f_net = train_data bf_net (take 200000 inputs) (take 200000 outputs)
                  in output f_net (inp !! 0)
          putStrLn "Expected Output:"
          print  $ outs !! 0
          -- generate_random_net witdh depth nr_neuron nr_con
          --find_best_random_net nr_nets nr_steps width depth nr_neuron nr_con sample
          putStrLn "Random Generated Network"
          putStrLn "Prediction:"
          the_net <- find_best_random_net 500 500 4 180 120 8  (take 10000 inputs, take 10000 outputs)
          print $ let b_net = train_data the_net (take 200000 inputs) (take 200000 outputs)
             in output b_net (inp !! 0)
          putStrLn "Expected Output:"
          print  $ outs !! 0
