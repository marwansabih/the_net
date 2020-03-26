import           System.Random

newRand = randomIO :: IO Int

data Node = Node (Double,Double) [(Double, (Int,Int))] deriving Show
data Net  = Net [[Node]] deriving Show

data Node2 a = Node2 a [(Double, (Int,Int))]
data Net2 a = Net2 [[Node2 a]]

app2 :: Net2 a -> [[Node2 a]]
app2 (Net2 b) = b

empty_net :: Int -> Int -> Net2 Double
empty_net width depth = Net2 $ replicate depth  $  replicate width  ( Node2 0.0 [] )

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
                                                                return $ app_design net design

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
                                                         list = concatMap (\n' ->  zip (replicate m n' ) [0..]) [1..n-1]
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

gen_design :: [(Int,Int)] -> Int -> IO [((Int,Int),[(Double,(Int,Int))])]
gen_design  nodes nr_con = undefined
                                        where ns = gen_nr_con nr_con $ length nodes

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
                                                                let rest = filter (\z ->not (elem z t)) f_n
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
                                                        w <- randomRIO(0.0::Double,0.1::Double)
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
                                                where n_layer =  zipWith(\a (Node _ b) -> Node (a,0) b) input layer

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
train_data net input targets = foldr (\(i,t) net' -> train net' i t 0.5) net i_t
                                          where i_t = zip input targets

output :: Net -> [Double] -> [Double]
output net input = map(\(Node (a,_) _) -> a) layer
                    where
                         r_net = reset net
                         f_net = f_propagate $set_input r_net input
                         layer = last $ app f_net
main::IO()
main = do
          print $ let net = train_data simple_net (replicate 10000  [0.3,0.5]) (replicate 10000  [0.9, 0.275])
                  in output net [0.3,0.5]
          -- generate_random_net witdh depth nr_neuron nr_con
          the_net <- generate_random_net 2 4 4 2
          print $ let net = train_data the_net (replicate 10000  [0.3,0.5]) (replicate 10000  [0.9, 0.275])
                  in output net [0.3,0.5]
