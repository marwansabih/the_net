import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Time
import           System.Random

--ghc -O2 -o main.o the_net.hs -fprof-auto  -fprof-cafs -fforce-recomp

--using Box-Muller for generation of normal distribution
normal :: IO Double
normal = do
                d1 <- randomRIO(0::Double, 1::Double)
                d2 <- randomRIO(0::Double, 1::Double)
                let z = sqrt(-2.0 * (log d1)) * cos ( 2*pi*d2 )
                return $ 0.1 * z

dat :: IO ([[Double]],[[Double]])
dat =  do
           file <- readFile "nyc-east-river-bicycle-counts.csv"
           let ls = tail $ lines file
           let tokens = map (splitOn ",") ls
           let num_str = map(\x -> snd (splitAt 3 x)) tokens
           let pairs = map (splitAt 7) $ map (map(\a -> read a :: Double)) num_str
           let normalized = map( \(a,b) -> ( norm_inp a, map(1.0/10000*) b)) pairs
           return $ (\x -> (map fst x, map snd x)) normalized

pimp_dat :: Int -> ([[Double]],[[Double]]) -> ([[Double]],[[Double]])
pimp_dat n_len (inp,out) = (pimp inp, pimp out)
                                        where
                                            diff = n_len - ( length $ head inp )
                                            f_l = div diff 2
                                            front = replicate (f_l) (0.0::Double)
                                            back = replicate (diff -f_l) (0.0::Double)
                                            pimp = map(\x -> front ++ x ++ back)

norm_inp :: [Double] -> [ Double]
norm_inp inp = zipWith (\f a -> f a) [(1.0/100*),(1.0/100*), (1.0*), (1.0/10000*),(1.0/10000*),(1.0/10000*),(1.0/10000*)] inp

data Node = Node (Double,Double) [(Double, (Int,Int))] deriving Show
data Net  = Net [[Node]]

data Node2 a = Node2 a [(Double, (Int,Int))]
data Net2 a = Net2 [[Node2 a]]

type Design = [((Int,Int),[(Double,(Int,Int))])]

newtype DT a = D (Design -> (a, Design))

appD :: DT a -> Design -> (a,Design)
appD (D dt) x = dt x

instance Functor DT where
    --fmap :: (a->b) -> DT a -> DT b
    fmap g st = D (\d -> let (x,d') = appD st d in (g x, d'))

instance Applicative DT where
    -- pure :: a -> ST a
    pure x = D (\d -> (x,d))
    -- (<*>) :: DT (a -> b) -> DT a -> DT b
    stf <*> stx = D (\d ->
                                let
                                  (f, d')  = appD stf d
                                  (x,d'') = appD stx d'
                                 in ( f x, d''))

instance Monad DT where
    -- (>>=) :: DT a -> (a -> DT b) -> DT b
    dt >>= f =  D (\d -> let (x,d') = appD dt d in appD (f x) d' )

alter_neurons:: Int -> Int -> Net -> IO (Net,Net)
alter_neurons nr_neuron nr_cons net = alter_neurons' nr_neuron nr_cons (net,net)

alter_neurons' :: Int -> Int -> (Net,Net) -> IO (Net,Net)
alter_neurons' 0 nr_cons nets = return nets
alter_neurons' nr_neuron nr_cons (net1,net2) = do
                                                (n_net1,n_net2) <-  alter_neuron nr_cons (net1,net2)
                                                alter_neurons' (nr_neuron-1) nr_cons(n_net1,n_net2)

alter_neuron :: Int -> (Net,Net) ->IO (Net,Net)
alter_neuron nr_cons (net1,net2) = do
                            let org_design1 = net_to_design net1
                            let org_design2 = net_to_design net2
                            let rm_ns1 = remove_able_nodes org_design1
                            let rm_ns2 = remove_able_nodes org_design2
                            let rm_ns = intersection rm_ns1 rm_ns2
                            let fr_ns1 = free_nodes org_design1
                            let fr_ns2 = free_nodes org_design2
                            let fr_ns = intersection fr_ns1 fr_ns2
                            to_delete <- draw_uniform 1 rm_ns
                            to_create <- draw_uniform (length to_delete) fr_ns
                            reset_design <- do_all to_delete org_design1 reset_weights
                            d_design <- do_all to_delete org_design2 (to_IO remove_neuron)
                            altered_design <- do_all to_create d_design (add_neuron_design nr_cons)
                            let r_net = design_to_net reset_design
                            let a_net = design_to_net altered_design
                            return (r_net, a_net)

intersection :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
intersection xs ys = filter(\x -> elem x xs) ys

design_to_net :: Design -> Net
design_to_net design = Net $ map(map(\(_,ts) -> Node (0.0,0.0) ts )) layers
                            where layers = to_layers 0 design []

to_layers :: Int -> Design -> [Design] -> [Design]
to_layers _ [] result = result
to_layers nr design layers = to_layers (nr+1) n_design n_layers
                                    where
                                        n_design = filter(\((a,_),_)-> a /= nr) design
                                        n_layer = filter(\((a,_),_)-> a == nr) design
                                        o_layer = sort n_layer
                                        n_layers = layers ++ [o_layer]

to_IO :: ((Int,Int) -> Design -> Design) -> ((Int,Int) -> Design -> IO Design)
to_IO f = (\x y -> return (f x y))

do_all :: [(Int,Int)] -> Design -> ((Int,Int) -> Design -> IO Design) -> IO Design
do_all [] design _ = return design
do_all (x:xs) design f = do
                                        n_d <- f x design
                                        do_all xs n_d f

draw_uniform :: Int ->[(Int,Int)] -> IO [(Int,Int)]
draw_uniform nr list = do
                                        (found,left) <- draw_uniform' nr ([],list)
                                        return found

draw_uniform' :: Int -> ([(Int,Int)],[(Int,Int)]) -> IO ([(Int,Int)],[(Int,Int)])
draw_uniform' 0 res = return res
draw_uniform' _ res@(_,[]) = return res
draw_uniform' nr (found,left) = do
                                                      k <-randomRIO(0,(length left)-1)
                                                      let (xs,y:ys) = splitAt k left
                                                      draw_uniform' (nr-1) (y:found,xs++ys)


add_neuron_design :: Int -> (Int,Int) -> Design -> IO Design
add_neuron_design n_cons pos design = do
                                                                    let (bias,b_ns) = full_d_b_nodes pos design
                                                                    let f_ns = full_d_f_nodes pos design
                                                                    bs <- draw_from_list 1 pos ([],b_ns)
                                                                    fs <- draw_from_list (n_cons-1) pos ([],f_ns)
                                                                    f <- connect_b pos $ fst bs
                                                                    g <- connect_f pos $ fst fs
                                                                    h <- connect_f bias [pos]
                                                                    let b_d = snd $ (appD g) design
                                                                    let n_d =  snd $ (appD f) b_d
                                                                    return $ snd $ (appD h) n_d

draw_from_list :: Int -> (Int,Int)-> ([(Int,Int)],[(Int,Int)]) -> IO ([(Int,Int)], [(Int,Int)])
draw_from_list 0 _ xs = return xs
draw_from_list _ _ (a,[]) = return (a,[])
draw_from_list n_cons pos nodes@(x,y) = do
                                                            let ds = map (distance pos) (snd nodes)
                                                            let is = to_interval 0 ds
                                                            f <- randomRIO(0.0, last is)
                                                            let idx = get_index f is 0
                                                            let (xs,y:ys) = splitAt idx (snd nodes)
                                                            draw_from_list (n_cons-1) pos (y:x, xs ++ ys)

connect_f :: (Int,Int) -> [(Int,Int)] -> IO (DT [(Int,Int)])
connect_f pos fs = reset_list pos (pure fs) add_f_connection

connect_b :: (Int,Int) -> [(Int,Int)] -> IO (DT [(Int,Int)])
connect_b pos fs = reset_list pos (pure fs) add_b_connection

full_d_f_nodes :: (Int,Int) -> Design -> [(Int,Int)]
full_d_f_nodes pos design = l_l ++ (filter (\x -> elem x fu_ns) f_ns)
    where
        f_ns = f_nodes pos (map fst design)
        fu_ns = full_nodes design
        m =  maximum $ map fst f_ns
        l_l = filter(\(a,b) -> a == m) f_ns

d_f_nodes :: (Int,Int) -> Design -> [(Int,Int)]
d_f_nodes pos design = f_nodes pos (map fst  design)

full_d_b_nodes :: (Int,Int) -> Design -> ((Int,Int),[(Int,Int)])
full_d_b_nodes pos design = ((0,m),result)
           where
               b_ns = b_nodes pos (map fst design)
               f_ns = full_nodes design
               m =  maximum $ map snd $ filter(\(a,_) -> a == 0) b_ns
               f_l = filter(\x@(a,b) -> x /= (0,m) && a == 0) b_ns
               result = f_l ++ (filter (\x -> elem x f_ns) b_ns)



d_b_nodes :: (Int,Int) -> Design -> ((Int,Int),[(Int,Int)])
d_b_nodes pos design = ((0,m),filter(\x -> x /= (0,m)) b_ns)
           where
               b_ns = b_nodes pos (map fst design)
               m =  maximum $ map snd $ filter(\(a,_) -> a == 0) b_ns

reset_weights :: (Int,Int) -> Design -> IO Design
reset_weights pos design = do
                              let p_to = point_to pos design
                              let p_at = point_at pos design
                              g <- reset_b_weights pos p_to
                              f <- reset_f_weights pos p_at
                              let b_d = snd $ (appD g) design
                              return $ snd $ (appD f) b_d


reset_b_weights :: (Int,Int) -> [(Int,Int)] -> IO (DT [(Int,Int)])
reset_b_weights pos p_to = reset_list pos (pure p_to) reset_b_weight

reset_f_weights :: (Int,Int) -> [(Int,Int)] -> IO (DT [(Int,Int)])
reset_f_weights pos p_at = reset_list pos (pure p_at) reset_f_weight

reset_list :: (Int,Int) -> DT [(Int,Int)] -> ((Int,Int) -> Double -> [(Int,Int)] -> DT [(Int,Int)]) -> IO (DT [(Int,Int)])
reset_list  pos dt f  = do
                                w <- normal
                                let g = f pos w
                                let dt' = dt >>= g
                                if fst (appD dt' []) == []
                                    then return dt'
                                else reset_list pos dt' f

reset_f_weight :: (Int,Int) -> Double -> [(Int,Int)] -> DT [(Int,Int)]
reset_f_weight _ _ [] = D (\d -> ([],d))
reset_f_weight (a,b) w ((x1,x2):xs) = D (\d -> (xs, add_to d ))
                                where
                                    replace [] = []
                                    replace ((w',(k,l)):is) = if (x1-a-1,x2) == (k,l)
                                                                          then (w,(k,l)) : replace is
                                                                          else (w',(k,l)) : replace is
                                    add_to [] = []
                                    add_to (((c,d),ts):ds) = if (a,b) == (c,d)
                                                                         then ((c,d), replace ts) : add_to ds
                                                                         else ((c,d), ts) : add_to ds

reset_b_weight :: (Int,Int) -> Double -> [(Int,Int)] -> DT [(Int,Int)]
reset_b_weight _ _ [] = D (\d -> ([],d))
reset_b_weight (a,b) w ((x1,x2):xs) = D (\d -> (xs, add_to d ))
                                where
                                    replace [] = []
                                    replace ((w',(k,l)):is) = if (a-x1-1,b) == (k,l)
                                                                          then (w,(k,l)) : replace is
                                                                          else (w',(k,l)) : replace is
                                    add_to [] = []
                                    add_to (((c,d),ts):ds) = if (x1,x2) == (c,d)
                                                                         then ((c,d), replace ts) : add_to ds
                                                                         else ((c,d), ts) : add_to ds

add_b_connection :: (Int,Int) -> Double -> [(Int,Int)] -> DT [(Int,Int)]
add_b_connection _ _ [] = D (\d -> ([], d))
add_b_connection pos@(a,b) w ((x1,x2):xs) = D (\d -> (xs, add_to d ))
                                where
                                    add_to [] = []
                                    add_to (((c,d),ts):ds) = if (x1,x2) == (c,d)
                                                                         then ((c,d), ((w,(a-c-1,b)):ts)) : (add_to ds)
                                                                         else ((c,d),ts) : (add_to ds)

add_f_connection :: (Int,Int) -> Double -> [(Int,Int)] ->  DT [(Int,Int)]
add_f_connection (a,b) w [] = D (\d -> ([], d ))
add_f_connection pos@(a,b) w ((x1,x2):xs) = D (\d -> (xs, add_to d ))
                                    where
                                        add_to [] = []
                                        add_to (((c,d),ts):ds) = if (c,d) == (a,b)
                                                                           then (pos, (w,(x1-a-1,x2)):ts) : ( add_to ds )
                                                                           else ((c,d),ts) : ( add_to ds )



remove_neuron:: (Int,Int) -> Design -> Design
remove_neuron pos dsgn = map(\(a,b) -> if(a == pos) then (a,[]) else (a,b)) wp_dsgn
              where
                  p_to = point_to pos dsgn
                  funcs = map (\x -> remove_entry_at x pos) p_to
                  wp_dsgn = foldr (\f x -> f x) dsgn funcs

remove_entry_at :: (Int,Int) -> (Int,Int) -> Design -> Design
remove_entry_at key entry [] = []
remove_entry_at (c,d) (e,f) ((a,ts):xs) = if a == (c,d) then (a, filter is_not_entry ts ): n_xs else (a,ts) : n_xs
                                        where
                                            is_not_entry (_,(y,z)) = (y,z) /= (e-c-1,f)
                                            n_xs = remove_entry_at (c,d) (e,f) xs

remove_able_nodes :: Design -> [(Int,Int)]
remove_able_nodes dsgn = filter (\x-> is_remove_able x dsgn) $ full_nodes dsgn

-- functions needs to take into account that the bias points to everything except the last layer
-- but the bias should not be counted as "real" connection
--the idea is after the removel of a node every node needs to have
--an output-connection and an input connection.
is_remove_able :: (Int,Int) -> Design -> Bool
is_remove_able pos dsgn = and $ map (\x -> length x > 1) $ p_tos ++ p_ats
                            where
                                    bias_pos = last $ map (\(x,_) -> x) $ filter (\((a,_),_) -> a == 0 ) dsgn
                                    p_to = point_to pos dsgn
                                    p_at = point_at pos dsgn
                                    p_tos = map (\x -> point_at x dsgn) p_to
                                    p_ats = map(\z -> filter(\y -> y /= bias_pos) z) $ map(\x -> point_to x dsgn)  p_at


point_to ::  (Int,Int) -> Design -> [(Int,Int)]
point_to (c,d) = map(fst) . filter (\((a,_),ts) -> elem (c-a-1,d)  (map(snd) ts) )

point_at :: (Int,Int) -> Design -> [(Int,Int)]
point_at (a,b) = (\(_,ts) -> map(\(w,(c,d)) -> (c+a+1,d) ) ts) . head . filter(\x -> (a,b) == (fst x))


filter_nodes :: ( ((Int,Int),[(Double,(Int,Int))]) -> Bool )  ->Design -> [(Int,Int)]
filter_nodes f dsgn  = map(fst) $ filter f  n_dsgn
                where
                    m = maximum $ map(\((a,b),_) -> a) dsgn
                    n_dsgn = filter(\((a,b),_) -> a /= m && a /= 0) dsgn



free_nodes :: Design -> [(Int,Int)]
free_nodes = filter_nodes (null . snd)

full_nodes :: Design -> [(Int,Int)]
full_nodes = filter_nodes (not . null . snd)

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
                    fst_layer = zip (repeat 0) [0..m-1]
                    ids = fst_layer : ( map( \x -> zip (repeat x) [0..(m-2)] ) [1..(n-1)] )

generate_fully_connected_net :: Int -> Int -> IO Net
generate_fully_connected_net width depth =
                                                do
                                                    let net = empty_net width depth
                                                    weights <- sequ $ map(sequ) $ replicate depth (replicate width (normal))
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
                                                                net <- add_bias gen_net nodes
                                                                return net

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
                    r  <- normal
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
                                                      let m = length $ head $ app2 net
                                                      let  n =  length $ app2 net
                                                      let  list = concatMap (\n' ->  zip (replicate m n' ) [0..]) [1..n-2]
                                                      (xs,_) <- draw_n_pos nr ([] , list)
                                                      return $ generate_nodes net xs

draw_n_pos :: Int -> ([(Int,Int)],[(Int,Int)]) -> IO ([(Int,Int)],[(Int,Int)])
draw_n_pos 0 found  = return found
draw_n_pos n  xs       = do
                                         pos <- draw_pos $ snd xs
                                         draw_n_pos (n-1) ((fst pos):(fst xs), snd pos )

draw_pos :: [(Int,Int)] -> IO ((Int,Int),[(Int,Int)])
draw_pos [] = do
                        return ((0,0),[])
draw_pos list = do
                           let len = length list-1
                           n <- randomRIO ( 0,len)
                           let (xs,v:ys) = splitAt n list in return (list !! n  ,xs++ys)

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
                                                        w <- normal
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
                                                        w <- normal
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

training_batch :: Net -> [[Double]] -> [[Double]] -> Double -> Net
training_batch net input targets s =  foldr (\(i,t) net -> add_w net (get_gradient_net i t s) ) net set
                                                   where
                                                       set = zip input targets
                                                       add_up = zipWith(\(w1, x) (w2, _) -> (w1+w2,x))
                                                       add_w = apply(\(Node (a,z) ts) (Node (a',d) ts') -> Node (a,z) (add_up ts ts') )
                                                       get_gradient_net = get_gradient net

get_gradient :: Net -> [Double] -> [Double] ->  Double ->Net
get_gradient net input targets s = get_gradient_f s c_net
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net = f_propagate i_net
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net

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

get_gradient_f :: Double -> Net  -> Net
get_gradient_f s (Net [a]) = Net  [a]
get_gradient_f s (Net (x:xs)) = Net $ (get_gradient_nodes x s xs) : (app $ get_gradient_f s (Net xs))

get_gradient_nodes :: [Node] -> Double -> [[Node]] -> [Node]
get_gradient_nodes   xs s layers = map (\x -> get_gradient_node x s layers)  xs

get_gradient_node :: Node -> Double -> [[Node]] -> Node
get_gradient_node (Node (z',d) ts) s layers = Node (z',d) n_ts
                                                                where
                                                                    zs = map (\(_, (l,n)) -> find_z layers (l,n)) ts
                                                                    n_ts = zipWith(\(w, t) z -> (- (s*z*d), t)) ts zs


find_z :: [[Node]] -> (Int,Int) -> Double
find_z layers (l,n)  = z
                            where
                                (xs,t:ys) = splitAt l layers
                                (ks,(Node (z,_) _):ls) = splitAt n t

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

change_out_net :: Int -> Net -> IO Net
change_out_net nr_nodes net = do
                                                 let design = net_to_design net
                                                 n_des <- change_out_design nr_nodes design
                                                 return $ design_to_net n_des


change_out_design :: Int -> Design -> IO Design
change_out_design nr_nodes design = do
                                             let  last_l = last_layer_design design
                                             let (m,_) = head last_l
                                             let  (keep,go) = splitAt nr_nodes last_l
                                             let  p_tos = to_set $ concatMap (\x -> point_to x design) go
                                             connect <- (reconnect_list (pure p_tos) (reconnect (keep,go) ) )
                                             let (_,n_des) = (appD connect) design
                                             return $ filter (\((a,b),_) -> a /= m || b < nr_nodes) n_des

to_set:: Eq a => [a] -> [a]
to_set []     = []
to_set (x:xs) = x: to_set (filter (\y -> y /= x) xs )

reconnect_list :: DT [(Int,Int)] -> (IO ([(Int,Int)]-> DT [(Int,Int)]))-> IO (DT [(Int,Int)])
reconnect_list  dt f  = do
                                g <- f
                                let dt' = dt >>= g
                                if fst (appD dt' []) == []
                                    then return dt'
                                else reconnect_list dt' f

reconnect :: ([(Int,Int)],[(Int,Int)]) ->  IO ([(Int,Int)]-> DT [(Int,Int)])
reconnect (keep,go) = do
                        idx <- randomRIO(0, (length keep) -1)
                        let n = fst $ head keep
                        let f p_to ds = case ds of
                                [] -> []
                                ((p@(p1,p2),ts):xs) -> if p == p_to then ((p, new_ts) : (f p_to xs)) else (p, ts) : (f p_to xs)
                                    where
                                         (k1,k2) = keep !! idx
                                         n_ts = map(\(w, q@(q1,q2)) -> if elem (p1+q1+1,q2) go then (w, (q1,k2) ) else (w,q) ) ts
                                         new_ts = set_by (\(_,e1) (_,e2) ->  e1 == e2) n_ts
                        return $(\(p_to:p_tos) ->(D (\d -> (p_tos, f p_to d))))


set_by :: (a-> a -> Bool)->[a] -> [a]
set_by _ []     = []
set_by f (x:xs) = x: set_by f  (filter (not . (f x)) xs)

last_layer_design :: Design -> [(Int,Int)]
last_layer_design design = map (\(p,_)-> p) $ filter (\((a,_),_) -> a == m) design
    where m = maximum $ map( \((a,b),_) -> a) design

sequ :: [IO a] -> IO [a]
sequ [] = return []
sequ (x:xs) = do
                     n_x <-x
                     n_xs <- sequ xs
                     return (n_x:n_xs)


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

get_random_batch :: Int -> ([[Double]],[[Double]])  -> IO ([[Double]],[[Double]])
get_random_batch n set = get_random_batch' n set ([],[])

get_random_batch' :: Int -> ([[Double]],[[Double]]) -> ([[Double]],[[Double]])  -> IO ([[Double]],[[Double]])
get_random_batch' 0 (input, output) (f_input,f_output) = return (f_input,f_output)
get_random_batch' n (input, output) (f_input,f_output) = do
                                                    idx <- randomRIO(0, length (input)-1)
                                                    let (fis, i:is)   = splitAt idx input
                                                    let (fos, o:os) = splitAt idx output
                                                    let n_is = is ++ fis
                                                    let n_os = fos ++ os
                                                    get_random_batch' (n-1) (n_is,n_os) (i:f_input, o:f_output)



test2::IO()
test2 = do
    net <- generate_random_net 5 5 3 4
    print net
    putStrLn $ check_entries net
    print $ net_to_design $ net

test3::IO()
test3 = do
    net <- generate_random_net 5 5 3 4
    print $ net
    let design = net_to_design $ net
    print $ free_nodes design
    print $ full_nodes design
    putStrLn "Point_to (4,3):"
    print$ point_to (4,3) design
    putStrLn "Point_at (0,1):"
    print$ point_at (0,1) design
    putStrLn "Point_at (4,1):"
    print $ point_at (4,1) design

test4:: IO()
test4 = do
                net <- generate_random_net 5 5 3 2
                print net
                let design = net_to_design net
                let the_node = head $ full_nodes design
                print the_node
                print $ is_remove_able the_node design
                print "Pointing to"
                print $ point_to the_node design
                print "Pointing to at"
                let p_t = point_to the_node design
                print $ map(\x -> point_at x design) p_t
                print "Pointing at"
                print $ point_at the_node design
                print "Pointing at to"
                let p_at = point_at the_node design
                print $ map(\x -> point_to x design) p_at
                let bias_pos = last $ map (\(x,_) -> x) $ filter (\((a,_),_) -> a == 0 ) design
                print "Bias"
                print $ bias_pos
                print "All removeable nodes:"
                print $ remove_able_nodes design

test5:: IO()
test5 = do
        let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
        print $ design
        print $ remove_entry_at (0,2) (1,1) design
        print $ remove_neuron (1,1) design
        print $ remove_neuron (1,2) design

--testing the monad :)
test6 = do
    let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.2,(0,4)),(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
    let f = appD $ pure [(2,3),(4,5)] >>= add_f_connection (1,1) 0.5 >>= add_f_connection (1,1) 0.8
    print $ f design
    let g = appD $ pure [(0,1),(0,2)] >>= add_b_connection (1,1) 0.3 >>= add_b_connection (1,1) 0.7
    print $ g design
    let h = appD $ pure [(0,1),(0,2),(0,2)] >>= reset_b_weight (1,1) 11.0 >>= reset_b_weight (1,1) 17.0 >>= reset_b_weight (1,2) 18
    print $ h design
    let i = appD $ pure [(1,1),(1,1),(1,1)] >>= reset_f_weight (0,1) 11.0 >>= reset_f_weight (0,2) 17.0
    print $ i design
--testing more of the monad
test7 = do
    let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.2,(0,4)),(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
    g <- reset_f_weights (0,1) [(1,4),(1,1)]
    print $ (appD g) design
    h <- reset_b_weights (1,1) [(0,1),(0,2)]
    print $ (appD h) design
    n_des <- reset_weights (1,1) design
    print n_des

test8 = do
     drawn <- draw_from_list 3 (0,0) ([],[(9,0),(100,100),(3,3),(5,5),(1,1),(1,3)])
     print drawn
     let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.2,(0,4)),(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
     print $  d_b_nodes (1,1) design

test9 = do
    let design = [((0,0),[]),((0,1),[]),((0,2),[]),((0,3),[]),((0,4),[]),
                         ((1,0),[]), ((1,1),[]),((1,2),[]),((1,3),[]),((1,4),[]),
                         ((2,0),[]),((2,1),[]),((2,2),[]),((2,3),[]),((2,4),[]),
                         ((3,0),[]),((3,1),[]),((3,2),[]),((3,3),[]),((3,4),[])]
    d <- add_neuron_design 3 (1,1) design
    print d
    print "Testing to layers"
    let layers = to_layers 0 design []
    print layers
    print "Testing desing to net"
    print $ design_to_net d
    print "Testing altering neurons!"
    print "Will not be altered since no removeable neurons"
    (net1,net2) <- alter_neurons 1 3 (design_to_net d)
    print net1
    print net2
    let design2 = [((0,0),[(0.1,(0,0)),(0.2,(0,1))]),((0,1),[]),((0,2),[]),((0,3),[(0.1,(0,0)),(0.1,(0,1)),(0.1,(0,2))]),((0,4),[(0.1,(0,0)),(0.2,(0,1))]),
                         ((1,0),[(0.1,(1,0))]), ((1,1),[(0.1,(1,0))]),((1,2),[(0.1,(1,0))]),((1,3),[]),((1,4),[]),
                         ((2,0),[]),((2,1),[]),((2,2),[]),((2,3),[]),((2,4),[]),
                         ((3,0),[]),((3,1),[]),((3,2),[]),((3,3),[]),((3,4),[])]
    (net1',net2') <- alter_neurons 1 3 (design_to_net design2)
    print "should be altered"
    print net1'
    print net2'
    print "should be altered two ways"
    (net1'',net2'') <- alter_neurons 2 3 (design_to_net design2)
    print net1''
    print net2''




--finding the bug
test10 = do
                let design = [((0,0),[(6.577902026013427e-2,(0,3)),(2.9939257933782062e-2,(0,2)),(-2.6304549820658316e-2,(2,0)),(-8.218781819194819e-2,(0,0)),(-1.1041119102555008e-3,(2,1)),(9.166562213774526e-2,(3,2)),(9.05378837675605e-3,(3,1))]),
                                     ((0,1),[(9.575892321757908e-2,(0,0)),(3.3728207958921175e-2,(3,2)),(-3.3598829763540616e-3,(2,1)),(5.116839734361722e-2,(0,2)),(3.815340492950309e-2,(3,1)),(5.341642099381605e-2,(3,0))]),
                                     ((0,2),[(1.7587177824529055e-2,(2,1)),(1.0483077768658856e-2,(3,3)),(9.33338194596059e-3,(3,0)),(-8.180039627413525e-2,(0,2)),(-9.088420244961858e-2,(2,0)),(6.498912730986006e-2,(0,0)),(-8.039723440519693e-2,(0,3))]),
                                     ((0,3),[(2.06622430277605e-2,(0,0)),(7.07485417221132e-3,(3,2)),(-7.377633498164099e-2,(0,3)),(6.756810164273083e-2,(0,2)),(-2.5910111455625162e-2,(3,0))]),
                                     ((0,4),[(-1.0804173335471817e-2,(2,1)),(-4.534713681353983e-3,(0,3)),(-6.984759566290717e-2,(0,0)),(-2.703079250966789e-2,(0,2)),(-2.6008530633378588e-2,(2,0))]),
                                     ((1,0),[(-8.380828928374262e-2,(2,2)),(-4.9384994548781425e-2,(2,0)),(-7.229691541134933e-2,(2,3)),(-5.072614491225032e-2,(1,1)),(-3.741665170681188e-2,(1,0))]),
                                     ((1,1),[]),
                                     ((1,2),[(4.5506745229327045e-2,(1,0)),(-7.737301213380053e-2,(2,3)),(8.153423517419608e-2,(2,1)),(-5.690100118270332e-2,(2,0)),(-7.317350267921828e-2,(2,2))]),
                                     ((1,3),[(8.611906469786354e-2,(1,0)),(7.495540233583364e-2,(2,2)),(4.5880166508181414e-2,(2,0)),(-5.704446717367857e-2,(2,1)),(-2.358684659018792e-2,(1,1)),(-7.120445274711887e-2,(2,3))]),
                                     ((2,0),[]),
                                     ((2,1),[]),
                                     ((2,2),[]),
                                     ((2,3),[]),
                                     ((3,0),[(4.901577015119314e-2,(0,1)),(-2.2625291744335446e-2,(0,2)),(-5.525930785801241e-2,(0,3)),(5.023692217852105e-2,(0,0))]),
                                     ((3,1),[(5.906086079946843e-2,(0,0)),(-4.236237247904791e-2,(0,3)),(-7.71716743096109e-2,(0,1)),(6.96876629203621e-2,(0,2))]),
                                     ((3,2),[]),
                                     ((3,3),[]),
                                     ((4,0),[]),
                                     ((4,1),[]),
                                     ((4,2),[]),
                                     ((4,3),[])]
                print "Start"
                let net = design_to_net design
                (inp,outs) <- dat
                let inputs = cycle inp
                let outputs = cycle outs
                let n_net = train_data net (take 1500 inputs) (take 1500 outputs) 0.01
                print $ output n_net (inp !! 0)
                let design2 = [((0,0),[(2.4364809864687944e-2,(0,3)),(2.3725099428270974e-2,(3,1)),(-5.151568631749562e-2,(1,1)),(8.153692835933743e-3,(0,1)),(5.030914896643321e-2,(1,3)),(-4.04249290580107e-2,(3,0))]),
                                       ((0,1),[(1.5386864952611257e-2,(0,1)),(2.8862554480309433e-2,(1,2)),(7.86534117726621e-2,(3,0)),(-6.86080711099373e-2,(3,2)),(9.377925502003479e-2,(0,3)),(6.163764211733008e-2,(3,3)),(2.8098981637196513e-2,(1,1)),(2.9203984604679184e-3,(1,3))]),
                                       ((0,2),[(8.195226737896984e-2,(1,3)),(8.088159071021986e-2,(1,1)),(4.366182645563704e-2,(3,2)),(-9.394618813991656e-2,(0,3)),(2.284662010552782e-2,(0,1)),(-3.641795007464231e-2,(3,1)),(4.4983203446493925e-2,(1,2))]),
                                       ((0,3),[(5.121694585852429e-2,(3,1)),(6.301502019008023e-2,(0,1)),(6.547610596950526e-2,(1,3)),(8.934545258899534e-2,(3,3)),(-3.4100891660514554e-3,(0,3))]),
                                       ((0,4),[(-7.418398446884072e-2,(1,3)),(-9.646540963952593e-2,(1,1)),(-2.0787059838140443e-2,(0,3)),(-9.330217595077378e-3,(0,1)),(4.220460931136913e-2,(1,2))]),
                                       ((1,0),[]),
                                       ((1,1),[(9.904146922313506e-2,(2,1)),(9.218019437473024e-4,(2,2)),(-5.2319161621044045e-2,(2,3)),(-7.593263292926378e-2,(0,3)),(2.8092828586015917e-2,(2,0)),(5.0436775627131974e-2,(0,2)),(4.17503825892131e-2,(0,1))]),
                                       ((1,2),[]),
                                       ((1,3),[(2.676057324283404e-2,(0,3)),(2.952827987702547e-2,(2,3)),(-9.536372975897234e-2,(2,2)),(-8.056509860354877e-2,(0,1)),(1.6677107786702616e-2,(0,2))]),
                                       ((2,0),[]),
                                       ((2,1),[(5.932553263186535e-4,(1,3)),(6.435360548020883e-2,(1,1)),(7.780457439384353e-3,(1,0)),(4.871260461016658e-2,(1,2))]),
                                       ((2,2),[(1.0225066831412583e-2,(1,0)),(-9.161314098406358e-2,(1,1)),(6.899238281879269e-2,(1,2)),(-9.481184832392953e-2,(1,3))]),
                                       ((2,3),[(2.4708006669330262e-2,(1,2)),(-6.387611675385957e-3,(1,3)),(-5.433739951088173e-2,(1,1)),(4.339579798876608e-2,(1,0))]),
                                       ((3,0),[]),
                                       ((3,1),[]),
                                       ((3,2),[]),
                                       ((3,3),[]),
                                       ((4,0),[]),
                                       ((4,1),[]),
                                       ((4,2),[]),
                                       ((4,3),[])]
                let net2 = design_to_net design2
                let n_net2 = train_data net2 (take 1500 inputs) (take 1500 outputs) 0.01
                print $ output n_net2 (inp !! 0)
                let found_net = compete net net2 250   (take 10000 inputs, take 10000 outputs) 0.01
                print $ output (snd found_net) (inp !! 0)
                g_net <- generate_random_net 4 5 10 8
                print g_net
                a_net <- alter_neurons 1 8 g_net
                print $ a_net

test11 = do
    let width = 4
    let depth = 5
    let nr_neuron = 10
    let nr_con = 8
    let net = empty_net width depth
    print "creating empty_net worked"
    nodes <- generate_random_nodes net nr_neuron
    print "generating random nodes worked"
    design <- add_neighbours nodes nr_con
    print "add_neighbours to design worked"
    let gen_net = app_design net design
    print "transforming to net worked"
    net <- add_bias gen_net nodes
    print "adding bias worked"

test12 = do
    g_net <- generate_random_net 4 5 10 8
    print g_net
    let design = net_to_design g_net
    let f_ns = free_nodes design
    let d_ns = full_nodes design
    to_delete <- draw_uniform 1 d_ns
    to_create <- draw_uniform (length to_delete) f_ns
    d_design <- do_all to_delete design (to_IO remove_neuron)
    altered_design <- do_all to_create d_design (add_neuron_design 4)
    print "Done"

test13 = do
        net <-generate_random_net 4 20 10 5
        (inp,outs) <- dat
        (n_inp, n_out) <- get_random_batch 10 (take 40 inp, take 40 outs)
        --training_batch :: Net -> [[Double]] -> [[Double]] -> Double -> Net
        let t_net = training_batch net (n_inp) (n_out) 0.01
        print $ output t_net (inp !! 0)
        print  $ outs !! 0
        net1 <-generate_random_net 4 20 10 5
        net2 <-generate_random_net 4 20 10 5
        (err, b_net) <- compete_batch 2000 50 0.1 net1 net2 (inp,outs)
        print $ err
        print $ output  b_net (inp !! 0)
        print  $ outs !! 0

test14 = do
    (inp,outs) <- dat
    (n_inp, n_out) <- get_random_batch 10 (inp,outs)
    print (n_inp, n_out)

test15 = do
    net <-generate_random_net 4 20 10 5
    (inp,outs) <- dat
    let net' = get_gradient net (inp !! 0) (outs !! 0) 0.01
    let f  = map( \( _ ,ts) -> map ( \(w,_) -> w) ts)
    print $ f $ net_to_design net'
    print "orignal net"
    print $  f $  net_to_design net

test16 = do
    net  <- generate_random_net 5 5 3 3
    changed <- change_out_net 2 net
    print changed

test17 = do
    net <- generate_random_net 7 10 3  7
    changed <- change_out_net 1 net
    (a_net,n_net)<- alter_neurons 1 3 changed
    print a_net
    print n_net
    (inp,outs) <- dat
    --t_net <- training_batches 100 10 changed (inp,outs) 0.01
    --print "changed done"
    --b_net2 <- training_batches 100 10 a_net (inp,outs) 0.01
    --print "Done with a_net"
    --b_net2 <- training_batches 100 10 n_net (inp,outs) 0.01
    --print "Done with n_net"
    --update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
    found <- update_random_net 100 100 10 1 3 (inp,outs) changed 0.01
    print "Done wiht found"
    --(n_a, n_b) <- alter_neurons 1 3 a_net
    --print n_a
    --print n_b
    --(n_c, n_d) <- alter_neurons 1 3 n_net
    --print n_c
    --print n_d

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
          the_net <- generate_fully_connected_net 7 7
          --the_net <- generate_random_net 7 10 40  7
          n_net <- change_out_net 1 the_net
          --update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
          --b_net <- update_random_net 1000 1000 10 2  7 (inp,outs) n_net 0.01
          -- training_batches nr_times bs net sample s
          b_net <- training_batches 10000 10 n_net (inp,outs) 0.01
          print $ output b_net (outs !! 0)
          putStrLn "Expected Output:"
          print  $ outs !! 0
