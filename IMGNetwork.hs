module IMGNetwork
(
        generate_image_net,
        alter_img_neurons
)
where



import           Control.Monad
import           Data.List
import           Data.List.Split
import           System.Random
import           Types
--using Box-Muller for generation of normal distribution
normal :: IO Double
normal = do
                d1 <- randomRIO(0::Double, 1::Double)
                d2 <- randomRIO(0::Double, 1::Double)
                let z = sqrt(-2.0 * (log d1)) * cos ( 2*pi*d2 )
                return $ 0.1 * z

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


removing_connections :: Int -> Net -> IO (Net,Net)
removing_connections nr_cons net = do
                             let design = net_to_design net
                             (cons, del_design) <- delete_connections nr_cons design
                             --r_design <- reset_con_weights cons design
                             --n_design <- add_connections nr_cons design
                             --return (design_to_net r_design, design_to_net n_design)
                             return (net, design_to_net del_design)

adding_connections :: Int -> Net -> IO (Net,Net)
adding_connections nr_cons net = do
                             let design = net_to_design net
                             --(cons, del_design) <- delete_connections nr_cons design
                             --r_design <- reset_con_weights cons design
                             n_design <- add_connections nr_cons design
                             --return (design_to_net r_design, design_to_net n_design)
                             return (net, design_to_net n_design)

alter_connections :: Int -> Net -> IO (Net,Net)
alter_connections nr_cons net = do
                             let design = net_to_design net
                             (cons, del_design) <- delete_connections nr_cons design
                             --r_design <- reset_con_weights cons design
                             n_design <- add_connections nr_cons del_design
                             --return (design_to_net r_design, design_to_net n_design)
                             return (net, design_to_net n_design)

reset_con_weights :: [((Int,Int),(Int,Int))] -> Design -> IO Design
reset_con_weights [] design = return design
reset_con_weights ((pos, entry):xs) design = do
                                                    w <- normal
                                                    let map_entry = map(\(w',ent) ->  if ent == entry then (w,ent) else (w',ent))
                                                    let n_des = map(\(x,ts) -> if x == pos then (x, map_entry ts) else (x,ts) ) design
                                                    reset_con_weights xs n_des


add_connections :: Int -> Design -> IO Design
add_connections nr_cons design = foldM (\x y -> add_connection x) design [1..nr_cons]


add_connection ::Design -> IO Design
add_connection design = do
                                             w <- normal
                                             let first_layer = init $ filter (\(x,_) -> x == 0) $ map fst design
                                             let p_nodes = first_layer ++ ( full_nodes design )
                                             let nr_p_cons ps  =  length $ connect_able_nodes ps design
                                             let choices = filter(\x -> nr_p_cons x > 0) p_nodes
                                             [choice@(c1,c2)] <- draw_uniform 1 choices
                                             [(a,b)] <- draw_uniform 1 $ connect_able_nodes choice design
                                             let n_entry = (w, (a-c1-1,b))
                                             let n_des = map(\(x,ts) -> if x == choice then (x,n_entry:ts) else (x,ts)) design
                                             return n_des

connect_able_nodes :: (Int,Int) -> Design -> [(Int,Int)]
connect_able_nodes pos design = filter(\x -> notElem x point_ats) f_f_nodes
                                        where
                                            f_f_nodes = full_front_nodes pos design
                                            point_ats = point_at pos design

full_front_nodes :: (Int,Int) -> Design -> [(Int,Int)]
full_front_nodes pos design = intersection front_nodes fu_nodes ++ last_layer
                             where
                                 front_nodes = d_f_nodes pos design
                                 fu_nodes = full_nodes design
                                 m = maximum $ map (\(x,_) -> x) front_nodes
                                 last_layer = filter (\(a,_) -> a == m) front_nodes

delete_connections :: Int -> Design -> IO ([((Int,Int),(Int,Int))],Design)
delete_connections n design = delete_connections' n ([],design)

delete_connections' :: Int ->  ([((Int,Int),(Int,Int))],Design) -> IO ([((Int,Int),(Int,Int))],Design)
delete_connections' 0 found = return found
delete_connections' n (xs,design) = do
                                            (x, n_des) <- delete_connection design
                                            delete_connections' (n-1) (x:xs, n_des)

delete_connection :: Design -> IO (((Int,Int),(Int,Int)),Design)
delete_connection design = do
                                             let bias_pos = last $ map (\(x,_) -> x) $ filter (\((a,_),_) -> a == 0 ) design
                                             let nr_own_cons ps = length $ point_at ps design
                                             let choices = filter( /=bias_pos)$ filter(\x -> nr_own_cons  x > 1)  $ desconnect_able_nodes design
                                             [choice] <- draw_uniform 1 choices
                                             let p_ats = point_at choice design
                                             let nr_tos ps = length $ filter (/= bias_pos) $ point_to ps design
                                             let p_ats' = filter (\x -> nr_tos x > 1) p_ats
                                             k <-randomRIO(0,(length p_ats')-1)
                                             let to_delete = (\(a,b) -> (a- (fst choice)-1,b ))  (p_ats' !! k)
                                             let ts = snd $ head $ filter(\(x,ts) -> x ==choice) design
                                             let n_ts = filter(\(_,x) -> x /= to_delete) ts
                                             let n_des = map(\(x,ts) -> if x == choice then (x,n_ts) else (x,ts)) design
                                             return  ((choice, to_delete), n_des)

alter_img_neurons:: Int -> Int -> Int -> Net -> IO (Net,Net)
alter_img_neurons img_width nr_neuron nr_cons net = alter_neurons' img_width nr_neuron nr_cons (net,net)

alter_neurons' :: Int -> Int -> Int -> (Net,Net) -> IO (Net,Net)
alter_neurons' width 0 nr_cons nets = return nets
alter_neurons' width nr_neuron nr_cons (net1,net2) = do
                                                (n_net1,n_net2) <-  alter_neuron width nr_cons (net1,net2)
                                                alter_neurons' width (nr_neuron-1) nr_cons(n_net1,n_net2)

alter_neuron :: Int -> Int -> (Net,Net) ->IO (Net,Net)
alter_neuron width nr_cons (net1,net2) = do
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
                            --reset_design <- do_all to_delete org_design1 reset_weights
                            d_design <- do_all to_delete org_design2 (to_IO remove_neuron)
                            altered_design <- do_all to_create d_design (add_neuron_design width nr_cons)
                            --let r_net = design_to_net reset_design
                            let a_net = design_to_net altered_design
                            return (net1, a_net)

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


add_neuron_design :: Int -> Int -> (Int,Int) -> Design -> IO Design
add_neuron_design width n_cons pos design = do
                                                                    let (bias,b_ns) = full_d_b_nodes pos design
                                                                    let f_ns = full_d_f_nodes pos design
                                                                    bs <- draw_from_list width 1 pos ([],b_ns)
                                                                    fs <- draw_from_list width (n_cons-1) pos ([],f_ns)
                                                                    f <- connect_b pos $ fst bs
                                                                    g <- connect_f pos $ fst fs
                                                                    h <- connect_f bias [pos]
                                                                    let b_d = snd $ (appD g) design
                                                                    let n_d =  snd $ (appD f) b_d
                                                                    return $ snd $ (appD h) n_d

draw_from_list ::Int -> Int -> (Int,Int)-> ([(Int,Int)],[(Int,Int)]) -> IO ([(Int,Int)], [(Int,Int)])
draw_from_list width 0 _ xs = return xs
draw_from_list width _ _ (a,[]) = return (a,[])
draw_from_list width n_cons pos nodes@(x,y) = do
                                                            let ds = map (distance width pos) (snd nodes)
                                                            let is = to_interval 0 ds
                                                            f <- randomRIO(0.0, last is)
                                                            let idx = get_index f is 0
                                                            let (xs,y:ys) = splitAt idx (snd nodes)
                                                            draw_from_list width (n_cons-1) pos (y:x, xs ++ ys)

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


desconnect_able_nodes :: Design -> [(Int,Int)]
desconnect_able_nodes dsgn = filter (\x-> posses_removable_connection x dsgn) $ map fst dsgn

posses_removable_connection :: (Int,Int) -> Design -> Bool
posses_removable_connection pos design = or $ map (\x -> length x > 1) p_ats
                                                            where
                                                                bias_pos = last $ map (\(x,_) -> x) $ filter (\((a,_),_) -> a == 0 ) design
                                                                p_at = point_at pos design
                                                                p_ats = map(\z -> filter(\y -> y /= bias_pos) z) $ map(\x -> point_to x design)  p_at


--which nodes from previos layers point to the node
point_to ::  (Int,Int) -> Design -> [(Int,Int)]
point_to (c,d) = map(fst) . filter (\((a,_),ts) -> elem (c-a-1,d)  (map(snd) ts) )

-- which nodes the node points to in following layers
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
                                                    let net' = convert_net2_net r_net
                                                    change_out_net 1 net'

generate_image_net:: Int -> Int -> Int -> Int -> Int -> Int -> IO Net
generate_image_net img_width img_height depth nr_classes nr_neuron nr_con =
                                                        do
                                                            net2 <- generate_random_net2 img_width img_height depth nr_neuron nr_con
                                                            let net' = convert_net2_net net2
                                                            change_out_net nr_classes net'

generate_random_net2:: Int ->Int -> Int -> Int -> Int -> IO (Net2 Double)
generate_random_net2 img_width img_height depth nr_neuron nr_con =
                                                            do
                                                                let net = empty_net (img_width*img_height) depth
                                                                nodes <- generate_random_nodes net nr_neuron
                                                                design <- add_neighbours img_width nodes nr_con
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

distance :: Int -> (Int,Int) -> (Int,Int) -> Float
distance  width (a,b) (c,d) = sqrt  $ fromIntegral $ (a-c)^2 + (y1 -y2)^2 + (z1-z2)^2
                                        where
                                            y1 = div b width
                                            y2 = div d width
                                            z1 = mod b width
                                            z2 = mod d width

f_nodes :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
f_nodes (a,b) nodes = filter(\(c,d) -> c > a ) nodes

b_nodes :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
b_nodes (a,b) nodes = filter(\(c,d) -> c < a ) nodes

taken :: [((Int,Int),[(Double,(Int,Int))])] -> (Int,Int) -> [(Int,Int)]
taken (x:xs) (a,b) = if fst x == (a,b) then map(\(d,(c,e))-> (a+c+1,e)) (snd x) else taken xs (a,b)


gen_empty_design :: [(Int,Int)] -> [((Int,Int),[(Double,(Int,Int))])]
gen_empty_design xs = map(\(a,b) -> ((a,b),[])) xs

add_neighbours:: Int -> [(Int,Int)] -> Int -> IO [((Int,Int),[(Double,(Int,Int))])]
add_neighbours width nodes nr_con = do
                                                    let e_d = gen_empty_design nodes
                                                    d <- draw_b_neighbours width nodes nodes e_d
                                                    draw_f_neighbours width nodes nodes d nr_con

draw_f_neighbours :: Int -> [(Int,Int)] -> [(Int,Int)] -> [((Int,Int),[(Double,(Int,Int))])] -> Int -> IO [((Int,Int),[(Double,(Int,Int))])]
draw_f_neighbours width [] nodes design n_cons = return design
draw_f_neighbours width (n:nodes) a_nodes design n_cons = do
                                                                let f_n = f_nodes n a_nodes
                                                                let t = taken design n
                                                                let rest = filter (\z -> notElem z t) f_n
                                                                x <- draw_many width rest n (n_cons)
                                                                let n_design = foldr (\z y -> to_design y z) design x
                                                                draw_f_neighbours width nodes a_nodes n_design n_cons

draw_many :: Int -> [(Int,Int)] -> (Int,Int) -> Int -> IO [([(Int,Int)],(Int,Int), [(Double, (Int,Int))])]
draw_many width [] n b = return []
draw_many width nodes n 1 = return []
draw_many width  ns@(no:nodes) n n_con = do
                                                                x <- draw_f_neighbour width ns n
                                                                let l = (\(a,b,c) -> a) x
                                                                xs <- draw_many width l n (n_con-1)
                                                                return (x : xs)


draw_f_neighbour ::Int -> [(Int,Int)] -> (Int,Int) -> IO ([(Int,Int)],(Int,Int), [(Double, (Int,Int))])
draw_f_neighbour width nodes n@(b,a) = let
                                                ds = map(\x -> distance width n x) nodes
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

draw_b_neighbours :: Int -> [(Int,Int)] -> [(Int,Int)] -> [((Int,Int),[(Double,(Int,Int))])] -> IO [((Int,Int),[(Double,(Int,Int))])]
draw_b_neighbours width [] nodes design = return design
draw_b_neighbours width (n:nodes) a_nodes design = do
                                                                  x <- draw_b_neighbour width (b_nodes n a_nodes) n
                                                                  let n_design = to_design design x
                                                                  draw_b_neighbours width  nodes a_nodes n_design

to_design :: [((Int,Int),[(Double,(Int,Int))])] -> ([(Int,Int)],(Int,Int), [(Double, (Int,Int))]) -> [((Int,Int),[(Double,(Int,Int))])]
to_design [] _ = []
to_design (x@(a,b):xs) e@(_,c,d) = if a == c then (a,b++d):n_xs else x:n_xs
                                                where
                                                    n_xs = to_design xs e

draw_b_neighbour :: Int -> [(Int,Int)] -> (Int,Int) -> IO ([(Int,Int)],(Int,Int), [(Double, (Int,Int))])
draw_b_neighbour width nodes (0,a) = return (nodes,(0,a),[])
draw_b_neighbour width nodes n = let
                                                    ds = map(\x -> distance width n x) nodes
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
