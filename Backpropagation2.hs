--module Backpropagation2 where

import           Control.Monad
import           Control.Parallel            (par, pseq)
import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Split
import           Data.Time
--import           Network2
import           Data.Sequence               as V
import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
--import qualified Data.Vector                 as V
import           System.Random
import           Types2

--ghc -O2 -optc-O3  -threaded -optc-ffast-math -fexcess-precision -funfolding-use-threshold=16 -o main.o the_net.hs  -fprof-auto  -fprof-cafs -fforce-recomp
--main.hs +RTS -N4 eventuell bringt threaded so gut wie gar nichts....

new_map f  xs = map f xs `using` parListChunk 200 rseq --parMap rpar

softmax :: V.Vector Double -> V.Vector Double
softmax xs = V.map (1/norm*) e_xs
                where
                   x_max = V.maximum xs
                   e_xs = V.map (\x -> exp (x-x_max)) xs
                   norm = V.sum e_xs


set_input :: Net -> V.Vector Double -> Net
set_input (Net layers) input = Net $ V.cons  n_layer  (V.tail layers)
                                                where
                                                    layer = V.head layers
                                                    n_layer =  V.zipWith(\a (Node _ b) -> Node (a,0) b) ( V.snoc input 1.0) layer


reset :: Net -> Net
reset (Net layers) = Net $ V.map (V.map (\(Node _ b) -> Node (0,0) b)) layers

reset' :: Net -> Net
reset' (Net layers) = Net $ V.snoc  (V.map (V.map (\(Node (a,_) b) -> Node (a,0) b)) ( V.init layers)) (V.last layers)

relu :: Node -> Node
relu (Node (x,_) ts) = if x >  0 then Node (x,x) ts else Node (x,0) ts

copy :: Node -> Node
copy (Node (x,_) ts) = Node (x,x) ts

mult_relu' :: Node -> Node
mult_relu' (Node (a,z) ts) = if (a > 0) then Node (a,z) ts else Node (a,0) ts

headSplit xs = (V.head y,ys)
    where
        (y, ys) = V.splitAt 1 xs

f_propagate :: Net  -> Net
f_propagate (Net layers) | V.length layers == 0           = Net (V.empty)
f_propagate (Net layers)  | V.length layers == 1     = Net (V.map (V.map copy) layers)
f_propagate (Net layers) = Net $ V.cons input (app (f_propagate ( Net n_layers)))
                                                      where
                                                           (l,layers') = headSplit layers
                                                           input = V.map relu l
                                                           n_layers = f_propagate_nodes input layers'


f_propagate_nodes :: V.Vector Node -> V.Vector (V.Vector Node) -> V.Vector (V.Vector Node)
f_propagate_nodes nodes layers | V.length nodes == 0 = layers
f_propagate_nodes nodes layers = f_propagate_nodes xs n_layers
                                                    where
                                                       (x,xs) = headSplit nodes
                                                       n_layers = f_propagate_node x layers


f_propagate_node :: Node ->  V.Vector (V.Vector Node) -> V.Vector (V.Vector Node)
f_propagate_node ( Node _ ts) layers | (V.length ts == 0) = layers
f_propagate_node ( Node (a,z)  targets ) layers = f_propagate_node (Node (a,z) targets' )  (xs V.++ ( V.cons (as V.++ ( V.cons (Node (s+w*z,0) ts)  bs' ))  ys')  )
                                                                      where
                                                                        ((w,(l,n)),targets') = headSplit targets
                                                                        (xs, ys) = V.splitAt l layers
                                                                        (t,ys') = headSplit ys
                                                                        (as, bs) = V.splitAt n t
                                                                        (Node (s,_) ts, bs' ) = headSplit bs


set_last_deltas :: V.Vector Double -> Net -> Net
set_last_deltas targets net = Net $ V.snoc layers  n_layer
                                        where
                                          layers =  V.init $ app net
                                          n_layer = V.zipWith(\t (Node (a,b) ts) ->  Node(a,b-t) ts) targets $ V.last $ app net


apply_softmax :: Net -> Net
apply_softmax net = Net $ V.snoc layers n_layer
                                        where
                                          layers =  V.init $ app net
                                          s_m = softmax $ V.map(\(Node (a,_) _) -> a ) $ V.last $ app net
                                          n_layer = V.zipWith(\b (Node (a,_) ts) ->  Node(a,b) ts) s_m $ V.last $ app net


b_propagate :: Net -> Net
b_propagate (Net layers) | length layers == 0        = Net layers
b_propagate (Net layers) | length layers == 2      = Net $ V.cons (b_propagate_nodes a ls)  ls
    where
        ls = V.tail layers
        a = V.head layers
b_propagate (Net (layers)) = Net  $ V.cons  (b_propagate_nodes x n_layers)  (app (b_propagate (Net layers')) )
    where
        x = V.head layers
        layers' = V.tail layers
        n_layers = app (b_propagate (Net layers'))

b_propagate_nodes :: V.Vector Node -> V.Vector ( V.Vector Node )-> V.Vector Node
b_propagate_nodes xs _  | V.length xs == 0 =V.empty
b_propagate_nodes nodes layers = V.cons n_node ( b_propagate_nodes nodes' layers)
    where
        x = V.head nodes
        nodes' = V.tail nodes
        n_node = b_propagate_node x layers

b_propagate_node :: Node -> V.Vector (V.Vector Node) -> Node
b_propagate_node (Node x ts) layers | length ts == 0 = mult_relu' (Node x ts)
b_propagate_node (Node (a,v) ts) layers = b_propagate_node (Node (a, v + w*d ) ts') layers
    where
        (w,(l,n)) = V.head ts
        ts' = V.tail ts
        (_,tts) = V.splitAt l layers
        t = V.head tts
        (ks, ls) = V.splitAt n t
        (Node (_,d) _) = V.head ls


training_normed_batch_classic :: Net -> (V.Vector (V.Vector Double), V.Vector (V.Vector Double)) -> Double -> Net
training_normed_batch_classic  net set s =  foldr (\(i,t) net -> add_w net (get_gradient_net i t s) ) net n_set
    where
        n_set = (\(x,y) -> V.zip x y) set
        add_up = V.zipWith(\(w1, x) (w2, _) -> (w1+w2,x))
        add_w = apply(\(Node (a,z) ts) (Node (a',d) ts') -> Node (a,z) (add_up ts ts') )
        get_gradient_net = get_normed_gradient_classic net

training_batch_classic :: Net -> (V.Vector (V.Vector Double), V.Vector (V.Vector Double)) -> Double -> Net
training_batch_classic  net set s =  foldr (\(i,t) net -> add_w net (get_gradient_net i t s) ) net n_set
    where
        n_set = (\(x,y) -> V.zip x y) set
        add_up = V.zipWith(\(w1, x) (w2, _) -> (w1+w2,x))
        add_w = apply(\(Node (a,z) ts) (Node (a',d) ts') -> Node (a,z) (add_up ts ts') )
        get_gradient_net = get_gradient_classic net


training_batch :: Net -> (V.Vector (V.Vector Double), V.Vector (V.Vector Double)) -> Double -> Net
training_batch  net set s =  foldr (\(i,t) net -> add_w net (get_gradient_net i t s) ) net n_set
                                                   where
                                                       n_set = (\(x,y) -> V.zip x y) set
                                                       add_up = V.zipWith(\(w1, x) (w2, _) -> (w1+w2,x))
                                                       add_w = apply(\(Node (a,z) ts) (Node (a',d) ts') -> Node (a,z) (add_up ts ts') )
                                                       get_gradient_net = get_gradient net

get_normed_gradient_classic :: Net -> V.Vector Double -> V.Vector Double ->  Double ->Net
get_normed_gradient_classic net input targets s = multiply_weigths (s/norm) gradient
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net' = f_propagate i_net
                         f_net = apply_softmax f_net'
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net
                         gradient = get_gradient_f' c_net
                         norm = get_norm gradient

get_gradient_classic :: Net -> V.Vector Double -> V.Vector Double ->  Double ->Net
get_gradient_classic net input targets s = get_gradient_f s c_net
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net' = f_propagate i_net
                         f_net = apply_softmax f_net'
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net

get_gradient :: Net -> V.Vector Double -> V.Vector Double ->  Double ->Net
get_gradient net input targets s = get_gradient_f s c_net
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net = f_propagate i_net
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net

train :: Net -> V.Vector Double -> V.Vector Double ->  Double ->Net
train net input targets s = update_weights s c_net
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net = f_propagate i_net
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net


apply :: (Node -> Node -> Node) -> Net -> Net -> Net
apply f net1 net2 = Net $ V.zipWith( V.zipWith( f)) layers1 layers2
                                       where
                                           layers1 = app net1
                                           layers2 = app net2

update_weights :: Double -> Net  -> Net
update_weights s (Net layers) | length layers == 1 = Net  layers
update_weights s (Net xs) = Net $ V.cons (update_weights_nodes x s xs')  (app $ update_weights s (Net xs'))
    where
        x = V.head xs
        xs' = V.tail xs

update_weights_nodes :: V.Vector Node -> Double -> V.Vector (V.Vector Node) ->  V.Vector Node
update_weights_nodes   xs s layers = V.map (\x -> update_weights_node x s layers)  xs

update_weights_node :: Node -> Double -> V.Vector (V.Vector Node)  -> Node
update_weights_node (Node (z',d) ts) s layers = Node (z',d) n_ts
    where
        zs = V.map (\(_, (l,n)) -> find_z layers (l,n)) ts
        n_ts = V.zipWith(\(w, t) z -> (w- (s*z'*z), t)) ts zs


get_gradient_f' ::  Net  -> Net
get_gradient_f'  (Net xs) | length xs == 1 = Net  xs
get_gradient_f'  (Net xs) = Net $ V.cons (get_gradient_nodes' x  xs')  (app $ get_gradient_f'  (Net xs'))
    where
        x = V.head xs
        xs' = V.tail xs

get_gradient_f :: Double -> Net  -> Net
get_gradient_f s (Net xs) | length xs == 1 = Net  xs
get_gradient_f s (Net xs) = Net $ V.cons (get_gradient_nodes x s xs') (app $ get_gradient_f s (Net xs'))
    where
        x = V.head xs
        xs' = V.tail xs

multiply_weigths :: Double -> Net -> Net
multiply_weigths norm net = Net n_layers
                where
                    layers = app net
                    n_layers = V.map ( V.map (\(Node x ts)-> Node x (V.map (\(w,t) -> (norm*w,t) ) ts  ))) layers

get_norm :: Net -> Double
get_norm net = sqrt $ sum $ V.map (\x -> x*x) weights
                where
                    weights = get_weigths net

get_weigths :: Net -> V.Vector Double
get_weigths net = V.map fst all_ts
    where
        layers = app net
        all_ts = V.concat $ V.toList $ ( V.map(\(Node _ ts) -> ts) ( V.concat (V.toList layers)) )

get_gradient_nodes' :: V.Vector Node  -> V.Vector (V.Vector Node) -> V.Vector Node
get_gradient_nodes'   xs layers = V.map (\x -> get_gradient_node' x  layers)  xs

get_gradient_node' :: Node -> V.Vector (V.Vector Node) -> Node
get_gradient_node' (Node (z',d) ts)  layers = Node (z',d) n_ts
                                                                where
                                                                    zs = V.map (\(_, (l,n)) -> find_z layers (l,n)) ts
                                                                    n_ts = V.zipWith(\(w, t) z -> (- (z'*z), t)) ts zs


get_gradient_nodes :: V.Vector Node -> Double -> V.Vector (V.Vector Node)  -> V.Vector Node
get_gradient_nodes   xs s layers = V.map (\x -> get_gradient_node x s layers)  xs

get_gradient_node :: Node -> Double -> V.Vector (V.Vector Node) -> Node
get_gradient_node (Node (z',d) ts) s layers = Node (z',d) n_ts
                                                                where
                                                                    ds = V.map (\(_, (l,n)) -> find_z layers (l,n)) ts
                                                                    n_ts = V.zipWith(\(w, t) d' -> (- (s*z'*d'), t)) ts ds


find_z :: V.Vector (V.Vector Node) -> (Int,Int) -> Double
find_z layers (l,n)  = z
    where
        (xs,ys) = V.splitAt l layers
        t = V.head ys
        (ks, ls) = V.splitAt n t
        Node (_,z) _ = V.head ls


get_random_batch :: Int -> (V.Vector(V.Vector Double),V.Vector(V.Vector Double))  -> IO (V.Vector(V.Vector Double),V.Vector(V.Vector Double))
get_random_batch n set = get_random_batch' n set (V.empty,V.empty)

get_random_batch' :: Int -> (V.Vector(V.Vector Double),V.Vector(V.Vector Double)) -> (V.Vector(V.Vector Double),V.Vector(V.Vector Double))  -> IO (V.Vector(V.Vector Double),V.Vector(V.Vector Double))
get_random_batch' 0 (input, output) (f_input,f_output) = return (f_input,f_output)
get_random_batch' n (input, output) (f_input,f_output) = do
                                                    idx <- randomRIO(0, (length input)-1)
                                                    let (fis, is')   = V.splitAt idx input
                                                    let is = V.tail is'
                                                    let i = V.head is'
                                                    let (fos, os') = V.splitAt idx output
                                                    let os = V.tail os'
                                                    let o = V.head os'
                                                    let n_is = is V.++ fis
                                                    let n_os = fos V.++ os
                                                    get_random_batch' (n-1) (n_is,n_os) ( V.cons i f_input, V.cons o f_output)
test_get_gradient_classic :: IO()
test_get_gradient_classic = do
    let net = Net $ V.fromList [ V.fromList [Node (0,0) ( V.fromList [(1,(0,0))] ) , Node (1,0) ( V.fromList [(1,(0,1))] ) ]
                                              , V.fromList [Node (0,0) ( V.fromList [(1,(0,0))] ) , Node (0,0)  ( V.fromList [(1,(0,1))] ) ]
                                              , V.fromList [Node (0,0) V.empty , Node (0,0) V.empty ]
                                              ]
    let start = [ V.fromList [Node (0,0) ( V.fromList [(1,(0,0))] ) , Node (1,0) ( V.fromList [(1,(0,1))] ) ] ]
    let ls = replicate 10000 $ V.fromList [Node (0,0) ( V.fromList [(1,(0,0))] ) , Node (0,0)  ( V.fromList [(1,(0,1))] ) ]
    let end = [V.fromList [Node (0,0) V.empty , Node (0,0) V.empty ]]
    let net = Net $ V.fromList $ start ++ ls ++ end
    getCurrentTime >>= print
    --print $ app net
    let net' = set_input net (V.fromList [1,1])
    --print $ app net'
    let f_net = f_propagate net'
    --print $ app f_net
    let net'' = reset' $ apply_softmax f_net
    let b_net = b_propagate $ set_last_deltas (V.fromList [0,1]) net''
    let c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net
    --print $ app b_net
    --print $ app c_net
    let n_net = get_gradient_classic net (V.fromList [1,1]) (V.fromList [0,1]) 1
    print $ V.last $  app n_net
    getCurrentTime >>= print
    print "hi"
{--
test_get_weigths :: IO()
test_get_weigths = do
                    net <- generate_random_net 4 4 4 3
                    print $ get_weigths net

test_get_norm :: IO()
test_get_norm = do
                    net <- generate_random_net 4 4 4 3
                    let weigths = get_weigths net
                    let norm = get_norm net
                    print $ sum $ map(^2) $ map (1/norm*) weigths

test_multiply_weigths :: IO()
test_multiply_weigths = do
                    net <- generate_random_net 4 4 4 3
                    print $ get_weigths net
                    print $ get_weigths $ multiply_weigths 10 net

test_get_gradient_classic :: IO()
test_get_gradient_classic = do
    let net = Net [ [Node (0,0) [(1,(0,0))], Node (1,0) [(1,(0,1))]], [Node (0,0) [(1,(0,0))], Node (0,0) [(1,(0,1))]], [Node (0,0) [], Node (0,0) [] ] ]
    let net' = set_input net [1,1]
    let f_net = f_propagate net'
    print $ app f_net
    let net'' = reset' $ apply_softmax f_net
    let b_net = b_propagate $ set_last_deltas [0,1] net''
    let c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net
    print $ app b_net
    print $ app c_net
    let n_net = get_gradient_classic net [1,1] [0,1] 1
    print $ app n_net
    print "hi"
--}

main :: IO ()
main = test_get_gradient_classic
