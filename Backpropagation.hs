module Backpropagation where

import           Control.Monad
import           Control.Parallel            (par, pseq)
import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Split
import           Data.Time
import           System.Random
import           Types

--ghc -O2 -optc-O3  -threaded -optc-ffast-math -fexcess-precision -funfolding-use-threshold=16 -o main.o the_net.hs  -fprof-auto  -fprof-cafs -fforce-recomp
--main.hs +RTS -N4 eventuell bringt threaded so gut wie gar nichts....

new_map = parMap rpar

softmax :: [Double] -> [Double]
softmax xs = map (1/norm*) e_xs
                where
                   e_xs = new_map exp xs
                   norm = sum e_xs


set_input :: Net -> [Double] -> Net
set_input (Net (layer : layers)) input = Net $  n_layer : layers
                                                where n_layer =  zipWith(\a (Node _ b) -> Node (a,0) b) (input++[1.0]) layer

reset :: Net -> Net
reset (Net layers) = Net $ new_map (map (\(Node _ b) -> Node (0,0) b)) layers

reset' :: Net -> Net
reset' (Net layers) = Net $ new_map (map (\(Node (a,_) b) -> Node (a,0) b)) layers

relu :: Node -> Node
relu (Node (x,_) ts) = if x >  0 then Node (x,x) ts else Node (x,0) ts

mult_relu' :: Node -> Node
mult_relu' (Node (a,z) ts) = if (a > 0) then Node (a,a*z) ts else Node (a,0) ts

f_propagate :: Net  -> Net
f_propagate (Net ([]))           = Net ([])
f_propagate (Net ([a]))      = Net ([new_map relu a])
f_propagate (Net (input:layers)) = Net $ n_input :app (f_propagate ( Net n_layers))
                                                      where
                                                           n_input = new_map relu input
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


set_last_deltas :: [Double] -> Net -> Net
set_last_deltas targets net = Net $ layers ++ [n_layer]
                                        where
                                          layers =  init $ app net
                                          n_layer = zipWith(\t (Node (a,_) ts) ->  Node(a,a-t) ts) targets $ last $ app net


apply_softmax :: Net -> Net
apply_softmax net = Net $ layers ++ [n_layer]
                                        where
                                          layers =  init $ app net
                                          s_m = softmax $ new_map(\(Node (a,_) _) -> a ) $ last $ app net
                                          n_layer = zipWith(\a (Node (_,b) ts) ->  Node(a,b) ts) s_m $ last $ app net


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



training_batch_classic :: Net -> ([[Double]], [[Double]]) -> Double -> Net
training_batch_classic  net set s =  foldr (\(i,t) net -> add_w net (get_gradient_net i t s) ) net n_set
                                                   where
                                                       n_set = (\(x,y) -> zip x y) set
                                                       add_up = zipWith(\(w1, x) (w2, _) -> (w1+w2,x))
                                                       add_w = apply(\(Node (a,z) ts) (Node (a',d) ts') -> Node (a,z) (add_up ts ts') )
                                                       get_gradient_net = get_gradient_classic net


training_batch :: Net -> ([[Double]], [[Double]]) -> Double -> Net
training_batch  net set s =  foldr (\(i,t) net -> add_w net (get_gradient_net i t s) ) net n_set
                                                   where
                                                       n_set = (\(x,y) -> zip x y) set
                                                       add_up = zipWith(\(w1, x) (w2, _) -> (w1+w2,x))
                                                       add_w = apply(\(Node (a,z) ts) (Node (a',d) ts') -> Node (a,z) (add_up ts ts') )
                                                       get_gradient_net = get_gradient net

get_gradient_classic :: Net -> [Double] -> [Double] ->  Double ->Net
get_gradient_classic net input targets s = get_gradient_f s c_net
                    where
                         r_net = reset net
                         i_net = set_input r_net input
                         f_net' = f_propagate i_net
                         f_net = apply_softmax f_net'
                         d_net = set_last_deltas targets $ reset' f_net
                         b_net = b_propagate d_net
                         c_net = apply (\(Node (a,z) ts) (Node (a',d) ts') -> Node (z,d) ts) f_net b_net

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
update_weights_nodes   xs s layers = new_map (\x -> update_weights_node x s layers)  xs

update_weights_node :: Node -> Double -> [[Node]] -> Node
update_weights_node (Node (z',d) ts) s layers = Node (z',d) n_ts
                                                                where
                                                                    zs = new_map (\(_, (l,n)) -> find_z layers (l,n)) ts
                                                                    n_ts = zipWith(\(w, t) z -> (w- (s*z*d), t)) ts zs

get_gradient_f :: Double -> Net  -> Net
get_gradient_f s (Net [a]) = Net  [a]
get_gradient_f s (Net (x:xs)) = Net $ (get_gradient_nodes x s xs) : (app $ get_gradient_f s (Net xs))

get_gradient_nodes :: [Node] -> Double -> [[Node]] -> [Node]
get_gradient_nodes   xs s layers = new_map (\x -> get_gradient_node x s layers)  xs

get_gradient_node :: Node -> Double -> [[Node]] -> Node
get_gradient_node (Node (z',d) ts) s layers = Node (z',d) n_ts
                                                                where
                                                                    zs = new_map (\(_, (l,n)) -> find_z layers (l,n)) ts
                                                                    n_ts = zipWith(\(w, t) z -> (- (s*z*d), t)) ts zs


find_z :: [[Node]] -> (Int,Int) -> Double
find_z layers (l,n)  = z
                            where
                                (xs,t:ys) = splitAt l layers
                                (ks,(Node (z,_) _):ls) = splitAt n t


get_random_batch :: Int -> ([[Double]],[[Double]])  -> IO ([[Double]],[[Double]])
get_random_batch n set = get_random_batch' n set ([],[])

get_random_batch' :: Int -> ([[Double]],[[Double]]) -> ([[Double]],[[Double]])  -> IO ([[Double]],[[Double]])
get_random_batch' 0 (input, output) (f_input,f_output) = return (f_input,f_output)
get_random_batch' n (input, output) (f_input,f_output) = do
                                                    idx <- randomRIO(0, (length input)-1)
                                                    let (fis, i:is)   = splitAt idx input
                                                    let (fos, o:os) = splitAt idx output
                                                    let n_is = is ++ fis
                                                    let n_os = fos ++ os
                                                    get_random_batch' (n-1) (n_is,n_os) (i:f_input, o:f_output)
