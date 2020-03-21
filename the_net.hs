data Node = Node (Double,Double) [(Double, (Int,Int))] deriving Show
data Net  = Net [[Node]] deriving Show

simple_net :: Net
simple_net = Net [
                                [ Node (10,0) [(0.1 , (0,0)), (0.1 , (1,0))] , Node (5,0) [(0.1,(0,1)),(0.1,(1,1))] ],
                                [ Node (0,0) [(0.1 , (0,0))]   , Node (0,0) [(0.1,(0,1))]  ],
                                [ Node (0,0) [(0.1 , (0,0))]   , Node (0,0) [(0.1,(0,0))]  ]
                             ]

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
                                      (xs,t:ys) = splitAt l layers
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
