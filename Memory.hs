module Memory where
(
   save_net,
   load_net
)
import           Data.List
import           Data.List.Split
import           Data.Text       (strip)
import           Network
import           Types

save_net :: String -> Net ->IO()
save_net filename net = do
                        let nodes = app net
                        let nodes' = map ( map (\(Node _ b) -> b)) nodes
                        writeFile filename $ encode nodes'

load_net :: String -> IO Net
load_net filename = do
                        file <- readFile filename
                        let nodes' = decode file
                        let nodes = map( map(\b -> Node (0.0,0.0) b)) nodes'
                        return $ Net nodes


encode :: [[[(Double,(Int,Int))]]] -> String
encode = concatMap encode_layer
        where
             encode_layer =  ("l " ++) . ( concatMap encode_node )
             encode_node x =  "n " ++  concatMap encode_entry x
             encode_entry (w,(a,b)) = "i " ++ show w ++ " " ++ show a ++ " " ++ show b ++ " "


decode :: String ->  [[[(Double,(Int,Int))]]]
decode = map ( map ((map decode_entry) . tail . splitOn("i")) . tail . splitOn("n") ). tail . splitOn("l")

decode_entry :: String -> (Double,(Int,Int))
decode_entry entry = (w', (a', b'))
        where
                [w,a,b] = words entry
                w' = read w :: Double
                a' = read a :: Int
                b' = read b :: Int

the_net = generate_random_net 3 4 3 3

test_save_net :: IO()
test_save_net = do
                net <- the_net
                print net
                save_net "saved_net" net
                l_net <- load_net "saved_net"
                print l_net
