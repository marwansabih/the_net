module TestVectors where
import           Control.Monad       (forM_)
import           Data.Time.Clock     (diffUTCTime, getCurrentTime)
import           Data.Vector.Mutable as VM
import           GHC.Prim
import           IMGNetwork
import           Types

gmForM = Control.Monad.forM_

type FNode = (Double,Double,Double, MVector RealWorld (Double,Double,Int,Int))

type FLayer = MVector RealWorld (FNode)

type FNet = MVector RealWorld  (MVector RealWorld (FNode))



relu :: Double -> Double
relu  x = if x > 0 then x else 0

relu' :: Double -> Double
relu' x = if x > 0 then 1 else 0


net :: IO FNet
net = netFromList [[(0,0,0,[(1,0,1,0)]),(0,0,0,[(1,0,1,1)])]
                              ,[(0,0,0,[(1,0,1,0)]),(0,0,0,[(1,0,1,1)])]
                              ,[(0,0,0,[(1,0,1,0)]),(0,0,0,[(1,0,1,1)])]
                              ,[(0,0,0,[]),(0,0,0,[])]
                              ]

rawNet :: [[(Double,Double,Double,[(Double,Double,Int,Int)])]]
rawNet = (Prelude.replicate 10000 [(0.0,0.0,0.0,[(1.0,0.0,1,0)]),(0.0,0.0,0.0,[(1.0,0.0,1,1)])] ) Prelude.++  [[(0.0,0.0,0.0,[]),(0.0,0.0,0.0,[])]]

net2 :: IO FNet
net2 = netFromList rawNet

resetNet :: FNet -> IO ()
resetNet net = do
    gmForM [0..(VM.length net -1)] $ \idx1 -> do
        layer <- VM.read net idx1
        gmForM [0..(VM.length layer -1)] $ \idx2 -> do
            (_,_,_,ts) <- VM.read layer idx2
            VM.write layer idx2 (0,0,0,ts)

setInput :: [Double] -> FNet -> IO ()
setInput xs net = do
    gmForM ( Prelude.zip [0..] (xs ++ [1]) )$ \(nr,b) -> do
        layer <- VM.read net 0
        (a,z,d,xs) <- VM.read layer nr
        VM.write layer nr (a,b,d,xs)

setInput' :: [Double] -> FNet -> IO ()
setInput' xs net = do
    gmForM ( Prelude.zip [0..] xs )$ \(nr,b) -> do
        layer <- VM.read net 0
        (a,z,d,xs) <- VM.read layer nr
        VM.write layer nr (a,b,d,xs)

fPropagate :: (Double -> Double) -> FNet -> IO ()
fPropagate f net = do
    gmForM [0..(VM.length net -3)] $ \idx -> do
        layer <- VM.read net idx
        gmForM [0..(VM.length layer -1)] $ \idx2 -> do
            (_,z,_,ts) <- VM.read layer idx2
            gmForM [0..(VM.length ts -1)] $ \idx3 -> do
                (w,dw,y,x) <-VM.read ts idx3
                destLayer <- VM.read net (y+idx)
                (a,z',d,xs) <- VM.read destLayer x
                VM.write destLayer x ((a+w*z),z',d,xs)
            nextLayer <- VM.read net (idx + 1 )
            gmForM [0.. (VM.length nextLayer -1)] $ \idx4 -> do
                (a,_,d,xs) <- VM.read nextLayer idx4
                VM.write nextLayer idx4 (a,f a, d, xs)
    let idx = VM.length net -2
    layer <- VM.read net idx
    gmForM [0..(VM.length layer -1)] $ \idx2 -> do
        (_,z,_,ts) <- VM.read layer idx2
        gmForM [0..(VM.length ts -1)] $ \idx3 -> do
            (w,dw,y,x) <-VM.read ts idx3
            destLayer <- VM.read net (y+idx)
            (a,z',d,xs) <- VM.read destLayer x
            VM.write destLayer x ((a+w*z),z',d,xs)
        nextLayer <- VM.read net (idx + 1 )
        gmForM [0.. (VM.length nextLayer -1)] $ \idx4 -> do
            (a,_,d,xs) <- VM.read nextLayer idx4
            VM.write nextLayer idx4 (a, a, d, xs)


sumD :: MVector RealWorld (FNode) -> IO Double
sumD vector = do
    vsum <- VM.replicate 1 (0::Double)
    gmForM [0..(VM.length vector -1)] $ \idx -> do
        (_,_,d,_) <- VM.read vector idx
        entry <- VM.read vsum 0
        VM.write vsum 0 (d+entry)
    v <- VM.read vsum 0
    return v

maxZ :: MVector RealWorld (FNode) -> IO Double
maxZ vector = do
    (_,z,_,_) <- VM.read vector 0
    vsum <- VM.replicate 1 z
    gmForM [1..(VM.length vector -1)] $ \idx -> do
        (_,z,_,_) <- VM.read vector idx
        entry <- VM.read vsum 0
        if z > entry then VM.write vsum 0 z else VM.write vsum 0 entry
    v <- VM.read vsum 0
    return v


softmax :: FNet -> IO ()
softmax net = do
    let lastIdx = VM.length net -1
    layer <- VM.read net lastIdx
    maxV <- maxZ layer
    gmForM [0..(VM.length layer -1) ] $ \idx -> do
        (a,z,_,ts) <- VM.read layer idx
        VM.write layer idx (a,z, exp (z-maxV), ts)
    norm <- sumD layer
    gmForM [0..(VM.length layer -1) ]  $ \idx -> do
        (a,z,d,ts) <- VM.read layer idx
        VM.write layer idx (a,z,d/norm,ts)

setFirstDeltas :: [Double] -> FNet -> IO ()
setFirstDeltas xs net = do
    softmax net
    layer <- VM.read net (VM.length net - 1)
    gmForM [0..(VM.length layer -1)] $ \idx -> do
        (a,z,d,ts) <- VM.read layer idx
        VM.write layer idx (a,z,d - (xs !! idx),ts)
    --return net

bPropagate :: (Double -> Double) -> FNet -> IO ()
bPropagate f net = do
    let sndLast = VM.length net -2
    gmForM [sndLast,(sndLast-1)..1] $ \idx -> do
        layer <- VM.read net idx
        let end = VM.length layer - 1
        gmForM [0..end] $ \idx2 -> do
            (a,z,d,ts) <- VM.read layer idx2
            gmForM [0..(VM.length ts -1)] $ \idx3 -> do
                (w,dw,y,x) <- VM.read ts idx3
                destLayer <- VM.read net ( idx + y)
                (_,_,d',_) <- VM.read destLayer x
                (_,_,d,_) <- VM.read layer idx2
                VM.write layer idx2 (a,z,d+w*d',ts)
        gmForM [0..end] $ \idx -> do
            (a,z,d,ts) <- VM.read layer idx
            VM.write layer idx (a,z,(f a)*d,ts)

updateDW :: FNet -> IO ()
updateDW net = do
    gmForM [0..(VM.length net-2)] $ \idx -> do
        layer <- VM.read net idx
        gmForM [0..(VM.length layer -1)] $ \idx2 -> do
            (_,z,_,ts) <- VM.read layer idx2
            gmForM [0.. (VM.length ts -1)] $ \idx3 -> do
                (w,dw,y,x) <- VM.read ts idx3
                targets <- VM.read net (idx + y)
                (_,_,d,_) <- VM.read targets x
                VM.write ts idx3 (w,dw+z*d,y,x)

updateWeights :: Double -> FNet -> IO ()
updateWeights s net  = do
    gmForM [0..(VM.length net-2)] $ \idx -> do
        layer <- VM.read net idx
        gmForM [0..(VM.length layer -1)] $ \idx2 -> do
            (_,_,_,ts) <- VM.read layer idx2
            gmForM [0.. (VM.length ts -1)] $ \idx3 -> do
                (w,dw,y,x) <- VM.read ts idx3
                VM.write ts idx3 (w-(dw*s),0,y,x)

trainBatch :: [([Double],[Double])] -> Double -> FNet -> IO ()
trainBatch samples s net = do
    gmForM samples $ \(input,targets) -> do
        resetNet net
        setInput input net
        fPropagate relu net
        setFirstDeltas targets net
        bPropagate relu' net
        updateDW net
        return ()
    updateWeights s net

readD :: Int -> FLayer -> IO Double
readD idx layer = do
        (_,_,d,_) <- VM.read layer idx
        return d

outputWS :: [Double] -> FNet -> IO [Double]
outputWS  input net = do
    resetNet net
    setInput input  net
    fPropagate relu net
    lastLayer <- VM.read net (VM.length net - 1)
    mapM (\idx -> readD idx lastLayer) [0..(VM.length lastLayer -1)]

output :: [Double] -> FNet -> IO [Double]
output input net = do
    resetNet net
    setInput input  net
    fPropagate relu net
    softmax net
    lastLayer <- VM.read net (VM.length net - 1)
    mapM (\idx -> readD idx lastLayer) [0..(VM.length lastLayer -1)]

printNet :: FNet -> IO ()
printNet net = do
    gmForM [0 .. (VM.length net -1)  ] $ \x -> do
        xs <- VM.read net x
        gmForM [0.. (VM.length xs -1)] $ \y -> do
            (a,b,c,d) <- VM.read xs y
            putStr $ show (a,b,c) Prelude.++ " "
        putStrLn ""

printNetWeights :: FNet -> IO ()
printNetWeights net = do
    gmForM [0 .. (VM.length net -1)  ] $ \x -> do
        xs <- VM.read net x
        gmForM [0.. (VM.length xs -1)] $ \y -> do
            (a,b,c,d) <- VM.read xs y
            putStr $ show (a,b,c) Prelude.++ " "
            gmForM [0.. VM.length d -1] $ \idx -> do
                (w,dw,y,x) <- VM.read d idx
                putStr $ show (w,dw,y,x)   Prelude.++ " "
            putStr "| "
        putStrLn ""


toNode ::  (Double,Double,Double, [(Double,Double,Int,Int)])  -> IO FNode
toNode (a,z,d,wl) = do
    wl <- vmFromList wl
    return (a,z,d,wl)

netFromList :: [[ (Double, Double, Double, [(Double,Double, Int, Int)]) ]]-> IO FNet
netFromList xs = do
    xs <- Prelude.mapM ( Prelude.mapM toNode) xs
    xs <- Prelude.mapM vmFromList xs
    vmFromList xs

nodeToRaw :: FNode -> IO (Double,Double,Double, [(Double,Double, Int, Int)])
nodeToRaw (a,b,c,ts) = do
    xs <- Prelude.mapM(\idx -> VM.read ts idx) [0..(VM.length ts -1)]
    return (a,b,c,xs)

netToList :: FNet -> IO [[ (Double, Double, Double, [(Double,Double, Int, Int)]) ]]
netToList net = do
    layers <- Prelude.mapM (\idx -> VM.read net idx) [0..(VM.length net -1)]
    nodes <- Prelude.mapM ( \layer -> Prelude.mapM (\idx -> VM.read layer idx) [0..VM.length layer -1 ]) layers
    Prelude.mapM (Prelude.mapM nodeToRaw) nodes



vector :: IO (MVector RealWorld Int)
vector = do
    vec <- VM.replicate 11 (11::Int)
    x <-  (VM.read vec 3)
    print x
    return vec

netVector :: IO (MVector RealWorld  (MVector RealWorld (MVector RealWorld Int)))
netVector = do
    entrys <- VM.replicate 10 (10::Int)
    xs <- VM.replicate 10 entrys
    VM.replicate 10 xs

printNetVector ::  MVector RealWorld  (MVector RealWorld (MVector RealWorld Int))-> IO ()
printNetVector vector = do
    gmForM [0..9] $ \i -> gmForM [0..9]  $  \j -> do
        gmForM [0..9] $ \k -> do
            x <- readNetVector vector i j k
            putStr $ show x Prelude.++ " "
        putStrLn ""

readNetVector vector i j k = do
    x0 <- VM.read vector i
    x1 <- VM.read x0  j
    VM.read x1 k

printAndwrite = do
    vector <- netVector
    overwriteNetVector vector
    printNetVector vector

overwriteNetVector vector = do
    gmForM [0..9] $ \i -> gmForM [0..9]  $  \j -> do
        gmForM [0..9] $ \k -> do
            writeNetVector vector i j k 30


writeNetVector vector i j k v = do
    x0 <- VM.read vector i
    x1 <- VM.read x0 j
    VM.write x1 k v


getFullLine :: IO String
getFullLine = getChar >>= (\c ->
           if c == '\n' then return []
           else getFullLine >>= (\cs -> return (c:cs)))

example = do
    v <- VM.replicate 100 Nothing      -- Char[] v = new Char[100];
    putStrLn (show (VM.length v))      -- System.out.println(v.length);
    VM.write v 10 (Just 'a')           -- v[10] = 'a';
    x <- VM.read v 10                  -- Int x = v[10];
    putStrLn (show x)                 -- System.out.println(v[10]);
    vec <- vector
    y <- VM.read vec 10
    print y

vmFromList  :: [a] -> IO (MVector GHC.Prim.RealWorld a)
vmFromList [] = VM.new 0
vmFromList  ls@(x:xs) = do
    vec <- VM.replicate (Prelude.length ls)  x
    vec <- copyFromList ls vec
    return vec

copyFromList ::  [a] -> MVector RealWorld a ->  IO (MVector RealWorld a)
copyFromList xs vector = copyFromList' 0 xs vector

copyFromList' :: Int -> [a] -> MVector RealWorld a ->  IO (MVector RealWorld a)
copyFromList' idx [] vector =  return vector
copyFromList' idx (x:xs) vector  = do
    vec <- write vector idx x
    copyFromList' (idx+1) xs vector

testBackpropagation :: IO ()
testBackpropagation = do
    net <- net
    resetNet net
    setInput [1,1] net
    fPropagate relu net
    setFirstDeltas [0,1] net
    bPropagate relu' net
    updateDW net
    updateWeights 1 net
    printNetWeights net

testTrainBatch :: IO ()
testTrainBatch = do
    random_net <- generate_image_net 28 28 5 10 50 40
    net <- netFromList $ toFastNet random_net
    print "Start"
    getCurrentTime >>= print
    let samples = Prelude.concat $ Prelude.replicate 1 [(Prelude.replicate 785 (1::Double),[0::Double,0,0,0,0,1,0,0,0,0]), (Prelude.concat (Prelude.replicate 392 [0.01::Double, 0.6] ) ++ [1],[1::Double,0,0,0,0,0,0,0,0,0])]
    gmForM [0..1000] $ \x -> trainBatch  samples 0.001 net
    getCurrentTime >>= print
    gmForM [0..1000] $ \x -> do
        output (Prelude.replicate 785 (1::Double)) net >>= print
        output (Prelude.concat (Prelude.replicate 392 [0.01::Double, 0.6] ) ++ [1]) net >>= print
    getCurrentTime >>= print
    --printNetWeights net

testList =[[(0::Double,0::Double,0::Double,[(1::Double,0::Double,1::Int,0::Int),(0.5,0,1,1)]),(0,0,0,[(-0.5,0,1,0),(1,0,1,1)])]
               ,[(0,0,0,[(0.5,0,1,0),(0.5,0,1,1)]),(0,0,0,[(-2,0,1,0),(1,0,1,1)])]
               ,[(0,0,0,[(1,0,1,0),(1,0,1,1)]),(0,0,0,[(2,0,1,0),(2,0,1,1)])]
               ,[(0,0,0,[]),(0,0,0,[])]
               ]

testNet = netFromList testList

testGradient :: IO ()
testGradient = do
    net <- testNet
    print "Set Input"
    setInput' [1,2] net
    printNet net
    print "fPropagate"
    fPropagate relu net
    printNet net
    print "Softmax"
    softmax net
    printNet net
    print "bPropagate"
    setFirstDeltas [1,1] net
    bPropagate relu' net
    printNet net
    print "DW"
    updateDW net
    printNetWeights net
    print "updateWeights"
    updateWeights 1 net
    printNetWeights net

--main :: IO ()
--main = testTrainBatch
