module Config
( generateConfig
, readConfig
,Config (..)
) where

import           Backpropagation
import           Data.List.Split
import           System.Directory

data Config = Config { path                       :: String
                                    , filename    :: String
                                    , filepath    :: String
                                    , imgHeight   :: Int
                                    , imgWidth    :: Int
                                    , nrLayers    :: Int
                                    , nrClasses   :: Int
                                    , nrNeurons   :: Int
                                    , nrCons      :: Int
                                    ,steps        :: Int
                                    ,batchSize    :: Int
                                    ,learningRate :: Double
} deriving Show

generateConfig :: IO ()
generateConfig = do
  height <- readInt "Image Height: "
  width <- readInt  "Image Width: "
  layers <- readInt "Number Layers : "
  classes <- readInt "Number Classes: "
  nrNeurons <- readInt "Number Neurons: "
  nrCons <- readInt "Number Connections of Neurons: "
  steps <- readInt "Steps between saving: "
  batchSize <- readInt "Batchsize: "
  learningRate <- readDouble "Learningrate: "

  let filename = "image_net_" ++ show height  ++ "_" ++ show width ++ "_" ++ show layers ++ "_" ++ show classes ++ "_" ++ show nrNeurons ++ "_" ++ show nrCons
  let path = "save/" ++ filename ++"/"
  let filepath = "save/" ++ filename ++"/" ++ "image_net"
  createDirectory path
  let config = Config path filename filepath height width layers classes nrNeurons nrCons steps batchSize learningRate
  writeConfig config
  print config

readInt :: String -> IO Int
readInt prompt = do
  putStr prompt
  nr <- getLine
  let r = read nr :: Int
  return  r

readDouble :: String -> IO Double
readDouble prompt = do
  putStr prompt
  nr <- getLine
  let r = read nr :: Double
  return  r

writeConfig :: Config -> IO ()
writeConfig config = do
  let filePath = path config ++ "/config.txt"
  writeFile filePath $ "Height " ++ show (imgHeight config) ++ "\n"
  appendFile filePath $ "Width " ++ show (imgWidth config) ++ "\n"
  appendFile filePath $ "Layers " ++ show (nrLayers config) ++ "\n"
  appendFile filePath $ "Classes " ++ show (nrClasses config) ++ "\n"
  appendFile filePath$ "Neurons " ++ show (nrNeurons config) ++ "\n"
  appendFile filePath $ "Connections " ++ show (nrCons config) ++ "\n"
  appendFile filePath $ "SavingIntervall " ++ show (steps config) ++ "\n"
  appendFile filePath $ "Batchsize " ++ show (batchSize config) ++ "\n"
  appendFile filePath $ "LearningRate " ++ show (learningRate config) ++ "\n"

readConfig :: String -> IO Config
readConfig filename = do
    let path = "save/" ++ filename
    let filePath = "save/" ++ filename ++ "/image_net"
    file <- readFile $ path ++ "/config.txt"
    let xs = map( (\xs -> (\ys -> (head ys, last ys)) . (splitOn " ")) xs) $ lines file
    let findEntry name =snd $ head $ filter ( ( == name) . fst) xs
    let readValueI name = read (findEntry name) :: Int
    let readValueD name =  read (findEntry name) :: Double
    let height = readValueI "Height"
    let width = readValueI "Width"
    let layers = readValueI "Layers"
    let classes = readValueI "Classes"
    let neurons = readValueI "Neurons"
    let cons = readValueI "Connections"
    let steps = readValueI "SavingIntervall"
    let batchSize = readValueI "Batchsize"
    let learningRate = readValueD "LearningRate"
    return $ Config path filename filePath height width layers classes neurons cons steps batchSize learningRate
