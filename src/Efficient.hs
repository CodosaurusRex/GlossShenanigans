module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Map as M (Map, fromList, toList, (!))
import Codec.Picture.Types
import Control.Monad.Primitive
import Control.Monad
import Graphics.Gloss.Juicy
import Control.Concurrent.Async
import Control.Concurrent.STM
import Pipes
import Pipes.RealTime
import System.Environment
import System.Directory
import Data.Ephys.OldMWL.Parse
import Data.Time.Clock
import Data.Monoid
import Data.Ephys.Spike
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS (drop)
import Codec.Picture.Bitmap
import Control.Applicative
import Data.Vector as V ((!))
import Codec.BMP

type TotalPlots = Map (Int, Int) Plot

type MutImage = MutableImage (PrimState IO) PixelRGBA8
type FrozImage = Image PixelRGBA8

data Plot = Plot { image:: MutImage
                 , id   :: Int       
                 } 
            
data World = World { totalPlots  :: TotalPlots
                   , spikeChan   :: TChan TrodeSpike
                   , time        :: Double
                   , currchanX   :: Int
                   , currchanY   :: Int
                   }

green'   = PixelRGBA8 255 0 255 0
blue'    = PixelRGBA8 0 255 255 0
red'     = PixelRGBA8 0 0 255 255
white'   = PixelRGBA8 255 255 255 255
yellow'  = PixelRGBA8 255 0 255 255
cyan'    = PixelRGBA8 255 255 255 0
magenta' = PixelRGBA8 0 255 255 255 


------------------------Initializing stuff-------------------------
listOfKeys :: [(Int, Int)]
listOfKeys = [(x,y) | x <- [0..3], y <- [0..3], x < y]

myMutable :: IO MutImage -- <Checked>
myMutable = createMutableImage 500 500 (PixelRGBA8 255 0 0 255) --creates a new Mutable Image that's 720 by 480 and is all black

myFrozen :: IO FrozImage
myFrozen = do
  mut <- myMutable
  freezeImage mut

initWorld :: TChan TrodeSpike ->TotalPlots -> World --initializes the world <Checked>
initWorld c ps = World ps c 4492 0 1

initPlots :: IO TotalPlots -- <Checked>
initPlots = do
  let listOfIOImages (x,y) = x `seq` y `seq` createMutableImage 700 700 (PixelRGBA8 0 (fromIntegral y) 255 (fromIntegral x))



        {-if length ls < 6 
                          then listOfIOImages$
                               createMutableImage 700 700 (PixelRGBA8 0 0 255 0):[]
                               createMutableImage 700 700 (PixelRGBA8 0 0 255 5):
                               createMutableImage 700 700 (PixelRGBA8 0 0 255 4):
                               createMutableImage 700 700 (PixelRGBA8 0 0 255 3):
                               createMutableImage 700 700 (PixelRGBA8 0 0 255 2):
                               createMutableImage 700 700 (PixelRGBA8 0 0 255 1):[]-}
                                                                                       
 --                         else ls --build a list of IO Mutable Images
      toPlot im num = Plot im num 
  lsIm <- forM listOfKeys listOfIOImages 
--sequence $ listOfIOImages [] --Turn that list of IO Mutable Images into a list of images
  let plots = zipWith toPlot lsIm ([0..5] :: [Int]) 
  return $ fromList (zip listOfKeys (plots)) -- zip the keys with mutable images and then turn them all into a map. Return the map in the IO monad.
      


-----------------------Conversion Stuff-------------------------------
imToPic :: MutImage -> IO Picture --this function has been <Checked>.
imToPic mutim = do
  im <- freezeImage mutim --changes MutableImage to Image
  let bString = encodeBitmap im
  return $ scale (1) (-1) $ bitmapOfByteString 700 700 (BS.drop (54) $ toStrict bString) False


toPointList :: TrodeSpike -> [Double] -- <Checked>
toPointList s = (realToFrac $ (V.!) (spikeAmplitudes s) 0):
                (realToFrac $ (V.!) (spikeAmplitudes s) 1):
                (realToFrac $ (V.!) (spikeAmplitudes s) 2):
                (realToFrac $ (V.!) (spikeAmplitudes s) 3):[]
scaleFac :: Double
scaleFac = 2000000
                

-------------------Gloss stuff-----------------------------

main :: IO()
main = do
  (fn:_) <- getArgs
  let d = InWindow "cool window" (700, 700) (0,0)
  t0 <- getCurrentTime
  c <- newTChanIO
  _ <- async. runEffect $ produceTrodeSpikesFromFile fn 16
       >-> relativeTimeCat (\s -> spikeTime s - 4492)
       >-> cToTChan c
  plots <- initPlots
  image <- myFrozen 
  playIO d blue 300 (initWorld c plots) (drawWorld) handleInp $ step t0

drawWorld :: World -> IO Picture --changes from world to actual picture
drawWorld (World plots c _ chanx chany) = do
  let currPlot = (M.!) plots (chanx, chany)
      im       = image currPlot
  --putStrLn $ "drawing to" ++ (show (chanx, chany))
  imToPic (im)  

----------------------update stuff------------------------------


step :: UTCTime -> Float -> World -> IO World
step t0 _ w = do
  tNext <- getExperimentTime t0 4492
  let c = spikeChan w
  spike <- atomically $ flushChan c
  updateBMPs (totalPlots w) (map toPointList spike)
  return w { time = tNext}

updateBMPs :: TotalPlots -> [[Double]] -> IO ()
updateBMPs plots updatels = do
  let ls = toList plots
  mapM_ (indivPlots (concat updatels)) ls


indivPlots :: [Double] -> ((Int, Int), Plot) -> IO () --not a problem here
indivPlots ls ((chanx, chany), (plot@(Plot _ id))) = do
  let mutIm = image plot
  dimPlot mutIm
  if ((length ls) == 0)
    then return ()
    else do
    let x     = ceiling $ scaleFac * (!!) ls chanx
        y     = ceiling $ scaleFac * (!!) ls chany
    --putStrLn $ "updating to" ++ (show (chanx, chany)) ++ (show (x,y))
    if ((x > 0) && (y > 0))
      then do
           writePixel mutIm x y color
           indivPlots (drop 4 ls) ((chanx, chany), plot)
      else indivPlots (drop 4 ls) ((chanx, chany), plot)
  where color = case id of 0 -> green'
                           1 -> blue'
                           2 -> red'
                           3 -> magenta'
                           4 -> yellow'
                           5 -> cyan'
                           
--------------------------------TChan stuff--------------------
                
cToTChan :: TChan TrodeSpike -> Consumer TrodeSpike IO() --consumer that forever writes the spikes to the tChan
cToTChan c =  forever $ (await >>= lift . atomically . writeTChan c)




flushChan :: TChan TrodeSpike -> STM [TrodeSpike]
flushChan c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
             e <- readTChan c
             go (e:acc)


----------------------------time stuff--------------------------------

getExperimentTime :: UTCTime -> Double -> IO Double
getExperimentTime t0 et0 =
  (et0 +) . realToFrac . flip diffUTCTime t0 <$> getCurrentTime



-----------------------Input stuff---------------

handleInp :: Event -> World -> IO World
handleInp (EventKey (MouseButton b) Up _ _) w@(World _ _ _ x y)
{-
  | b == LeftButton = 
    return $ w {currchanX = (currchanX w + 1) `mod` 4}
  | b == RightButton =
      return $ w { currchanY = (currchanY w + 1) `mod` 4}
-}
  | b == LeftButton = do
    let (newX, newY) = nextChannel x y
    return $ w {currchanX = newX, currchanY = newY}
  | otherwise = return w
handleInp _ w = return w


nextChannel :: Int -> Int -> (Int, Int)
nextChannel x y= case (x,y) of (0,1) -> (0,2)
                               (0,2) -> (0,3)
                               (0,3) -> (1,2)
                               (1,2) -> (1,3)
                               (1,3) -> (2,3)
                               (2,3) -> (0,1)
                               otherwise -> (0,1)
  
-------------------------ignore past here--------------------------


printWorld :: World -> IO ()
printWorld  w = print $ (currchanX w, currchanY w)


dimPlot :: MutImage -> IO ()
dimPlot im  = return ()



indivPlots' :: [Double] -> ((Int, Int), Plot) -> IO ()
indivPlots' ls ((x,y), Plot image id) = do
  let mutIm= image
  sequence_ $ map (pixelWriter mutIm x y) ls

pixelWriter :: MutImage -> Int -> Int -> Double -> IO () 
pixelWriter im x1 x2 y = do
  writePixel im (100*((x1 * x2) + x2)) (ceiling $ scaleFac * y) (PixelRGBA8 255 255 255 255) 
