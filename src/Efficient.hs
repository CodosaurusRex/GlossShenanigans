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
import Codec.Picture.Bitmap
import Control.Applicative
import Data.Vector as V ((!))

type TotalPlots = Map (Int, Int) Plot

type MutImage = MutableImage (PrimState IO) PixelRGBA8

data Plot = Plot { image     :: MutImage
                 }
            
data World = World { totalPlots  :: TotalPlots
                   , spikeChan   :: TChan TrodeSpike
                   , time        :: Double
                   , currchanX   :: Int
                   , currchanY   :: Int
                   }

------------------------Initializing stuff-------------------------
listOfKeys :: [(Int, Int)]
listOfKeys = [(x,y) | x <- [0..3], y <- [0..3], x < y]

myMutable :: IO MutImage -- <Checked>
myMutable = createMutableImage 500 500 (PixelRGBA8 0 0 0 0) --creates a new Mutable Image that's 720 by 480 and is all black

initWorld :: TChan TrodeSpike ->TotalPlots -> World --initializes the world <Checked>
initWorld c ps = World ps c 4492 0 1

initPlots :: IO TotalPlots -- <Checked>
initPlots = do
  let listOfIOImages ls = if length ls < 6 
                          then listOfIOImages $ createMutableImage 500 500 (PixelRGBA8 0 0 0 0):ls
                          else ls --build a list of IO Mutable Images
      toPlot im = Plot im 
  lsIm <- sequence $ listOfIOImages [] --Turn that list of IO Mutable Images into a list of images
  let plots = map toPlot lsIm
  return $ fromList (zip listOfKeys plots) -- zip the keys with mutable images and then turn them all into a map. Return the map in the IO monad.
      


-----------------------Conversion Stuff-------------------------------

imToPic :: MutImage -> IO Picture --this function has been <Checked>.
imToPic mutim = do
  im <- freezeImage mutim --changes MutableImage to Image
  return $ bitmapOfByteString 500 500 (toStrict $ encodeBitmap im) False --changes Image to picture
  
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
  let d = InWindow "cool window" (500, 500) (0,0)
  t0 <- getCurrentTime
  c <- newTChanIO
  _ <- async. runEffect $ produceTrodeSpikesFromFile fn 16
       >-> relativeTimeCat (\s -> spikeTime s - 4492)
       >-> cToTChan c
  plots <- initPlots
  playIO d black 30 (initWorld c plots) (drawWorld) handleInp $ step t0

drawWorld :: World -> IO Picture --changes from world to actual picture
drawWorld (World plots c _ chanx chany) = do
  let currPlot = (M.!) plots (chanx, chany)
      im       = image currPlot
  imToPic im
  
  
  

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
  mapM_ (indivPlots (listOfListsToList updatels)) ls

indivPlots :: [Double] -> ((Int, Int), Plot) -> IO ()
indivPlots ls ((chanx, chany), plot) = do
  let mutIm = image plot
  if ((length ls) == 0)
    then return ()
    else do
    let x     = ceiling $ scaleFac * (!!) ls chanx
        y     = ceiling $ scaleFac * (!!) ls chany
    writePixel mutIm x y (PixelRGBA8 255 255 255 255)
    print x
    print y
    indivPlots (drop 4 ls) ((chanx, chany), plot)

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
handleInp _ w = return w


---------------------helper funcs-------------------------------

listOfListsToList :: [[Double]]-> [Double]
{-listOfListsToList (x:xs) list = if length xs == 0
                              then list
                              else listOfListsToList xs (x ++ list)
-}

listOfListsToList ls = concat ls
