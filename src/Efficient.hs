{-# LANGUAGE TemplateHaskell      #-}

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
import Data.Binary
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Typeable
import qualified Control.Concurrent as C
import qualified Pipes.Prelude as PP


type TempData = TrodeSpike

type TotalPlots = Map (Int, Int) Plot

type MutImage = MutableImage (PrimState IO) PixelRGBA8
type FrozImage = Image PixelRGBA8

data Plot = Plot { image:: MutImage
                 , id   :: Int       
                 } 
            
data World = World { totalPlots  :: TotalPlots
                   , spikeChan   :: TChan TempData --TrodeSpike
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



getRandomNumTup :: IO [Double]
getRandomNumTup = do
  a <-getStdRandom (randomR (0, 699))
  b <-getStdRandom (randomR (0, 699))
  c <-getStdRandom (randomR (0, 699))
  d <-getStdRandom (randomR (0, 699))
  return [a,b,c,d]

------------------------Initializing stuff-------------------------
listOfKeys :: [(Int, Int)]
listOfKeys = [(x,y) | x <- [0..3], y <- [0..3], x < y]

myMutable :: IO MutImage -- <Checked>
myMutable = createMutableImage 500 500 (PixelRGBA8 255 0 0 255) --creates a new Mutable Image that's 720 by 480 and is all black

myFrozen :: IO FrozImage
myFrozen = do
  mut <- myMutable
  freezeImage mut

initWorld :: TChan TempData {-TrodeSpike-} ->TotalPlots -> World --initializes the world <Checked>
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
toPointList s = (realToFrac $ (V.! ) (spikeAmplitudes s) 0):
                (realToFrac $ (V.!) (spikeAmplitudes s) 1):
                (realToFrac $ (V.!) (spikeAmplitudes s) 2):
                (realToFrac $ (V.!) (spikeAmplitudes s) 3):[]



toPointList' :: TrodeSpike -> [Double]
toPointList' s = [1,2,3,4]


scaleFac :: Double
scaleFac = 2e6
                

-------------------Gloss stuff-----------------------------

initGloss :: TChan TempData -> IO()
initGloss c = do
  --(fn:_) <- getArgs
  
  --c <- newTChanIO
  --_ <- async. runEffect $ produceTrodeSpikesFromFile fn 16
    --   >-> relativeTimeCat (\s -> spikeTime s - 4492)
      -- >-> cToTChan c-}

  t0 <- getCurrentTime
  plots <- initPlots
  let d = InWindow "cool window" (700, 700) (0,0)
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
  let c = spikeChan w --do not delete pls. needed.
  emp <- (atomically $ isEmptyTChan c)
  if (emp == False) 
    then do 
      spike <- atomically $ flushChan c
      updateBMPs (totalPlots w) (map toPointList spike)
      return w { time = tNext}
    else return w {time = tNext}

{-
step' :: UTCTime -> Float -> World -> IO World
step' t0 _ w = do
  tNext <- getExperimentTime t0 4492
  let c = spikeChan w
  spike <- atomically $ flushChan' c
  updateBMPs (totalPlots w) spike
  return w { time = tNext}
-}

updateBMPs :: TotalPlots -> [[Double]] -> IO ()
updateBMPs plots updatels = do
  let ls = toList plots
  mapM_ (indivPlots (concat updatels)) ls


indivPlots :: [Double] -> ((Int, Int), Plot) -> IO () --not a problem here
indivPlots ls ((chanx, chany), (plot@(Plot _ id))) = do
  let mutIm = image plot
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
cToTChan c =  forever $ do
                --liftIO $ putStrLn "adding"
                a <-await
                --liftIO $ putStrLn "done awaiting"
                liftIO . atomically $ writeTChan c a
                --liftIO $ putStrLn "added"

flushChan :: TChan TrodeSpike -> STM [TrodeSpike]
flushChan c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
             e <- readTChan c
             go (e:acc)
{-
flushChan' :: TChan TempData -> STM [[Double]]
flushChan' c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
             e <- readTChan c
             go (e:acc)  
-}

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
  
-----------------------------cloud stuff--------------------------------

sender :: Process ()
sender = do
  liftIO $ putStrLn "expecting sendport"
  s <- expect :: Process (SendPort TempData)
  liftIO $ putStrLn "got a sendport"
  c <- liftIO $ newTChanIO
  liftIO $ C.forkIO $ runEffect $ produceTrodeSpikesFromFile "/home/chennosaurus/1628.tt" 16 
       >-> relativeTimeCat (\s -> spikeTime s -  4492) --4491)
       >-> cToTChan c
  liftIO $ putStrLn "past the cToTChan"
  forever $ do
           --liftIO $ putStrLn "about to send dat"
           sendDat s c
  
sendDat :: SendPort TempData -> TChan TrodeSpike -> Process () 
sendDat s c= do
  emp <- inp $ isEmptyTChan c
  if emp
     then do
        --liftIO $ putStrLn "empty tchan"
       return ()
     else do
       e <- inp $ readTChan c
       sendChan s e
       sendDat s c
  where inp = \a -> liftIO.atomically $ a

receiver :: ProcessId -> Process()
receiver id = do
  (sendCh, receiveCh) <- newChan :: Process (SendPort (TempData), ReceivePort (TempData))
  send id sendCh
  liftIO $ putStrLn "sent port"
  c <- liftIO $ newTChanIO
  receiveData c receiveCh
  liftIO $ C.forkIO $ initGloss c
  forever $ do
    receiveData c receiveCh

receiveData :: TChan TempData -> ReceivePort (TempData) -> Process ()
receiveData c port = do
  dat <- receiveChan port
  liftIO.atomically $ writeTChan c dat
  --DISPLAYPLOTS

remotable ['sender, 'receiver]

master :: [NodeId] -> Process ()
master [] = liftIO $ putStrLn "no slaves"
master [generator,displayer] = do
  a <- spawn  generator $(mkStaticClosure 'sender)
  liftIO $ putStrLn "done with send going on to display"
  b <- spawn displayer $ $(mkClosure ('receiver)) a
  return ()



main :: IO () 
main = do
  [serverType, port] <- getArgs 
  case serverType of
    "master" -> do  
       backend <- initializeBackend "localhost" port rtable
       startMaster backend master
    "dotplot" -> do
       backend <- initializeBackend "localhost" port rtable
       startSlave backend
    "getSpikes" -> do
       backend <- initializeBackend "localhost" port rtable
       startSlave backend
   where
     rtable :: RemoteTable
     rtable = __remoteTable initRemoteTable



-----------------------------useless stuff for final----------

---------------------------utils-----------------------------

