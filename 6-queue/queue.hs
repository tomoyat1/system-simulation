import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List

import System.Random

import Debug.Trace

import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Atom
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

type SeatState a = State (Array Int Int, Int) a
type ShopParams = (Int, Int)
type Case = (String, Int, Int, [Int])
type Result = (String, Int, Int, [Int])

constantRate :: Int -> Int -> [Int]
constantRate period rate = replicate 600 0
                          ++ (take 3600 $ cycle queuePattern)
                          ++ repeat 0
    where periodS = period * 60
          intervalS = (toRational periodS) / (toRational rate)
          queuePattern = 1 : replicate (truncate intervalS - 1) 0

randomRate :: Int -> Int -> IO [Int]
randomRate period rate = do
    let periodS = period * 60
    randomPattern <- randomInflowAtRate periodS rate
    return $ replicate 600 0
             ++ (take 3600 $ cycle randomPattern)
             ++ repeat 0

randomInflowAtRate :: Int -> Int -> IO [Int]
randomInflowAtRate periodS rate = do
    times <- fmap (sort . take rate . randomRs (0,periodS)) newStdGen
    return [ countElem x times | x <- [0..periodS]]

countElem :: Int -> [Int] -> Int
countElem a = length . filter (a ==)

case1 :: Case
case1 = ("constant", 10, 10, take (60 * 150) $ constantRate 10 10)

cases10Constant :: [Case]
cases10Constant = [("constant", 10, r, take (60 * 150) $ constantRate 10 r) | r <- [2,4..10]]

cases10Random :: IO [Case]
cases10Random = do
    inflowLists <- sequence [randomRate 10 r | r <- [2,4..10]]
    return $ zipWith makeCase [2,4..10] inflowLists
    where makeCase :: Int -> [Int] -> Case
          makeCase r is = ("random", 10, r, take (60 * 150) is)

case20Random :: IO Case
case20Random = do
    inflowList <- randomRate 10 20
    return ("random", 10, 20, take (60 * 150) inflowList)

doCase :: ShopParams -> Case -> Result
doCase params (label, period, rate, inflowList) =
    (label, period, rate, everyMinute $ sim params inflowList)

everyMinute :: [Int] -> [Int]
everyMinute xs = map snd $ filter isMinute $ zip [0..] xs
    where isMinute (s, v) = (s `mod` 60) == 0

sim :: ShopParams -> [Int] -> [Int]
sim (serviceTime, seats) inflowList = (`evalState` (listArray (0, seats) $ repeat 0, 0)) $ do
    forM inflowList (simStep serviceTime)

simStep :: Int -> Int -> SeatState Int
simStep serviceTime inflow = do 
    decrementTimer
    addToQueue inflow
    assignSeats serviceTime
    fmap snd get

decrementTimer :: SeatState ()
decrementTimer = modify $ \s -> (fmap decrement (fst s), snd s)
    where decrement i = if (i - 1) > 0 then i - 1 else 0

addToQueue :: Int -> SeatState ()
addToQueue inflow = modify $ \s -> (fst s, snd s + inflow)

assignSeats :: Int -> SeatState ()
assignSeats serviceTime = do
    seats <- fmap fst get
    queueLength <- fmap snd get
    let is = indices seats :: [Int]
        emptySeats = filter (\i -> (seats!i) == 0) is
        updates = zip emptySeats $ replicate queueLength serviceTime
        updatedSeats = seats//updates
        newQueueLength = forward queueLength $ length updates
    put (updatedSeats, newQueueLength)
    return ()

forward :: Int -> Int -> Int
forward ql u = if nql <= 0 then 0 else nql
    where nql = ql - u

main :: IO ()
main = do
    putStrLn "Constant rate at 17m service time, 9 seats"
    let constantResults = map (doCase $ (60 * 17, 9)) cases10Constant
    mapM_ (graph (17, 9)) constantResults

    putStrLn "Random rate at 17m service time, 9 seats"
    randomResults <- mapM (return . (doCase $ (60 * 17, 9))) =<< cases10Random
    mapM_ (graph (17, 9)) randomResults

    putStrLn "Random rate at 15m service time, 9 seats"
    randomResults <- mapM (return . (doCase $ (60 * 15, 9))) =<< cases10Random
    mapM_ (graph (15, 9)) randomResults

    putStrLn "Random rate at 20m service time, 9 seats"
    randomResults <- mapM (return . (doCase $ (60 * 20, 9))) =<< cases10Random
    mapM_ (graph (20, 9)) randomResults

    putStrLn "Random rate at 8.5m service time, 9 seats"
    randomResult <- fmap (doCase (30 * 17, 9)) case20Random
    graph (8, 9) randomResult

    putStrLn "Random rate at 17m service time, 18 seats"
    randomResult <- fmap (doCase (60 * 17, 18)) case20Random
    graph (17, 18) randomResult

graph :: ShopParams -> Case -> IO ()
graph (serviceTime, seats) (label, period, rate, resultList) = do
    let resultListLen = fromIntegral $ length resultList :: Integer
    let xs = [0..resultListLen] :: [Integer]
    let ys = map fromIntegral resultList :: [Integer]
    let points = zip xs ys
    putStr $ "Max " ++ label ++ "-" ++ show period ++ "-" ++ show rate ++ "-" ++ show serviceTime ++ "-" ++ show seats ++ ": "
    print $ maximum resultList
    plotPathStyle [Title (label ++ "-" ++ show period ++ "-" ++ show rate ++ "-" ++ show serviceTime ++ "-" ++ show seats),
                   terminal (PNG.cons (label ++ "-" ++ show period ++ "-" ++ show rate ++ "-" ++ show serviceTime ++ "-" ++ show seats ++ ".png"))]
                   (PlotStyle Steps (CustomStyle [])) points
