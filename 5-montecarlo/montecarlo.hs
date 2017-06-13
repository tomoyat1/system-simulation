import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Data.Array
import Data.List
import System.Random
import System.Random.Mersenne.Pure64

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.MultiPlot as MultiPlot
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Data.Time as Time
import Control.Monad (liftM2, )
import Data.Array (listArray, )
import Data.Foldable (foldMap, )
import Data.Monoid (mappend, mconcat, )

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = 1 >= x ** 2 + y ** 2

randomList :: Double -> Double -> IO [Double]
randomList b t = fmap (randomRs (b, t)) newStdGen

randomMTList :: Double -> Double -> IO [Double]
randomMTList b t = fmap (randomRs (b, t)) newPureMT

dotsToPi :: (Integral a) => a -> a -> Double
dotsToPi t d = 4 * (fromIntegral d) / (fromIntegral t)

-- Unit: cm
-- origin: edge of right wiper
inWiper :: (Double, Double) -> Bool
inWiper (x, y) = inLeft && inRight
    where line = sqrt 3 * x - y - 10 * sqrt 3
          inLeft = 50 ** 2 >= (x - 10) ** 2 + y ** 2 && 0 >= line
          inRight = 50 ** 2 >= (x - 50) ** 2 + y ** 2

dotsToWiperArea :: (Integral a) => a -> a -> Double
dotsToWiperArea t d = 50^^2 * (fromIntegral d) / (fromIntegral t)

monteCarloPi :: (Integral a) => String ->
    (Double -> Double -> IO [Double]) ->
    ((Double, Double) -> Bool) ->
    (Double, Double) ->
    Int ->
    IO a
monteCarloPi name listGen judge (a, b) times = do
    putStrLn ("Calculating " ++ name ++ ": " ++ show times ++ " dots")
    doubleXList <- listGen a b
    doubleYList <- listGen a b
    let doubleList =  zip doubleXList doubleYList
    dotGraph name judge times doubleList
    evaluate $ foldl tester 0 $ take times doubleList
    where tester a t
            | judge t = a + 1
            | otherwise = a

monteCarloPiLCG :: (Integral a) => String -> ((Double, Double) -> Bool) -> (Double, Double) -> Int -> IO a
monteCarloPiLCG name judge (a, b) = monteCarloPi (name ++ "-lcg") randomList judge (a, b)

monteCarloPiMT :: (Integral a) => String -> ((Double, Double) -> Bool) -> (Double, Double) -> Int -> IO a
monteCarloPiMT name judge (a, b) = monteCarloPi (name ++ "-mt") randomMTList judge (a, b)

dotGraph :: String -> ((Double, Double) -> Bool) -> Int -> [(Double, Double)] -> IO ()
dotGraph name judge times dots = do
    when (times <= 10^4) $ do
        let fileName = "./" ++ name ++ "-" ++ show times ++ ".png"
        putStrLn ("Rendering " ++ fileName)
        GP.plot (PNG.cons (fileName)) $
            Frame.cons (Opts.key False $ Opts.sizeRatio 1.0 Opts.deflt) $ mconcat $
                Plot2D.list Graph2D.points valid :
                Plot2D.list Graph2D.points invalid :
                []
        return ()
    return ()
        where takenDots = take times dots
              valid = filter judge takenDots
              invalid = takenDots \\ valid

testCases :: [Int]
testCases = [10^3, 10^4, 10^5, 10^6, 10^7, 10^8]

main :: IO ()
main = do
    putStrLn "pi lcg"
    piLcgRes <- mapM (monteCarloPiLCG "pi" inCircle (0, 1)) testCases
    let pisLcg = zipWith dotsToPi testCases piLcgRes
    forM_ pisLcg print

    putStrLn "wiper lcg"
    wiperLcgRes <- mapM (monteCarloPiLCG "wiper" inWiper (0, 50)) testCases
    let wipersLcg = zipWith dotsToWiperArea testCases wiperLcgRes
    forM_ wipersLcg print

    putStrLn "pi mt"
    piMTRes <- mapM (monteCarloPiMT "pi" inCircle (0, 1)) testCases
    let pisMt = zipWith dotsToPi testCases piMTRes
    forM_ pisMt print

    putStrLn "wiper mt"
    wiperMTRes <-  mapM (monteCarloPiMT "wiper" inWiper (0, 50)) testCases
    let wipersMt = zipWith dotsToWiperArea testCases wiperMTRes
    forM_ wipersMt print

    putStrLn "pi lcg: avg of 10 10^6"
    resList <- sequence $ take 10 $ repeat newEmptyMVar
    forM_ [0..9] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiLCG "pi" inCircle (0, 1) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToPi (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "pi mt: avg of 10 10^6"
    resList <- sequence $ take 10 $ repeat newEmptyMVar
    forM_ [0..9] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiMT "pi" inCircle (0, 1) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToPi (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "wiper lcg: avg of 10 10^6"
    resList <- sequence $ take 10 $ repeat newEmptyMVar
    forM_ [0..9] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiLCG "wiper" inWiper (0, 50) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToWiperArea (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "wiper mt: avg of 10 10^6"
    resList <- sequence $ take 10 $ repeat newEmptyMVar
    forM_ [0..9] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiMT "wiper" inWiper (0, 50) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToWiperArea (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "pi lcg: avg of 100 10^6"
    resList <- sequence $ take 100 $ repeat newEmptyMVar
    forM_ [0..99] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiLCG "pi" inCircle (0, 1) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToPi (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "pi mt: avg of 100 10^6"
    resList <- sequence $ take 100 $ repeat newEmptyMVar
    forM_ [0..99] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiMT "pi" inCircle (0, 1) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToPi (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "wiper lcg: avg of 100 10^6"
    resList <- sequence $ take 100 $ repeat newEmptyMVar
    forM_ [0..99] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiLCG "wiper" inWiper (0, 50) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToWiperArea (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg

    putStrLn "wiper mt: avg of 100 10^6"
    resList <- sequence $ take 100 $ repeat newEmptyMVar
    forM_ [0..99] (\i -> forkIO $ do
        putStrLn $ "Thread: " ++ show i
        res <- monteCarloPiMT "wiper" inWiper (0, 50) (10^6)
        evaluate res
        putMVar (resList!!i) (dotsToWiperArea (10^6) res))
        
    results <- sequence $ map readMVar resList
    let avg = (foldl (+) 0 results) / (fromIntegral $ length results)
    print avg
