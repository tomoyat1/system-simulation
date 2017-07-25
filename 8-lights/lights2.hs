{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Data.Array
import Data.IORef
import System.Environment
import System.Random

import Debug.Trace

type Light = (Double, Double, Double)
type Desk = (Double, Double, Double)

lights :: [Double] -> [Light]
lights brightness = zipWith ($) ((,,) <$> [0, 1.8, 3.6, 5.4] <*> [0, 1.8, 3.6]) brightness

desks :: [Desk]
desks = zipWith ($) ((,,) <$> [1, 2.2, 3.4] <*> [7.5, 14.5]) [700, 500, 300, 900, 700, 500]

illuminance :: [Light] -> Desk -> Double
illuminance lights desk = foldl addIlluminancePerLight 0 lights
    where addIlluminancePerLight :: Double -> Light -> Double
          addIlluminancePerLight a l = a + illuminancePerLight desk l

illuminancePerLight :: Desk -> Light -> Double
illuminancePerLight (xd, yd, id) (xl, yl, il) = cosA * il / dist
    where dist = 2.0 ** 2 + (xl - xd) ** 2 + (yl - yd) ** 2
          cosA = sqrt dist / 2.0

totalIlluminance :: [Light] -> Double
totalIlluminance = foldl (\a (x,y,z) -> a + z) 0

meanSquaredError :: [(Double, Double)] -> Double
meanSquaredError input = squaredSum / (fromIntegral . length) input
    where squaredSum = foldl (\a (x, c) -> a + (x - c) ** 2) 0 input

meanSquaredErrorEnergy :: [Light] -> Double
meanSquaredErrorEnergy = meanSquaredError . simCase

meanSquaredAndTotalIlluminanceEnergy :: [Light] -> Double
meanSquaredAndTotalIlluminanceEnergy lights = meanSquaredErrorEnergy lights + totalIlluminance lights

simCase :: [Light] -> [(Double, Double)]
simCase lights = zip (map (illuminance lights ) desks) (map (\(a, b, c) -> c) desks)

neighbourState :: [Light] -> IO [Light]
neighbourState state = do
    illuminances <- neighbourIlluminances $ map (\(a, b, c) -> c) state :: IO [Double]
    let coords :: [Double -> Light]
        coords = map (\(a, b, c) -> (,,) a b) state
    return $ zipWith ($) coords illuminances

neighbourIlluminances :: [Double] -> IO [Double]
neighbourIlluminances illuminances = do
    (index, _) <- randomR (0,length illuminances - 1) <$> newStdGen
    (willAdd :: Bool, _) <- random <$> newStdGen
    let op = if willAdd then (1+) else (subtract 1)
    let (front, end) = splitAt index illuminances
    let newIlluminance
            | op (head end) < 0 = 0
            | otherwise = op (head end)
    return $ front ++ [newIlluminance] ++ tail end

simulatedAnnealer :: [Light]
                  -> [Light]
                  -> ([Light] -> Double)
                  -> ([Light] -> IO [Light])
                  -> Double
                  -> (Double, Double)
                  -> IO [Light]
simulatedAnnealer curState bestState energy pertube limit (initTemp, alpha) = do
    curStateR <- newIORef curState
    bestStateR <- newIORef bestState
    doIt curStateR bestStateR 0
    where doIt curStateR bestStateR cnt =
            if cnt == limit then
                readIORef bestStateR
            else do
                {-
                when (floor cnt `quot` 100000 == 0)
                    (print =<< readIORef curStateR)
                -}
                when (floor cnt `mod` 10000 == 0)
                    (print =<< readIORef bestStateR)
                curState <- readIORef curStateR
                nextState <- pertube curState
                let curE = energy curState
                    nextE = energy nextState
                    bestE = energy bestState
                let newBestState = if nextE < bestE then
                        nextState
                    else
                        bestState
                writeIORef bestStateR newBestState
                (randomDouble :: Double, _) <- randomR (0, 1) <$> newStdGen
                when (floor cnt `mod` 10000 == 0)
                    (print "diff" >> (print $ curE - nextE))
                when (floor cnt `mod` 10000 == 0)
                    (print "P" >> (print $ probability curE nextE temp))
                let newCurState = if randomDouble <= probability curE nextE temp then
                        nextState
                    else
                        curState
                writeIORef curStateR newCurState
                {-
                when (floor cnt `mod` (floor limit `quot` 100000) == 0)
                    (do
                        print =<< readIORef bestStateR
                        print temp)
                    -}
                doIt curStateR bestStateR (cnt + 1)
                    where probability :: Double -> Double -> Double -> Double
                          probability ce ne t = if ne <= ce then 1 else exp ((ce - ne) / temp)
                          temp = initTemp * (alpha ** (cnt / limit))


-- Does not finish in realistic time
{-
res = map (\l -> map (illuminance l) desks) (map lights allBrightnesses)
resFiltered = filter filterFunc res
    where filterFunc :: [Double] -> Bool
          filterFunc illuminances = foldl (&&) True (validity illuminances)
          validity :: [Double] -> [Bool]
          validity illuminances = zipWith (\x y -> (x - y) ** 2 <= 250) illuminances $ map (\(a, b, c) -> c) desks
-}

main :: IO ()
main = do
    args <- getArgs
    let mode = read (head args) :: Int
    let initState = lights $ replicate 12 1000
        tries = 10^6
    {-
        res1
        exp (-349 / temp) == 0.99
        -349 / temp == log 0.99
        temp = 349 / -log 0.99
        temp = 34725
    -}
    {-
        res2
        exp (-350 / temp) == 0.99
        -349 / temp == log 0.99
        temp = 350 / -log 0.99
        temp = 34824
    -}
    -- 十分に高ければ良い
    print mode
    let r
            | mode == 0 = simulatedAnnealer initState initState meanSquaredErrorEnergy neighbourState tries (20000, 10**(-6) / 20000)
            | otherwise = simulatedAnnealer initState initState meanSquaredAndTotalIlluminanceEnergy neighbourState tries (20000, 10**(-6) / 20000)
    res <- r
    print res
    print $ meanSquaredErrorEnergy res
    print $ totalIlluminance res
    print $ map (uncurry (-)) (simCase res)

