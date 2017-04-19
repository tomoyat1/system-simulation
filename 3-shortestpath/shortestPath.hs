import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Atom
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import Data.List
import Data.Function
data Parameters = Parameters { startToRiver :: Double
                             , riverWidth :: Double
                             , riverToEnd :: Double
                             , courseLength :: Double
                             , current :: Double
                             , landSpeed :: Double
                             , waterSpeed :: Double } deriving (Show)
case0 :: Parameters
case0 = Parameters 100 150 50 400 0 1 6
case1 :: Parameters
case1 = Parameters 100 150 50 400 0.8 1 6
case2 :: Parameters
case2 = Parameters 100 150 50 400 4 1 6

range :: [Double]
range = [0,1..400]

timeWithCurrent :: Parameters -> Double -> Double -> Double
timeWithCurrent params p q = landTime1 + riverTimeWithCurrent + landTime2
    where landTime1 = sqrt (startToRiver params^^2
                           + p^^2) / landSpeed params
          landTime2 = sqrt (riverToEnd params^^2
                           + (courseLength params - q)^^2) / landSpeed params
          riverTimeWithCurrent = sqrt (riverWidth params^^2
                           + (q - p)^^2) / speed
            where rad = atan $ (q - p) / riverWidth params
                  speed = waterSpeed params + current params * sin rad

-- TODO: Generalize, rename, or get rid of completely
minAndArgs2 :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
minAndArgs2 (oldP, oldQ, oldRes) (newP, newQ, newRes)
    | oldRes <= newRes = (oldP, oldQ, oldRes)
    | otherwise = (newP, newQ, newRes)

-- TODO: Rename
combinations :: (Double -> Double -> Double)
    -> [Double]
    -> [Double]
    -> [(Double, Double, Double)]
combinations func p q = tuple <$> p <*> q
    where tuple p q = (p, q, func p q)

printMin :: (Double, Double, Double) -> IO ()
printMin (p, q, res) = putStrLn $ "p: " ++ show p ++ " q: " ++ show q ++ " time: " ++ show res

printParams :: Parameters -> IO ()
printParams params = putStrLn $ "スタート->川の距離: " ++ show (startToRiver params)
                             ++ " 川の幅: " ++ show (riverWidth params)
                             ++ " 川->ゴールの距離: " ++ show (riverToEnd params)
                             ++ " 全体の長さ: " ++ show (courseLength params)
                             ++ " 水流の速さ: " ++ show (current params)
                             ++ " 陸の移動速度: " ++ show (landSpeed params)
                             ++ " 水上の移動速度: " ++ show (waterSpeed params)

-- TODO: create printParams :: Parameters -> IO ()
main :: IO ()
main = do
    printParams (cases!!0)
    printMin $ foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent (cases!!0)) range range
    printParams (cases!!1)
    printMin $ foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent (cases!!1)) range range
    printParams (cases!!2)
    printMin $ foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent (cases!!2)) range range
    printParams (cases!!3)
    printMin $ foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent (cases!!3)) range range
    printParams (cases!!4)
    printMin $ foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent (cases!!4)) range range
    printParams (cases!!5)
    printMin $ foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent (cases!!5)) range range

    putStrLn "graph for case 0"
    plot [00,1..70::Double] [360,361..400::Double] 0 "Case 0: 6.0 m/s current"
    putStrLn "graph for case 1"
    plot [00,1..70::Double] [360,361..400::Double] 1 "Case 1: 4.0 m/s current"
    putStrLn "graph for case 2"
    plot [00,1..70::Double] [360,361..400::Double] 2 "Case 2: 0.8 m/s current"
    putStrLn "graph for case 3"
    plot [00,1..70::Double] [360,361..400::Double] 3 "Case 3: 0 m/s current"
    putStrLn "graph for case 4"
    plot [00,1..70::Double] [360,361..400::Double] 4 "Case 4: -0.8 m/s current"
    putStrLn "graph for case 5"
    plot [00,1..70::Double] [360,361..400::Double] 5 "Case 5: -4.0 m/s current"

    -- Doesn't work
    {-
    plotMesh3d [terminal (PNG.cons ("./current.png"))] [(Plot3dType Surface)]
        $ groupBy ((==) `on` \(x,_,_) -> x) (sort [(a,b,v) | (a,b,_) <- [foldl minAndArgs2 (1000,1000,1000) $ combinations (timeWithCurrent c) range range | c <- map (\x -> Parameters 100 150 50 400 x 1 6) [0,0.1..6] ], v <- [0,0.1..6 :: Double]])
    -}

timeWithoutCurrent :: Parameters -> Double -> Double -> Double
timeWithoutCurrent params p q = landTime1 + riverTime + landTime2
    where landTime1 = sqrt (startToRiver params^^2
                           + p^^2) / landSpeed params
          riverTime = sqrt (riverWidth params^^2
                           + (q - p)^^2) / waterSpeed params
          landTime2 = sqrt (riverToEnd params^^2
                           + (courseLength params - q)^^2) / landSpeed params

plot :: [Double] -> [Double] -> Int -> String -> IO ()
plot pRange qRange c title = do
    plotMesh3d [(Title title), terminal (PNG.cons ("./case" ++ show c ++ ".png"))] []
        (do x <- pRange; return (do y <- qRange; return (x,y,timeWithCurrent (cases!!c) x y)))

cases :: [Parameters]
cases = [ Parameters 100 150 50 400 6.0 1 6
        , Parameters 100 150 50 400 4 1 6
        , Parameters 100 150 50 400 0.8 1 6
        , Parameters 100 150 50 400 0 1 6
        , Parameters 100 150 50 400 (-0.8) 1 6
        , Parameters 100 150 50 400 (-4.0) 1 6
        ]
