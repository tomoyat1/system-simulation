import Control.Monad
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Atom
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

deltaT :: Double
deltaT = 0.001

dur :: (Num a) => a
dur = 8

durMili :: (Num a) => a
durMili = dur * 1000

class Tank t where
    heightToVol :: t -> Double -> Double
    volToHeight :: t -> Double -> Double
    alpha :: t -> Double
    baseRadius :: t -> Double
    tankHeight :: t -> Double

data CylinderTank = CylinderTank {
      cylinderRadius :: Double
    , cylinderAlpha :: Double
    , cylinderHeight :: Double }

instance Tank CylinderTank where
    heightToVol t height = height * (baseRadius t ** 2 * pi)
    volToHeight t volume = volume / (baseRadius t ** 2 * pi)
    alpha = cylinderAlpha
    baseRadius = cylinderRadius
    tankHeight = cylinderHeight

data ConeTank = ConeTank { 
      coneRadius :: Double
    , coneAlpha :: Double
    , coneHeight :: Double }

instance Tank ConeTank where
    heightToVol t height =
        (1/3) * (baseRadius t / tankHeight t) ** 2 * height ** 3
    volToHeight t volume = 
        ((3/pi) * (tankHeight t / baseRadius t) ** 2 * volume) ** (1/3)
    alpha = coneAlpha
    baseRadius = coneRadius
    tankHeight = coneHeight

alphas = [0.5, 1, 2]

cylinderTankWithAlpha alpha = CylinderTank 0.5 alpha 2
coneTankWithAlpha alpha = ConeTank 0.75 alpha 2.5

tank0s = map cylinderTankWithAlpha alphas
tank1s = map coneTankWithAlpha alphas

nextVolume :: (Tank t) => t -> Double -> Double -> Double
nextVolume tank vGain curVol = curVol + vGain - vLoss
    where vLoss = (alpha tank * ((volToHeight tank) curVol) * deltaT)

recurrenceToSeq :: (a -> a) -> a -> [a]
recurrenceToSeq f init =
    init:map f (recurrenceToSeq f init)

recurrenceToSeqWithInput :: (a -> a -> a) -> [a] -> a -> [a]
recurrenceToSeqWithInput f inputList init =
    init:zipWith f inputList (recurrenceToSeqWithInput f inputList init)

heightSeq tank inflowList initVol = map (volToHeight tank) volSeq
    where volSeq = recurrenceToSeqWithInput (nextVolume tank) inflowList initVol

doSimulation :: CylinderTank -> ConeTank -> IO ()
doSimulation tank0 tank1 = do  
    let xs0 = linearScale durMili (0, dur)
    let ys0 = take (fromInteger durMili) tank0HeightSeq
    let points = zip xs0 ys0
    putStrLn $ "drawing graph for tank0: alpha = (" ++ (show . alpha) tank0++ ", " ++ (show . alpha) tank1 ++ ")"

    let xs1 = linearScale durMili (0, dur)
    let ys1 = take durMili tank1HeightSeq

    let points2 = zip xs1 ys1
    putStrLn $ "drawing graph for tank1: alpha = (" ++ (show . alpha) tank0 ++ ", " ++ (show . alpha) tank1 ++ ")"
               
    plotPaths [(Title ("both: alpha = (" ++ (show . alpha) tank0 ++ ", " ++ (show . alpha) tank1 ++ "), "++ show dur ++ "s")),
               terminal (PNG.cons ("./both-"++ (show . alpha) tank0 ++ "-" ++ (show . alpha) tank1 ++ ".png"))] [points, points2]

    where tank0HeightSeq = heightSeq tank0 [0,0..] ((heightToVol tank0) 2)
          inflowFromTank0 = map (heightToVol tank0) (zipWith (-) tank0HeightSeq (tail tank0HeightSeq))
          tank1HeightSeq = heightSeq tank1 inflowFromTank0 0

main = do
    sequence $ doSimulation <$> tank0s <*> tank1s

    let xs = linearScale 8000 (0, 8.0 :: Double)
    let ys = take 8000 $ map (volToHeight (tank0s!!2)) $ map q [0,0.001..]
    print $ take 400 ys
    let points = zip xs ys
    plotPath [(Title ("solved equation: alpha = 0.5")),
               terminal (PNG.cons ("./solved.png"))] points
    where q t = 0.5**2 * pi * 2 * exp (-0.5 * t / (0.5**2 * pi))
