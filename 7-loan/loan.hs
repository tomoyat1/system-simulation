import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import qualified Data.Map.Strict as Map
import System.Environment
import System.IO

import Debug.Trace

import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Atom
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

type Params = (Double, Double, Double)
type Trim = Payment -> Params -> Double -> Double -> Int
data Payment = PIER | PER deriving (Show, Read)

-- 元利均等返済
-- 毎月(元金+利息)が一定額になるように払う
pier :: Params -> Double -> [(Double, Double)]
pier (monthly, bonus, intrest) principal = iter principal 1
    where iter :: Double -> Int -> [(Double, Double)]
          iter left months
                | (months `mod` 6) == 0 = bonusStep
                | otherwise = monthlyStep
                where bonusPayment = (bonus + monthly)
                      leftAfterBonus = left + left * intrest / 12 - bonusPayment
                      bonusStep
                        | left + left * intrest / 12 > (bonus + monthly) = (leftAfterBonus, bonusPayment) : iter leftAfterBonus (months + 1)
                        | otherwise = [(0, left + left * intrest / 12)]
                      monthlyPayment = monthly
                      leftAfterMonthly = left + left * intrest / 12 - monthlyPayment
                      monthlyStep
                        | left + left * intrest / 12> monthly = (leftAfterMonthly, monthlyPayment) : iter leftAfterMonthly (months + 1)
                        | otherwise = [(0, left + left * intrest / 12)]

-- 元金均等返済
-- 毎月の金利はその月に払ってしまう
per :: Params -> Double -> [(Double, Double)]
per (monthly, bonus, intrest) principal = iter principal 1
    where iter :: Double -> Int -> [(Double, Double)]
          iter left months
            | (months `mod` 6) == 0 = bonusStep
            | otherwise = monthlyStep
            where bonusPayment = bonus + monthly + left * intrest / 12
                  leftAfterBonus = left + left * intrest / 12 - bonusPayment
                  bonusStep
                    | left > (bonus + monthly) = (leftAfterBonus, bonusPayment) : iter leftAfterBonus (months + 1)
                    | otherwise = [(0, left)]
                  monthlyPayment = monthly + left * intrest / 12
                  leftAfterMonthly = left + left * intrest / 12 - monthlyPayment
                  monthlyStep
                    | left > monthly = (leftAfterMonthly, monthlyPayment) : iter leftAfterMonthly (months + 1)
                    | otherwise = [(0, left)]

sim :: Payment -> Params -> Double -> [(Double, Double)]
sim PIER params principal  = pier params principal
sim PER params principal = per params principal

years :: [(Double, Double)] -> Double
years output = fromIntegral (length output) / 12

auto :: Payment -> Params -> Double -> Double -> Trim -> Int
auto payment (monthly, bonus, intrest) principal period trim
    | zero <= period = 0
    | y < period = auto payment ((fromIntegral . floor) (monthly / 2), bonus, intrest) principal period trim
    | y > period = auto payment ((fromIntegral . floor) (monthly * 1.5), bonus, intrest) principal period trim
    | otherwise = trim payment (monthly, bonus, intrest) principal period
    where y = years $ sim payment (monthly, bonus, intrest) principal
          -- 1000年もあると現実的でなくなるからそこで打ち切り
          zero = years $ take 12000 $ sim payment (0, bonus, intrest) principal

trimMin :: Trim
trimMin payment (monthly, bonus, intrest) principal period
    | y == period = trimMin payment (monthly - 1, bonus, intrest) principal period
    | monthly <= 0 = 0
    | otherwise = floor monthly + 1
    where y = years $ sim payment (monthly, bonus, intrest) principal

trimMax :: Trim
trimMax payment (monthly, bonus, intrest) principal period
    | y == period = trimMax payment (monthly + 1, bonus, intrest) principal period
    | monthly <= 0 = 0
    | otherwise = floor monthly - 1
    where y = years $ sim payment (monthly, bonus, intrest) principal

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

doAuto :: Payment -> Double -> Double -> [Double] -> [Double] -> Map.Map Double (Map.Map Double (Int, Int))
doAuto payment bonus intrest principals periods =
    foldl (\m principal -> Map.insert principal (periodMap principal) m) Map.empty principals
    where periodMap :: Double -> Map.Map Double (Int, Int)
          periodMap principal = foldl (\m period -> Map.insert period (res principal period) m)
              Map.empty
              periods
          res principal period = (auto payment (principal / (period * 12), bonus, intrest) principal period trimMin,
                                                     auto payment (principal / (period * 12), bonus, intrest) principal period trimMax)

printRes m = do
    sequence_ $ Map.mapWithKey printPrincipal m
    where printPrincipal principal periods = do
          putStr $ show principal ++ ":\t"
          sequence_ $ Map.map (\res-> putStr (show res ++ "\t")) periods
          putStrLn ""

repaymentMin :: Payment -> Double -> Double -> Double -> (Int, Int) -> (Int, Int)
repaymentMin payment bonus intrest principal res = (start, end)
    where paymentList = sim payment ((fromIntegral . fst $ res), bonus, intrest) principal
          start = (floor . snd . head) paymentList
          end = (floor . snd . last) paymentList

repaymentMax :: Payment -> Double -> Double -> Double -> (Int, Int) -> (Int, Int)
repaymentMax payment bonus intrest principal res = (start, end)
    where paymentList = sim payment ((fromIntegral . snd $ res), bonus, intrest) principal
          start = (floor . snd . head) paymentList
          end = (floor . snd . last) paymentList

printRepaymentMin :: Map.Map Double (Map.Map Double (Int, Int)) -> Payment -> Double -> Double-> IO ()
printRepaymentMin m payment bonus intrest = do
    sequence_ $ Map.mapWithKey printPrincipal m
    where printPrincipal principal periods = do
          putStr $ show principal ++ ":\t"
          sequence_ $ Map.mapWithKey (\period res -> putStr $ (show (repaymentMin payment bonus intrest principal res )) ++ "\t") periods
          putStrLn ""

printRepaymentMax :: Map.Map Double (Map.Map Double (Int, Int)) -> Payment -> Double -> Double-> IO ()
printRepaymentMax m payment bonus intrest = do
    sequence_ $ Map.mapWithKey printPrincipal m
    where printPrincipal principal periods = do
          putStr $ show principal ++ ":\t"
          sequence_ $ Map.mapWithKey (\period res -> putStr $ (show (repaymentMax payment bonus intrest principal res )) ++ "\t") periods
          putStrLn ""
    
main :: IO ()
main = do
    let principals = [25 * 10^6, 30 * 10^6, 35 * 10^6, 40 * 10^6]
    let periods = [20, 25, 30, 35]
    let pierRes = doAuto PIER 300000 0.02 principals periods
    let perRes = doAuto PER 300000 0.02 principals periods
    putStrLn "元利均等返済"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes pierRes

    putStrLn "元金均等返済"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes perRes

    putStrLn "PIER返済額 (最小時)"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRepaymentMin pierRes PIER 300000 0.02

    putStrLn "PER返済額 (最小時)"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRepaymentMin perRes PER 300000 0.02

    putStrLn "PIER返済額 (最大時)"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRepaymentMax pierRes PIER 300000 0.02

    putStrLn "PER返済額 (最大時)"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRepaymentMax perRes PER 300000 0.02

    let per30mTotal = [map snd $ sim PER (33334, 300000, 0.02) (30 * 10^6), map snd $ sim PER (34261, 300000, 0.02) (30 * 10^6)]
    let per30mTotalNormalized = map (\list -> map (\m -> if m > 300000 then m - 300000 else m) list) per30mTotal
    plotLists [Title ("Principal Equal Repayment - Monthly Payments"), terminal (PNG.cons ("per-30m-total.png"))] per30mTotalNormalized

    let pier30m400k = doAuto PIER 400000 0.02 principals periods
    let per30m400k = doAuto PER 400000 0.02 principals periods
    let pier30m500k = doAuto PIER 500000 0.02 principals periods
    let per30m500k = doAuto PER 500000 0.02 principals periods
    let pier30m600k = doAuto PIER 600000 0.02 principals periods
    let per30m600k = doAuto PER 600000 0.02 principals periods

    putStrLn "元利均等返済 - 400,000"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes pier30m400k

    putStrLn "元金均等返済 400,000"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes per30m400k

    putStrLn "元利均等返済 - 500,000"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes pier30m500k

    putStrLn "元金均等返済 500,000"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes per30m500k

    putStrLn "元利均等返済 - 600,000"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes pier30m600k

    putStrLn "元金均等返済 600,000"
    putStr "\t"
    forM_ periods (\p -> putStr $ show p ++ "\t\t")
    putStrLn ""
    printRes per30m600k
