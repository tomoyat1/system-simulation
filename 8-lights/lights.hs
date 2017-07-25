{-# LANGUAGE NamedFieldPuns #-}
data SimpleLights = SimpleLights
    { lights :: (Double, Double, Double)
    , desk1 :: Double
    , desk2 :: Double } deriving (Show)

fivePercent :: Double -> Double -> Bool
fivePercent target val = val >= target * 0.95 && val <= target * 1.05

power (a, b, c) = a + b + c

minimumPower :: [SimpleLights] -> (SimpleLights, Double)
minimumPower sl = (res, (power . lights) res)
    where res = iter (head sl) sl
          iter a [] = a
          iter a sl = if (power . lights . head) sl > (power . lights) a then
                            iter a (tail sl)
                        else
                            iter (head sl) (tail sl)
squaredMeanError :: SimpleLights -> Double
squaredMeanError sl = (foldl (\a (x, y) -> a + (x - y) ** 2) 0 vals) / (fromIntegral . length) vals
    where vals = [(desk1 sl, 400), (desk2 sl, 700)]

minimumSquaredMeanError :: [SimpleLights] -> (SimpleLights, Double)
minimumSquaredMeanError sl = (res, squaredMeanError res)
    where res = iter (head sl) sl
          iter a [] = a
          iter a sl = if (squaredMeanError . head) sl > squaredMeanError a then
                            iter a (tail sl)
                        else
                            iter (head sl) (tail sl)

-- (0, 1325, 2000)
judge :: SimpleLights -> Bool
judge (SimpleLights (a, b, c) desk1 desk2) = fivePercent 400 desk1 && fivePercent 700 desk2

calc :: (Double, Double, Double) -> SimpleLights
calc (a, b, c) = SimpleLights (a, b, c) desk1 desk2
    where distA1 = 0.3 ** 2 + 2.0 ** 2
          distB1 = 1.2 ** 2 + 2.0 ** 2
          distC1 = 2.4 ** 2 + 2.0 ** 2
          distA2 = 1.5 ** 2 + 2.0 ** 2
          distB2 = 0.3 ** 2 + 2.0 ** 2
          distC2 = 0.9 ** 2 + 2.0 ** 2
          cosA1 = 2.0 / sqrt distA1
          cosB1 = 2.0 / sqrt distB1
          cosC1 = 2.0 / sqrt distC1
          cosA2 = 2.0 / sqrt distA2
          cosB2 = 2.0 / sqrt distB2
          cosC2 = 2.0 / sqrt distC2
          desk1 = cosA1 * a / distA1 + cosB1 * b / distB1 + cosC1 * c / distC1
          desk2 = cosA2 * a / distB2 + cosB2 * b / distC2 + cosC2 * c / distC2
values :: [(Double, Double, Double)]
values = (,,) <$> [0,10..2000] <*> [0,10..2000] <*> [0,10..2000]

res :: [SimpleLights]
res = filter judge $ map calc values

resWithMinPower = minimumPower res
resWithMinError = minimumSquaredMeanError res
--resWithLeastDeviation
