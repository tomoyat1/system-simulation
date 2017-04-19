-- f a b と i で分けて考え、f a bをIOモナドに入れる。すると、bisectionとmonadが分けられる
-- bisectionは次のcを返す純粋関数に
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.Either
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens

data FuncAndRange = FuncAndRange { func :: Double -> Double
                                 , bottom :: Double
                                 , top :: Double
                                 }

rangeLength :: FuncAndRange -> Double
rangeLength funcAndRange = abs $ top funcAndRange - bottom funcAndRange

mean :: Double -> Double -> Double
mean a b =
  (a + b) / 2

declineNext f a b c
  | f c < 0 = (FuncAndRange f a c, c)
  | otherwise = (FuncAndRange f c b, c)

inclineNext f a b c
  | f c > 0 = (FuncAndRange f a c, c)
  | otherwise = (FuncAndRange f c b, c)

bisectionNext :: FuncAndRange -> (FuncAndRange, Double)
bisectionNext funcAndRange
  | f a > f b = declineNext f a b c
  | f a < f b = inclineNext f a b c
  where
    c = mean (bottom funcAndRange) (top funcAndRange)
    f = func funcAndRange 
    a = bottom funcAndRange
    b = top funcAndRange

loop :: FuncAndRange -> Int -> IO Double
loop funcAndRange i = do
  putStr $ show i
  putStr ": "
  let
    (newFuncAndRange, c) = bisectionNext funcAndRange
    res
      | rangeLength newFuncAndRange <= 10 ** (-4) = return c
      | otherwise = loop newFuncAndRange $ i + 1
  print c
  res

one :: Double -> Double
one x = x + sin x - 1

two :: Double -> Double
two x = x ** 3 - 8 * x ** 2 + 6 * x

three :: Double -> Double
three x = log x - x ** 2 + 3

four :: Double -> Double
four x = sin x + 0.1 * x - 1

quad :: Double -> Double
quad x = x ** 2 - 4

chart1 :: Renderable ()
chart1 = toRenderable layout
  where
    pl = plot_lines_values .~ [[ (x, one x) | x <- [0..1]::[Double]]]
      $ plot_lines_style . line_color .~ opaque blue
      $ plot_lines_style . line_width .~ 3
      $ plot_lines_title .~ "plot_lines"
      -- $ layout_x_axis . laxis_generate
      -- $ layout_y_axis . laxis_generate
      $ def
    layout = layout_title .~ "y = x + sin(x) - 1"
      $ layout_plots .~ [ toPlot pl ]
      $def

chart2 :: Renderable ()
chart2 = toRenderable layout
  where
    pl = plot_lines_values .~ [[ (x, two x) | x <- [-2,-1.99..8]::[Double]]]
      $ plot_lines_style . line_color .~ opaque blue
      $ plot_lines_style . line_width .~ 3
      $ plot_lines_title .~ "plot_lines"
      $ def
    layout = layout_title .~ "y = x^3 - 8x^2 + 6x"
      $ layout_plots .~ [ toPlot pl ]
      $def

chart3 :: Renderable ()
chart3 = toRenderable layout
  where
    pl = plot_lines_values .~ [[ (x, three x) | x <- [0,0.01..3]::[Double]]]
      $ plot_lines_style . line_color .~ opaque blue
      $ plot_lines_style . line_width .~ 3
      $ plot_lines_title .~ "plot_lines"
      $ def
    layout = layout_title .~ "y =  log(x) - x^2 + 3"
      $ layout_plots .~ [ toPlot pl ]
      $ layout_x_axis . laxis_generate .~ scaledAxis defaultIntAxis (0, 3)
      $ layout_y_axis . laxis_generate .~ scaledAxis defaultIntAxis (-3, 3)
      $ def

chart4 :: Renderable ()
chart4 = toRenderable layout
  where
    pl = plot_lines_values .~ [[ (x, four x) | x <- [-5,-4.99..25]::[Double]]]
      $ plot_lines_style . line_color .~ opaque blue
      $ plot_lines_style . line_width .~ 3
      $ plot_lines_title .~ "plot_lines"
      $ def
    layout = layout_title .~ "y =  sin(x) + 0.1x - 1"
      $ layout_plots .~ [ toPlot pl ]
      $ def

printRes :: String -> IO Double -> IO ()
printRes label res = putStrLn . (++) label . show =<< res

main = do
  printRes "Result 1: " $ loop (FuncAndRange one 0 1) 0

  printRes "Result 2-1: " $ loop (FuncAndRange two (-1) 0.1) 0
  printRes "Result 2-2: " $ loop (FuncAndRange two 0 1) 0
  printRes "Result 2-3: " $ loop (FuncAndRange two 6 8) 0

  printRes "Result 3-1: " $ loop (FuncAndRange three 0 1) 0
  printRes "Result 3-2: " $ loop (FuncAndRange three 1.5 2) 0

  printRes "Result 4-1: " $ loop (FuncAndRange four 0 1) 0
  printRes "Result 4-2: " $ loop (FuncAndRange four 1.1 3) 0
  printRes "Result 4-3: " $ loop (FuncAndRange four 5 7) 0
  printRes "Result 4-4: " $ loop (FuncAndRange four 7 10) 0
  printRes "Result 4-5: " $ loop (FuncAndRange four 10 13) 0
  printRes "Result 4-6: " $ loop (FuncAndRange four 14 16) 0
  printRes "Result 4-7: " $ loop (FuncAndRange four 16 17) 0
  printRes "Result 4-8: " $ loop (FuncAndRange four 17 20) 0

  renderableToFile def "chart1.png" chart1
  renderableToFile def "chart2.png" chart2
  renderableToFile def "chart3.png" chart3
  renderableToFile def "chart4.png" chart4
