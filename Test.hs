import Graphics.SVG

main = putStrLn . showTree $ doc

doc = fill "none" $ svg [stroke "black" . graphCoords $ axes,
                         stroke "blue" . graphCoords $ plot1,
                         stroke "green" . graphCoords $ plot2]

-- vertical axis and tick marks, horizontal axis and tick marks
axes = g $ [polyline [Point 0 0, Point 0 200],
            polyline [Point 0 0, Point 200 0]
           ] ++ xticks
             ++ yticks

xticks = [line (Point x 0) (Point x 5) | x <- [0,10..200]]

yticks = [line (Point 0 y) (Point 5 y) | y <- [0,10..200]]

plot1 = g [path [M (Point 0 0), Q (Point 100 0) (Point 200 200)]] -- a plot of x^2

plot2 = g [path [M (Point 0 0), Q (Point 0 100) (Point 200 200)]] -- a plot of sqrt x

graphCoords = addAttribute (Attribute "transform" "translate(10,250) scale(1,-1)") -- flip and translate so our graph isn't upside-down

