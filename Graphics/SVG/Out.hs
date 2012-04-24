module Graphics.SVG.Out where

import Graphics.SVG.Types
import Text.Printf

showFlag :: Bool -> String
showFlag False = "0"
showFlag True = "1"

showTree :: SVGTree -> String
showTree (Leaf p e) = showElem e p
showTree (Node p b es) = showBlock b p es

showElem :: Elem -> Props -> String
showElem (Rect (Point x y) w h) props =
    printf "<rect x=\"%f\" y=\"%f\" width=\"%f\" height=\"%f\" %s />"
    x y w h (showProps props)
showElem (Circle (Point x y) r) props =
    printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" %s />"
    x y r (showProps props)
showElem (Ellipse (Point x y) rx ry) props =
    printf "<ellipse cx=\"%f\" cy=\"%f\" rx=\"%f\" ry=\"%f\" %s />"
    x y rx ry (showProps props)
showElem (Polyline ps) props =
    printf "<polyline points=\"%s\" %s />"
    (showPoints ps) (showProps props)
showElem (Path p) props =
    printf "<path d=\"%s\" %s />"
    (showPath p) (showProps props)
showElem (Text (Point x y) s) props =
    printf "<text x=\"%f\" y=\"%f\" %s>%s</text>"
    x y (showProps props) s
showElem (Use s) props =
    printf "<use xlink:href=\"#%s\" %s >"
    s (showProps props)

showProps :: Props -> String
showProps (Props as) = unwords . map showAttribute $ as

showAttribute :: Attribute -> String
showAttribute (Attribute k v) = printf "%s=\"%s\"" k v

showBlock :: Block -> Props -> [SVGTree] -> String
showBlock b props es = startBlock b props ++ (showTree =<< es) ++ endBlock b

startBlock :: Block -> Props -> String
startBlock Root props = printf "<svg xmlns=\"http://www.w3.org/2000/svg\" %s >" (showProps props)
--startBlock Viewport props = -- TODO
startBlock Group props = printf "<g %s >" (showProps props)
startBlock Defs props = printf "<defs %s >" (showProps props)
startBlock Symbol props = printf "<symbol %s >" (showProps props)

endBlock :: Block -> String
endBlock Root = "</svg>"
--endBlock Viewport = -- TODO
endBlock Group = "</g>"
endBlock Defs = "</defs>"
endBlock Symbol = "</symbol>"

showPoint :: Point -> String
showPoint (Point x y) = printf "%f,%f" x y

showPoints :: [Point] -> String
showPoints = unwords . map showPoint

showPath :: Path -> String
showPath = unwords . map showPathData

showPathData :: PathData -> String
showPathData Z = "Z"
showPathData (M p) = printf "M %s" (showPoint p)
showPathData (MR p) = printf "m %s" (showPoint p)
showPathData (L p) = printf "L %s" (showPoint p)
showPathData (LR p) = printf "l %s" (showPoint p)
showPathData (C c1 c2 d) = printf "C %s %s %s" (showPoint c1) (showPoint c2) (showPoint d)
showPathData (CR c1 c2 d) = printf "c %s %s %s" (showPoint c1) (showPoint c2) (showPoint d)
showPathData (S c d) = printf "S %s %s" (showPoint c) (showPoint d)
showPathData (SR c d) = printf "s %s %s" (showPoint c) (showPoint d)
showPathData (Q c d) = printf "Q %s %s" (showPoint c) (showPoint d)
showPathData (QR c d) = printf "q %s %s" (showPoint c) (showPoint d)
showPathData (T d) = printf "T %s" (showPoint d)
showPathData (TR d) = printf "t %s" (showPoint d)
showPathData (A rx ry th la sw d) = printf "A %f %f %f %s %s %s %s" rx ry th (showFlag la) (showFlag sw) (showPoint d)
showPathData (AR rx ry th la sw d) = printf "a %f %f %f %s %s %s %s" rx ry th (showFlag la) (showFlag sw) (showPoint d)

