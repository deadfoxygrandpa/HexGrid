module Demo where

import Graphics.Input as Input
import Mouse
import Window
import String
import Dict

import open HexGrid

styleGuide : Dict.Dict Int Shaper
styleGuide = Dict.fromList [ (0, SOutlined defaultLine)
                           , (1, SColor blue)
                           , (2, SColor red)
                           ]


(droppa, func) = Input.dropDown [ ("None", (\_ _ -> []))
                                , ("Ring", flip ring)
                                , ("Range", flip range)
                                , ("Diagonals", (\_ -> diagonals))
                                , ("Neighbors", (\_ -> neighbors))]
(txt, num) = Input.field "3"

scene (x, y) (w, h) selector f txtin s =
    let n' = maybe 3 id <| String.toInt s
        grid = hexagonalHexGrid 10 0
        grid' = foldr (\coord g -> insertIfPossible coord 2 g) grid <| f n' hovered
        grid'' = insertIfPossible hovered 1 grid'
        griddle = showHexGrid 25 styleGuide <| grid''
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord 25 pos
    in  layers [container w h middle griddle, flow down [container w 50 topLeft <| selector `above` txtin, asText hovered]]

main = scene <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num
