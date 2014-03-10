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
        gridSize = 10
        hexSize = min (100000) ((1 * (toFloat h)) / (3 * gridSize + 2))
        grid = hexagonalHexGrid gridSize 0
        grid' = foldr (\coord g -> insertIfPossible coord 2 g) grid <| f n' hovered
        grid'' = insertIfPossible hovered 1 grid'
        griddle = color green <| showHexGrid hexSize styleGuide <| grid''
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord hexSize pos
    in  layers [container w h middle griddle, flow down [container w 50 topLeft <| selector `above` txtin, asText hovered, asText <| sizeOf griddle]]

main = scene <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num
