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

(txt2, num2) = Input.field "10"

gridsignal = (flip hexagonalHexGrid) 0 <~ gridSizesignal
gridSizesignal = maybe 10 id <~ (String.toInt <~ num2)
gride = (\g s -> showHexGrid s styleGuide g) <~ gridsignal ~ hexs
hexs = (\s h -> min (100000) ((1 * (toFloat h)) / (3 * (toFloat s) + 2))) <~ gridSizesignal ~ Window.height

scene (x, y) (w, h) selector f txtin s txt2in gridSize grid griddle hexSize =
    let n' = maybe 3 id <| String.toInt s
        --grid' = foldr (\coord g -> insertIfPossible coord 2 g) grid <| f n' hovered
        --grid'' = insertIfPossible hovered 1 grid'
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord hexSize pos
        floato = showHexGrid2 hexSize w h styleGuide <| Hexagonal [filter (\h -> inGrid h.coord grid) <| [Hex 1 hovered] ++ (map (\c -> Hex 2 c) <| f n' hovered)]
        panel = flow down [ text . bold . toText <| "control panel:"
                           , flow right <| map (width 100) [plainText "effect:", selector]
                           , flow right <| map (width 100) [plainText "effect size: ", color lightGreen txtin]
                           , flow right <| map (width 100) [plainText "grid size: ", color lightYellow txt2in]
                           , plainText "hex coord your mouse is at:"
                           , asText hovered
                           ]
    in  layers [ container w h middle griddle
               , container w h middle floato
               , color darkPurple  <| spacer (widthOf panel + 5) (heightOf panel + 5)
               , color lightPurple <| spacer (widthOf panel + 3) (heightOf panel + 3)
               , container (widthOf panel + 5) (heightOf panel + 5) middle panel
               ]

main = scene <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num ~ txt2 ~ gridSizesignal ~ gridsignal ~ gride ~ hexs
