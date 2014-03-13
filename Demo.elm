module Demo where

import Graphics.Input as Input
import Mouse
import Window
import String
import Dict

import open HexGrid

styleGuide : Int -> Float -> Form
styleGuide n =
    let colorize = case n of
                    1 -> filled darkOrange
                    2 -> filled <| rgba 0 0 0 0.4
                    _ -> filled <| rgba 0 0 0 0.5
        shape    = case n of
                    1 -> \_ -> toForm . plainText <| "welp,"
                    2 -> rotate (degrees 30) . colorize . ngon 6
                    _ -> colorize . square--rotate (degrees 30) . colorize . ngon 6
    in shape


(droppa, func) = Input.dropDown [ ("Diagonals", (\_ -> diagonals))
                                , ("Ring", flip ring)
                                , ("Range", flip range)
                                , ("Neighbors", (\_ -> neighbors))
                                , ("None", (\_ _ -> []))
                                ]
(txt, num) = Input.field "3"

(txt2, num2) = Input.field "10"

gridsignal = (flip hexagonalHexGrid) 0 <~ gridSizesignal
gridSizesignal = maybe 10 id <~ (String.toInt <~ num2)

scene (x, y) (w, h) selector f txtin s txt2in gridSize grid =
    let n' = maybe 3 id <| String.toInt s
        hexSize = min (100000) ((1 * (toFloat h)) / (3 * (toFloat gridSize) + 2))
        grid' = foldr (\coord g -> insertIfPossible coord 2 g) grid <| f n' hovered
        grid'' = insertIfPossible hovered 1 grid'
        griddle = showHexGrid hexSize styleGuide <| grid''
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord hexSize pos
        panel = flow down [ text . bold . toText <| "control panel:"
                           , flow right <| map (width 100) [plainText "effect:", selector]
                           , flow right <| map (width 100) [plainText "effect size: ", color lightGreen txtin]
                           , flow right <| map (width 100) [plainText "grid size: ", color lightYellow txt2in]
                           , plainText "hex coord your mouse is at:"
                           , asText hovered
                           ]
    in  layers [ color lightOrange <| container w h middle griddle
               , color darkPurple  <| spacer (widthOf panel + 5) (heightOf panel + 5)
               , color lightPurple <| spacer (widthOf panel + 3) (heightOf panel + 3)
               , container (widthOf panel + 5) (heightOf panel + 5) middle panel
               ]

main = scene <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num ~ txt2 ~ gridSizesignal ~ gridsignal
