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
                    1 -> filled lightOrange
                    2 -> filled darkOrange
                    _ -> filled darkOrange
        shape    = case n of
                    1 -> scale 0.5 . colorize . circle
                    2 -> rotate (degrees 30) . colorize . ngon 6
                    _ -> colorize . square
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
        plainText' = text . Text.color darkOrange . toText
        panel = flow down [ text . Text.color darkOrange . bold . toText <| "control panel:"
                           , flow right <| map (size 100 20) [plainText' "effect:", color darkOrange selector]
                           , flow right <| map (size 100 20) [plainText' "effect size: ", color darkOrange txtin]
                           , flow right <| map (size 100 20) [plainText' "grid size: ", color darkOrange txt2in]
                           , plainText' "hex coord your mouse is at:"
                           , plainText' <| show  hovered
                           ]
    in  layers [ color black <| container w h middle griddle
               , color darkOrange  <| spacer (widthOf panel + 5) (heightOf panel + 5)
               , color black <| spacer (widthOf panel + 3) (heightOf panel + 3)
               , container (widthOf panel + 5) (heightOf panel + 5) middle panel
               ]

main = scene <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num ~ txt2 ~ gridSizesignal ~ gridsignal
