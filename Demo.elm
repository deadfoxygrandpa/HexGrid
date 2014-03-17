module Demo where

import Graphics.Input as Input
import Mouse
import Window
import String
import Dict

import open HexGrid

styleGuide : Float -> Int -> Color -> Int -> Float -> Form
styleGuide rotation sides col n =
    let colorize = case n of
                    1 -> filled col
                    2 -> filled col
                    _ -> filled col
        shape    = case n of
                    1 -> scale 0.8 . rotate (degrees <| rotation + 30) . colorize . ngon sides
                    2 -> scale 0.8 . rotate (degrees <| rotation + 35) . colorize . ngon sides
                    _ -> scale 0.9 . rotate (degrees <| rotation + 30) . colorize . ngon sides -- colorize . square
    in shape


(droppa, func) = Input.dropDown [ ("Diagonals", (\_ -> diagonals))
                                , ("Ring", flip ring)
                                , ("Range", flip range)
                                , ("Neighbors", (\_ -> neighbors))
                                , ("None", (\_ _ -> []))
                                ]
(txt, num) = Input.field "2"

(txt2, num2) = Input.field "5"

(bgrf, bgrs) = Input.field "0"
(bggf, bggs) = Input.field "0"
(bgbf, bgbs) = Input.field "0"

bgcolor = rgb <~ (maybe 0 id . String.toInt <~ bgrs)
               ~ (maybe 0 id . String.toInt <~ bggs)
               ~ (maybe 0 id . String.toInt <~ bgbs)

(fgrf, fgrs) = Input.field "255"
(fggf, fggs) = Input.field "156"
(fgbf, fgbs) = Input.field "0"

fgcolor = rgb <~ (maybe 255 id . String.toInt <~ fgrs)
               ~ (maybe 156 id . String.toInt <~ fggs)
               ~ (maybe 0   id . String.toInt <~ fgbs)

(rotf, rots) = Input.field "0"
(sidesf, sidess) = Input.field "6"

rotation = maybe 0 id . String.toFloat <~ rots
sides = maybe 6 id . String.toInt <~ sidess

gridsignal = (flip rectangularHexGrid) 0 <~ gridSizesignal
gridSizesignal = maybe 5 id <~ (String.toInt <~ num2)

scene (x, y) (w, h) selector f txtin s txt2in gridSize grid bgrf bggf bgbf bgcolor rotf sidesf fgrf fggf fgbf fgcolor styleGuide =
    let n' = maybe 2 id <| String.toInt s
        hexSize = (2 * (toFloat h))/(3 * (toFloat gridSize) + 1) --min (100000) ((1 * (toFloat h)) / (3 * (toFloat gridSize) + 2))
        grid' = foldr (\coord g -> insertIfPossible coord 2 g) grid <| f n' hovered
        grid'' = insertIfPossible hovered 1 grid'
        griddle = showHexGrid hexSize styleGuide <| grid''
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord hexSize pos
        plainText' = text . Text.color fgcolor . toText
        panel = flow down [ color fgcolor <| container 200 30 midLeft <| text . Text.color bgcolor . bold . toText <| "control panel:"
                           , spacer 2 2
                           , flow right <| map (size 100 20) [plainText' "effect:", color fgcolor selector]
                           , flow right <| map (size 100 20) [plainText' "effect size: ", color fgcolor txtin]
                           , flow right <| map (size 100 20) [plainText' "grid size: ", color fgcolor txt2in]
                           , plainText' "background color (rgb):"
                           , flow right <| intersperse (spacer 5 5) <|
                                              map (size 40 20) [ color fgcolor bgrf
                                                               , color fgcolor bggf
                                                               , color fgcolor bgbf
                                                               ]
                           , plainText' "foreground color (rgb):"
                           , flow right <| intersperse (spacer 5 5) <|
                                              map (size 40 20) [ color fgcolor fgrf
                                                               , color fgcolor fggf
                                                               , color fgcolor fgbf
                                                               ]
                           , flow right <| map (size 100 20) [plainText' "rotation:", color fgcolor rotf]
                           , flow right <| map (size 100 20) [plainText' "n-gon:", color fgcolor sidesf]
                           , plainText' "hex coord your mouse is at:"
                           , plainText' <| show  hovered
                           ]
    in  layers [ color bgcolor <| container w h middle griddle
               , color bgcolor <| spacer (widthOf panel + 3) (heightOf panel + 3)
               , container (widthOf panel + 5) (heightOf panel + 5) middle panel
               ]

main = scene <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num ~ txt2 ~ gridSizesignal ~ gridsignal
              ~ bgrf ~ bggf ~ bgbf ~ bgcolor ~ rotf ~ sidesf ~ fgrf ~ fggf ~ fgbf ~ fgcolor
              ~ (styleGuide <~ rotation ~ sides ~ fgcolor)
