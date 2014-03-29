module Demo where

import Graphics.Input as Input
import Mouse
import Window
import String
import Dict

import HexGrid (..)

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

-- Input Controls:

(gridTypeBox, gridType) = Input.dropDown
    [ ("Rectangular", True)
    , ("Hexagonal"  , False)
    ]
(effectBox, effect) = Input.dropDown
    [ ("Diagonals"   , \_ -> diagonals)
    , ("Ring"        , flip ring)
    , ("Range"       , flip range)
    , ("Neighbors"   , \_ -> neighbors)
    , ("Line"        , \_ -> line (hexCoord 0 0))
    , ("Rotate Left" , \_ coord -> [rotation Left coord, hexCoord 0 0])
    , ("Rotate Right", \_ coord -> [rotation Right coord, hexCoord 0 0])
    , ("None"        , \_ _ -> [])
    ]
(effectSizeBox, effectSizeString) = Input.field "2"
(gridSizeBox  , gridSizeString)   = Input.field "5"
(bgRedBox     , bgRedString)      = Input.field "0"
(bgGreenBox   , bgGreenString)    = Input.field "0"
(bgBlueBox    , bgBlueString)     = Input.field "0"
(fgRedBox     , fgRedString)      = Input.field "255"
(fgGreenBox   , fgGreenString)    = Input.field "156"
(fgBlueBox    , fgBlueString)     = Input.field "0"
(rotationBox  , rotationString)   = Input.field "0"
(sidesBox     , sidesString)      = Input.field "6"

-- Input Signals:

effectSize = maybe 2 id . String.toInt <~ effectSizeString
gridSize = (,) <~ (maybe 5 id . String.toInt <~ gridSizeString)
                ~ (maybe 5 id . String.toInt <~ gridSizeString)
bgColor = rgb <~ (maybe 0   id . String.toInt   <~ bgRedString)
               ~ (maybe 0   id . String.toInt   <~ bgGreenString)
               ~ (maybe 0   id . String.toInt   <~ bgBlueString)
fgColor = rgb <~ (maybe 255 id . String.toInt   <~ fgRedString)
               ~ (maybe 156 id . String.toInt   <~ fgGreenString)
               ~ (maybe 0   id . String.toInt   <~ fgBlueString)
rotationOffest =  maybe 0   id . String.toFloat <~ rotationString
sides          =  maybe 6   id . String.toInt   <~ sidesString

grid = (\s t -> if t then rectangularHexGrid s 0 else hexagonalHexGrid (fst s) 0) <~ gridSize ~ gridType

hexSize = (\h gridSize t -> if t
    then (2 * (toFloat h))/(3 * (toFloat . fst <| gridSize) + 1)
    else min (100000) ((1 * (toFloat h)) / (3 * (toFloat . fst <| gridSize) + 2))
    ) <~ Window.height ~ gridSize ~ gridType

type Environment = { mouse : (Int, Int)
                   , window : (Int, Int)
                   , gridTypeBox : Element
                   , effectBox : Element
                   , effect : Int -> (Int, Int) -> [HexCoord]
                   , effectSizeBox : Element
                   , effectSize : Int
                   , gridSizeBox : Element
                   , hexSize : Float
                   , grid : HexGrid Int
                   , bgRedBox : Element
                   , bgGreenBox : Element
                   , bgBlueBox : Element
                   , bgColor : Color
                   , rotationBox : Element
                   , sidesBox : Element
                   , fgRedBox : Element
                   , fgGreenBox : Element
                   , fgBlueBox : Element
                   , fgColor : Color
                   , styleGuide : Int -> Float -> Form
                   }

environment : Signal Environment
environment = Environment <~ Mouse.position
                           ~ Window.dimensions
                           ~ gridTypeBox
                           ~ effectBox
                           ~ effect
                           ~ effectSizeBox
                           ~ effectSize
                           ~ gridSizeBox
                           ~ hexSize
                           ~ grid
                           ~ bgRedBox
                           ~ bgGreenBox
                           ~ bgBlueBox
                           ~ bgColor
                           ~ rotationBox
                           ~ sidesBox
                           ~ fgRedBox
                           ~ fgGreenBox
                           ~ fgBlueBox
                           ~ fgColor
                           ~ (styleGuide <~ rotationOffest ~ sides ~ fgColor)

scene environment =
    let (x, y) = environment.mouse
        (w, h) = environment.window
        bgColor = environment.bgColor
        fgColor = environment.fgColor
        grid' = foldr (\coord g -> insertIfPossible coord 2 g) environment.grid <| environment.effect environment.effectSize hovered
        grid'' = insertIfPossible hovered 1 grid'
        griddle = showHexGrid environment.hexSize environment.styleGuide <| grid''
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord environment.hexSize pos
        plainText' = text . Text.color fgColor . toText
        panel = flow down [ color fgColor <| container 200 30 midLeft <| text . Text.color bgColor . bold . toText <| "control panel:"
                           , spacer 2 2
                           , flow right <| map (size 100 20) [plainText' "grid type:", color fgColor environment.gridTypeBox]
                           , flow right <| map (size 100 20) [plainText' "effect:", color fgColor environment.effectBox]
                           , flow right <| map (size 100 20) [plainText' "effect size: ", color fgColor environment.effectSizeBox]
                           , flow right <| map (size 100 20) [plainText' "grid size: ", color fgColor environment.gridSizeBox]
                           , plainText' "background color (rgb):"
                           , flow right <| intersperse (spacer 5 5) <|
                                              map (size 40 20) [ color fgColor environment.bgRedBox
                                                               , color fgColor environment.bgGreenBox
                                                               , color fgColor environment.bgBlueBox
                                                               ]
                           , plainText' "foreground color (rgb):"
                           , flow right <| intersperse (spacer 5 5) <|
                                              map (size 40 20) [ color fgColor environment.fgRedBox
                                                               , color fgColor environment.fgGreenBox
                                                               , color fgColor environment.fgBlueBox
                                                               ]
                           , flow right <| map (size 100 20) [plainText' "rotation:", color fgColor environment.rotationBox]
                           , flow right <| map (size 100 20) [plainText' "n-gon:", color fgColor environment.sidesBox]
                           , plainText' "hex coord your mouse is at:"
                           , plainText' <| show hovered
                           ]
    in  layers [ color bgColor <| container w h middle griddle
               , color bgColor <| spacer (widthOf panel + 3) (heightOf panel + 3)
               , container (widthOf panel + 5) (heightOf panel + 5) middle panel
               ]

main = scene <~ environment
