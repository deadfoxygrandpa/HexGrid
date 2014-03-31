module Demo where

import Graphics.Input as Input
import Graphics.Input.Field as Field
import Mouse
import Window
import String
import Dict

import HexGrid (..)
import HexGrid

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
gridSelector = Input.input True
gridTypeBox = Input.dropDown gridSelector.handle
    [ ("Rectangular", True)
    , ("Hexagonal"  , False)
    ]
gridType = gridSelector.signal

effectSelector = Input.input <| \_ -> diagonals
effectBox = Input.dropDown effectSelector.handle
    [ ("Diagonals"   , \_ -> diagonals)
    , ("Ring"        , flip ring)
    , ("Range"       , flip range)
    , ("Neighbors"   , \_ -> neighbors)
    , ("Line"        , \_ -> line (hexCoord 0 0))
    , ("Rotate Left" , \_ coord -> [HexGrid.rotation Left coord, hexCoord 0 0])
    , ("Rotate Right", \_ coord -> [HexGrid.rotation Right coord, hexCoord 0 0])
    , ("None"        , \_ _ -> [])
    ]
effect = effectSelector.signal

effectSize' = Input.input <| Field.Content "2" (Field.Selection 0 0 Field.Forward)
gridSize' = Input.input <| Field.Content "5" (Field.Selection 0 0 Field.Forward)
bgRed = Input.input <| Field.Content "0" (Field.Selection 0 0 Field.Forward)
bgGreen = Input.input <| Field.Content "0" (Field.Selection 0 0 Field.Forward)
bgBlue = Input.input <| Field.Content "0" (Field.Selection 0 0 Field.Forward)
fgRed = Input.input <| Field.Content "255" (Field.Selection 0 0 Field.Forward)
fgGreen = Input.input <| Field.Content "156" (Field.Selection 0 0 Field.Forward)
fgBlue = Input.input <| Field.Content "0" (Field.Selection 0 0 Field.Forward)
rotation = Input.input <| Field.Content "0" (Field.Selection 0 0 Field.Forward)
sides' = Input.input <| Field.Content "6" (Field.Selection 0 0 Field.Forward)

fieldStyle = Field.Style (Field.uniformly 3) Field.noOutline (Field.Highlight black 10) defaultStyle

effectSizeBox = Field.field fieldStyle effectSize'.handle id "2" <~ effectSize'.signal
gridSizeBox   = Field.field fieldStyle gridSize'.handle id "5" <~ gridSize'.signal
bgRedBox      = Field.field fieldStyle bgRed.handle id "0" <~ bgRed.signal
bgGreenBox    = Field.field fieldStyle bgGreen.handle id "0" <~ bgGreen.signal
bgBlueBox     = Field.field fieldStyle bgBlue.handle id "0" <~ bgBlue.signal
fgRedBox      = Field.field fieldStyle fgRed.handle id "255" <~ fgRed.signal
fgGreenBox    = Field.field fieldStyle fgGreen.handle id "156" <~ fgGreen.signal
fgBlueBox     = Field.field fieldStyle fgBlue.handle id "0" <~ fgBlue.signal
rotationBox   = Field.field fieldStyle rotation.handle id "0" <~ rotation.signal
sidesBox      = Field.field fieldStyle sides'.handle id "6" <~ sides'.signal

effectSizeString = .string <~ effectSize'.signal
gridSizeString = .string <~ gridSize'.signal
bgRedString = .string <~ bgRed.signal
bgGreenString = .string <~ bgGreen.signal
bgBlueString = .string <~ bgBlue.signal
fgRedString = .string <~ fgRed.signal
fgGreenString = .string <~ fgGreen.signal
fgBlueString = .string <~ fgBlue.signal
rotationString = .string <~ rotation.signal
sidesString = .string <~ sides'.signal

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
                           ~ (constant gridTypeBox)
                           ~ (constant effectBox)
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
        plainText' = centered . Text.color fgColor . toText
        panel = flow down [ color fgColor <| container 200 30 midLeft <| centered . Text.color bgColor . bold . toText <| "control panel:"
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
