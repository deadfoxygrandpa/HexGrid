module HexGrid where

import Set
import Dict
import Mouse
import Window
import Graphics.Input as Input

import Public.Preface.Preface (replace, splitAt, (#), repeat)

data HexGrid comparable = Rectangular [[Hex comparable]] | Hexagonal [[Hex comparable]]
type Hex comparable     = {value : comparable, coord : HexCoord}
type HexCoord  = {x : Int, y : Int}

data Shaper = SColor Color | STextured String | SGradient Gradient | SOutlined LineStyle

rectangularHexGrid : (Int, Int) -> comparable -> HexGrid comparable
rectangularHexGrid (w, h) a =
    let row x y        = map (\n -> {value = a, coord = {x = n, y = y}}) [0-x..w - 1 - x]
        rowPair offset = [row offset (offset * 2), row offset (offset * 2 + 1)]
    in  Rectangular <| concatMap (\n -> rowPair n) [0..(ceiling <| (toFloat h) / 2) - 1]

hexagonalHexGrid : Int -> comparable -> HexGrid comparable
hexagonalHexGrid r a =
    let row y = map (\n -> {value = a, coord = {x = n - (min y r), y = y - r}}) [0..r*2 - (abs (r - y))]
    in  Hexagonal <| map (\n -> row n) [0..2*r]

toHexList : HexGrid comparable -> [[Hex comparable]]
toHexList grid =
    case grid of
        Rectangular hs -> hs
        Hexagonal   hs -> hs
        
toTuple : Hex comparable -> (Int, Int, comparable)        
toTuple hex = (hex.coord.x, hex.coord.y, hex.value)

showHexGrid : Float -> Dict.Dict comparable Shaper -> HexGrid comparable -> Element
showHexGrid r dict grid =
    case grid of
        Rectangular hs -> flow down . map asText . map (\row -> map toTuple row) <| hs
        Hexagonal   hs -> let position {x, y} r = move (((sqrt 3) * r * (toFloat x) + ((sqrt 3)/2) * r * (toFloat y)), (-1.5 * r * (toFloat y)))
                              drawHex coord r v = position coord r . rotate (degrees 30) . makeForm (Dict.findWithDefault (SOutlined defaultLine) v dict) . ngon 6 <| r
                              w = round <| (sqrt 3) / 1.52 * (toFloat h)
                              h = round <| r * (toFloat <| length hs) * 1.53
                          in  collage w h <| map (\hex -> drawHex hex.coord r hex.value) (concat hs)

pixelToHexCoord : Float -> (Int, Int) -> HexCoord
pixelToHexCoord s (x, y) =
    let q = (1/3 * (sqrt 3) * (toFloat x) - 1/3 * (toFloat y)) / s
        r = 2/3 * (toFloat y) / s
        (x', y') = hexRound (q, r)
    in  HexCoord x' y'

makeForm : Shaper -> Shape -> Form
makeForm shaper shape =
    case shaper of
        SColor c    -> filled c shape
        STextured s -> textured s shape
        SGradient g -> gradient g shape
        SOutlined l -> outlined l shape

inGrid : HexCoord -> HexGrid comparable -> Bool
inGrid {x, y} grid =
    case grid of
        Rectangular hs -> let h = length hs
                              w = length . head <| hs
                              offset = y `div` 2
                          in  if | y < 0  -> False
                                 | y >= h -> False
                                 | x >= w -> False
                                 | x + offset < 0 -> False
                                 | x + offset >= w -> False
                                 | otherwise -> True
        Hexagonal   hs -> let radius = (length hs) `div` 2
                              offset = radius * 2 - (abs y)
                          in  if | y < -radius -> False
                                 | y >  radius -> False
                                 | x + radius + (min 0 y) < 0 -> False
                                 | x > offset - radius - (min 0 y) -> False
                                 | otherwise -> True

neighbors : HexCoord -> [HexCoord]
neighbors {x, y} = [ HexCoord (x + 1) y, HexCoord (x + 1) (y - 1), HexCoord x (y - 1)
                   , HexCoord (x - 1) y, HexCoord (x - 1) (y + 1), HexCoord x (y + 1) ]

diagonals : HexCoord -> [HexCoord]
diagonals {x, y} = [ HexCoord (x + 2) (y - 1), HexCoord (x + 1) (y - 2), HexCoord (x - 1) (y - 1)
                   , HexCoord (x - 2) (y + 1), HexCoord (x - 1) (y + 2), HexCoord (x + 1) (y + 1) ]   

distance : HexCoord -> HexCoord -> Int
distance coord1 coord2 =
    let (x1, y1) = (coord1.x, coord1.y)
        (x2, y2) = (coord2.x, coord2.y)
        (z1, z2) = (-(x1 - y1), -(x2 - y2))
    in  maximum [abs (x1 - x2), abs (y1 - y2), abs (z1 - z2)]

line : HexCoord -> HexCoord -> [HexCoord]
line coord1 coord2 =
    let (x1, y1) = ((toFloat coord1.x) + 0.000001, (toFloat coord1.y) + 0.000001)
        (x2, y2) = (toFloat coord2.x, toFloat coord2.y)
        (z1, z2) = (-(x1 - y1), -(x2 - y2))
        dx       = x1 - x2
        dy       = y1 - y2
        dz       = z1 - z2
        n        = round . maximum <| map abs [dx - dy, dy - dz, dz - dx]
        f x      = (toFloat x) / (toFloat n)
    in  map (\(a, b) -> HexCoord a b) . Set.toList . Set.fromList <| map (\i -> hexRound (x1 * (f i) + x2 * (1 - (f i)), y1 * (f i) + y2 * (1 - (f i)))) [0..n]

range : HexCoord -> Int -> [HexCoord]
range {x, y} n = 
    concatMap (\dx -> 
        map (\dy -> 
            HexCoord (x + dx) (y + dy)) 
        [max -n (-dx - n)..min n (-dx + n)]) 
    [-n..n]

ring : HexCoord -> Int -> [HexCoord]
ring {x, y} r =
    let h = HexCoord (x - r) (y + r) -- move southwest r tiles
    in  scanl (\i h' ->
            neighbor h' i)
            h
            (concatMap (\j -> repeat j r) [0..5])

neighbor : HexCoord -> Int -> HexCoord
neighbor {x, y} direction =
    let (dx, dy) = maybe (0, 0) id <| [ (1, 0), (1, -1), (0, -1)
                                      , (-1, 0), (-1, 1), (0, 1) ] # direction
    in  HexCoord (x + dx) (y + dy)

hexRound : (Float, Float) -> (Int, Int)
hexRound (x, y) =
    let z            = -(x - y)
        (rx, ry, rz) = (round x, round y, round z)
        errX         = abs (rx - x)
        errY         = abs (ry - y)
        errZ         = abs (rz - z)
    in  if | errX > errY && errX > errZ -> ((-ry - rz), ry)
           | errY > errZ                -> (rx, (-rx - rz))
           | otherwise                  -> (rx, ry)   

insert : HexCoord -> comparable -> HexGrid comparable -> Maybe (HexGrid comparable)
insert ({x, y} as coord) z grid = if not <| inGrid coord grid then Nothing else
    case grid of
        Rectangular hs -> Just grid
        Hexagonal   hs -> let radius = (length hs) `div` 2
                              row    = head . drop (y + radius) <| hs
                              row'   = replace (x + radius + (min 0 y)) (Hex z (HexCoord x y)) row
                          in  Just . Hexagonal <| replace (y + radius) row' hs 

insertIfPossible : HexCoord -> comparable -> HexGrid comparable -> HexGrid comparable
insertIfPossible ({x, y} as coord) z grid = if not <| inGrid coord grid then grid else
    case grid of
        Rectangular hs -> grid
        Hexagonal hs   -> let radius = (length hs) `div` 2
                              row    = head . drop (y + radius) <| hs
                              row'   = replace (x + radius + (min 0 y)) (Hex z (HexCoord x y)) row
                          in  Hexagonal <| replace (y + radius) row' hs 

-- Example

styleGuide : Dict.Dict Int Shaper
styleGuide = Dict.fromList [ (0, SOutlined defaultLine)
                           , (1, SColor blue)
                           , (2, SColor darkOrange)
                           ]

pixelToHexCoord2 : Float -> (Int, Int) -> HexCoord
pixelToHexCoord2 s (x, y) =
    let q = (1/3 * (sqrt 3) * (toFloat x) - 1/3 * (toFloat y)) / s
        r = 2/3 * (toFloat y) / s
    in  HexCoord (round q) (round r)

pixelToHexCoord3 : Float -> (Int, Int) -> HexCoord
pixelToHexCoord3 s (x, y) =
    let x' = toFloat x
        y' = toFloat y
        temp = floor  <| x' + (sqrt 3) * y' + 1
        q = floor <| ((toFloat (floor <| x' * 2 + 1)) + (toFloat temp)) / (3 * s)
        r = floor <| ((toFloat temp) + (toFloat (floor <| -x' + (sqrt 3) * y' + 1))) / (3 * s)
    in  HexCoord q r

(droppa, func) = Input.dropDown [ ("None", (\_ _ -> []))
                                , ("Ring", flip ring)
                                , ("Range", flip range)
                                , ("Diagonals", (\_ -> diagonals))
                                , ("Neighbors", (\_ -> neighbors))]
(txt, num) = Input.field "3"

scene n (x, y) (w, h) selector f txtin s = 
    let n' = maybe 3 id <| readInt s
        grid = hexagonalHexGrid 10 0
        grid' = foldr (\coord g -> insertIfPossible coord 2 g) grid <| f n' hovered
        grid'' = insertIfPossible hovered 1 grid'
        griddle = showHexGrid 25 styleGuide <| grid''
        pos = (x - (w `div` 2), y - (h `div` 2))
        hovered = pixelToHexCoord3 25 pos
    in  layers [container w h middle griddle, flow down [container w 50 midTop <| selector `above` txtin, asText hovered, asText <| pixelToHexCoord3 25 pos]]

main = scene 3 <~ Mouse.position ~ Window.dimensions ~ droppa ~ func ~ txt ~ num
