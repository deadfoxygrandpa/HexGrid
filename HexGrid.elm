module HexGrid where

{-| A library for creating and working with Hexagonal grids. The grids may be either rectangular or hexagonal in shape.
A HexGrid must be of type `HexGrid comparable` which means each Hex must contain a value of any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists of comparable types.

Currently all hexes are pointy-top.

# Build
@docs rectangularHexGrid, hexagonalHexGrid, insert, insertIfPossible

# Process
@docs neighbor, neighbors, ring, range, diagonals, line, distance, inGrid

# Interact
@docs showHexGrid, pixelToHexCoord

-}

import Set
import Dict

import open Helpers

data HexGrid comparable = Rectangular [[Hex comparable]] | Hexagonal [[Hex comparable]]
type Hex comparable     = {value : comparable, coord : HexCoord}
type HexCoord  = {x : Int, y : Int}

data Shaper = SColor Color | STextured String | SGradient Gradient | SOutlined LineStyle

{-| Create an empty rectangular hex grid -}
rectangularHexGrid : (Int, Int) -> comparable -> HexGrid comparable
rectangularHexGrid (w, h) a =
    let row x y        = map (\n -> {value = a, coord = {x = n, y = y}}) [0-x..w - 1 - x]
        rowPair offset = [row offset (offset * 2), row offset (offset * 2 + 1)]
    in  Rectangular <| concatMap (\n -> rowPair n) [0..(ceiling <| (toFloat h) / 2) - 1]

{-| Create an empty hexagonal hex grid -}
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

{-| Given a hex size in pixels, a Dict mapping hex values to Shaper types, and a HexGrid,
draw the HexGrid as a visual Element -}
showHexGrid : Float -> Dict.Dict comparable Shaper -> HexGrid comparable -> Element
showHexGrid r dict grid =
    case grid of
        Rectangular hs -> flow down . map asText . map (\row -> map toTuple row) <| hs
        Hexagonal   hs -> let position {x, y} r = move (((sqrt 3) * r * (toFloat x) + ((sqrt 3)/2) * r * (toFloat y)), (-1.5 * r * (toFloat y)))
                              drawHex coord r v = position coord r . rotate (degrees 30) . makeForm (Dict.findWithDefault (SOutlined defaultLine) v dict) . ngon 6 <| r
                              w = let w' = round <| (sqrt 3) / 2 * r
                                  in 2 * (w' + (length hs) * w')
                              h = round <| r * (toFloat <| length hs) * 2.5 + r
                          in  collage w h <| map (\hex -> drawHex hex.coord r hex.value) (concat hs)

{-| Given a hex size, and an onscreen pixel in (x, y) format, return the coordinate of the `Hex` this pixel is inside.
This is useful for mouse interaction. Assumes the HexGrid is visually centered in the window. -}
pixelToHexCoord : Float -> (Int, Int) -> HexCoord
pixelToHexCoord s (x, y) =
    let x' = toFloat (x)/(sqrt(3)*s)
        y' = toFloat (y)/s
        r = floor <| toFloat((floor <| (y' - x') + 1) + (floor <| (y' + x') + 1))/3
        q = floor <| toFloat((floor <| (2 * x') + 1) - r)/2
    in HexCoord q r

makeForm : Shaper -> Shape -> Form
makeForm shaper shape =
    case shaper of
        SColor c    -> filled c shape
        STextured s -> textured s shape
        SGradient g -> gradient g shape
        SOutlined l -> outlined l shape

{-| Tests a `HexCoord` to see if it is inside the `HexGrid` -}
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

{-| Given a `HexCoord`, return all immediately surrounding `HexCoord`s in a list -}
neighbors : HexCoord -> [HexCoord]
neighbors {x, y} = [ HexCoord (x + 1) y, HexCoord (x + 1) (y - 1), HexCoord x (y - 1)
                   , HexCoord (x - 1) y, HexCoord (x - 1) (y + 1), HexCoord x (y + 1) ]

{-| Given a `HexCoord`, return all `HexCoord`s "diagonal" to it in a list -}
diagonals : HexCoord -> [HexCoord]
diagonals {x, y} = [ HexCoord (x + 2) (y - 1), HexCoord (x + 1) (y - 2), HexCoord (x - 1) (y - 1)
                   , HexCoord (x - 2) (y + 1), HexCoord (x - 1) (y + 2), HexCoord (x + 1) (y + 1) ]

{-| Get the straight line distance between two `HexCoord`s -}
distance : HexCoord -> HexCoord -> Int
distance coord1 coord2 =
    let (x1, y1) = (coord1.x, coord1.y)
        (x2, y2) = (coord2.x, coord2.y)
        (z1, z2) = (-(x1 - y1), -(x2 - y2))
    in  maximum [abs (x1 - x2), abs (y1 - y2), abs (z1 - z2)]

{-| Return the list of `HexCoord`s that form the shortest straight line between two `HexCoord`s -}
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

{-| Return a list of all `HexCoord`s within a given distance of a given `HexCoord` -}
range : HexCoord -> Int -> [HexCoord]
range {x, y} n =
    concatMap (\dx ->
        map (\dy ->
            HexCoord (x + dx) (y + dy))
        [max -n (-dx - n)..min n (-dx + n)])
    [-n..n]

{-| Return a list of `HexCoord`s that form a ring shape of size n around a given `HexCoord` -}
ring : HexCoord -> Int -> [HexCoord]
ring {x, y} r =
    let h = HexCoord (x - r) (y + r) -- move southwest r tiles
    in  if | r < 0 -> []
           | otherwise ->
                 scanl (\i h' ->
                     neighbor h' i)
                     h
                     (concatMap (\j -> repeat r j) [0..5])

{-| Return the neighbor of a given `HexCoord` immediately in the given direction. Directions are an `Int` from 0 to 5.
0 is immediately east, and they move counterclockwise from there. -}
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

{-| Replace the value of the `Hex` at a given `HexCoord` -}
insert : HexCoord -> comparable -> HexGrid comparable -> Maybe (HexGrid comparable)
insert ({x, y} as coord) z grid = if not <| inGrid coord grid then Nothing else
    case grid of
        Rectangular hs -> Just grid
        Hexagonal   hs -> let radius = (length hs) `div` 2
                              row    = head . drop (y + radius) <| hs
                              row'   = replace (x + radius + (min 0 y)) (Hex z (HexCoord x y)) row
                          in  Just . Hexagonal <| replace (y + radius) row' hs

{-| Like `insert`, but returns the original grid if given an invalid `HexCoord` instead of wrapping the result
in a `Maybe` type. -}
insertIfPossible : HexCoord -> comparable -> HexGrid comparable -> HexGrid comparable
insertIfPossible ({x, y} as coord) z grid = if not <| inGrid coord grid then grid else
    case grid of
        Rectangular hs -> grid
        Hexagonal hs   -> let radius = (length hs) `div` 2
                              row    = head . drop (y + radius) <| hs
                              row'   = replace (x + radius + (min 0 y)) (Hex z (HexCoord x y)) row
                          in  Hexagonal <| replace (y + radius) row' hs
