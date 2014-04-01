module HexGrid where

{-| A library for creating and working with Hexagonal grids. The grids may be either rectangular or hexagonal in shape.
A HexGrid must be of type `HexGrid a` which means each Hex must contain a value of any a type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists of a types.

Currently all hexes are pointy-top.

# Build
@docs rectangularHexGrid, hexagonalHexGrid, insert, insertIfPossible

# Process
@docs neighbor, neighbors, ring, range, diagonals, line, distance, inGrid

# Interact
@docs showHexGrid, pixelToHexCoord

-}

import Dict
import Set

data HexGrid a = Rectangular Size (Dict.Dict HexCoord a) | Hexagonal Radius (Dict.Dict HexCoord a)
type HexCoord  = (Int, Int)
type Size = (Int, Int)
type Radius = Int

data Direction = Left | Right

hexCoord : Int -> Int -> HexCoord
hexCoord x y = (x, y)

data Shaper = SColor Color | STextured String | SGradient Gradient | SOutlined LineStyle

{-| Create an empty rectangular hex grid -}
rectangularHexGrid : (Int, Int) -> a -> HexGrid a
rectangularHexGrid s a =
    let w = fst s
        h = snd s
        row x y        = map (\n -> (hexCoord n y, a)) [0-x..w - 1 - x]
        rowPair offset = row offset (offset * 2) ++ row offset (offset * 2 + 1)
        hexes = concatMap (\n -> rowPair n) [0..(ceiling <| (toFloat h) / 2) - 1]
        hexes' = filter (\((_, y), _) -> y < h) hexes
        off = ceiling <| (toFloat <| h `div` 2) / 2
    in  Rectangular (w, h) . Dict.fromList <| map (\((x, y), a) -> ((x - off, y - (h `div` 2)), a)) hexes'

{-| Create an empty hexagonal hex grid -}
hexagonalHexGrid : Int -> a -> HexGrid a
hexagonalHexGrid r a =
    let row y = map (\n -> (((n - (min y r)), (y - r)), a)) [0..r*2 - (abs (r - y))]
    in  Hexagonal r . Dict.fromList <| concatMap (\n -> row n) [0..2*r]

{-| Given a hex size in pixels, a Dict mapping hex values to Shaper types, and a HexGrid,
draw the HexGrid as a visual Element -}
showHexGrid : Float -> (a -> Float -> Form) -> HexGrid a -> Element
showHexGrid r former grid =
    case grid of
        --Rectangular hs -> flow down . map asText . map (\row -> map toTuple row) <| hs
        Rectangular (w, h) hs -> let position (x, y) r = move (((sqrt 3) * r * (toFloat x) + ((sqrt 3)/2) * r * (toFloat y)), (-1.5 * r * (toFloat y)))
                                     drawHex (coord, v) r = position coord r . former v <| r
                                     w' = let w'' = round <| (sqrt 3) / 2 * r
                                          in 2 * (w'' + (w * 2 + 1) * w'')
                                     h' = round <| r * (toFloat <| h * 2 + 1) * 2.5 + r
                                     hexes = group <| map (\hex -> drawHex hex r) (Dict.toList hs)
                                 in  collage w' h' [hexes]
        Hexagonal   radius hs -> let position (x, y) r = move (((sqrt 3) * r * (toFloat x) + ((sqrt 3)/2) * r * (toFloat y)), (-1.5 * r * (toFloat y)))
                                     drawHex (coord, v) r = position coord r . former v <| r
                                     w = let w' = round <| (sqrt 3) / 2 * r
                                         in 2 * (w' + (radius * 2 + 1) * w')
                                     h = round <| r * (toFloat <| radius * 2 + 1) * 2.5 + r
                                     hexes = group <| map (\hex -> drawHex hex r) (Dict.toList hs)
                                 in  collage w h [hexes]

{-| Given a hex size, and an onscreen pixel in (x, y) format, return the coordinate of the `Hex` this pixel is inside.
This is useful for mouse interaction. Assumes the HexGrid is visually centered in the window. -}
pixelToHexCoord : Float -> (Int, Int) -> HexCoord
pixelToHexCoord s (x, y) =
    let x' = toFloat (x)/(sqrt(3)*s)
        y' = toFloat (y)/s
        r = floor <| toFloat((floor <| (y' - x') + 1) + (floor <| (y' + x') + 1))/3
        q = floor <| toFloat((floor <| (2 * x') + 1) - r)/2
    in hexCoord q r

{-| Tests a `HexCoord` to see if it is inside the `HexGrid` -}
inGrid : HexCoord -> HexGrid a -> Bool
inGrid ((x, y) as coord) grid =
    case grid of
        Rectangular _    hs -> Dict.member coord hs
        Hexagonal radius hs -> let offset = radius * 2 - (abs y)
                               in  if | y < -radius -> False
                                      | y >  radius -> False
                                      | x + radius + (min 0 y) < 0 -> False
                                      | x > offset - radius - (min 0 y) -> False
                                      | otherwise -> True

valueAt : HexCoord -> HexGrid a -> Maybe a
valueAt coord grid =
    case grid of
        Rectangular _ hs -> Dict.lookup coord hs
        Hexagonal   _ hs -> Dict.lookup coord hs

remove : HexCoord -> HexGrid a -> HexGrid a
remove coord grid =
    case grid of
        Rectangular s hs -> Rectangular s <| Dict.remove coord hs
        Hexagonal   r hs -> Hexagonal   r <| Dict.remove coord hs

{-| Given a `HexCoord`, return all immediately surrounding `HexCoord`s in a list -}
neighbors : HexCoord -> [HexCoord]
neighbors (x, y) = [ hexCoord (x + 1) y, hexCoord (x + 1) (y - 1), hexCoord x (y - 1)
                   , hexCoord (x - 1) y, hexCoord (x - 1) (y + 1), hexCoord x (y + 1) ]

{-| Given a `HexCoord`, return all `HexCoord`s "diagonal" to it in a list -}
diagonals : HexCoord -> [HexCoord]
diagonals (x, y) = [ hexCoord (x + 2) (y - 1), hexCoord (x + 1) (y - 2), hexCoord (x - 1) (y - 1)
                   , hexCoord (x - 2) (y + 1), hexCoord (x - 1) (y + 2), hexCoord (x + 1) (y + 1) ]

{-| Get the straight line distance between two `HexCoord`s -}
distance : HexCoord -> HexCoord -> Int
distance coord1 coord2 =
    let (x1, y1, z1) = axialToCube coord1
        (x2, y2, z2) = axialToCube coord2
    in  ((abs <| x1 - x2) + (abs <| y1 - y2) + (abs <| z1 - z2)) `div` 2

axialToCube : HexCoord -> (Int, Int, Int)
axialToCube (x, y) = (x, y, -x - y)

{-| Return the list of `HexCoord`s that form the shortest straight line between two `HexCoord`s -}
line : HexCoord -> HexCoord -> [HexCoord]
line coord1 coord2 = if (coord1 == coord2) then [] else
    let n = toFloat <| distance coord1 coord2
        pts = [0..n]
    in  map (\i -> hexRound <| (coord1 `hexMult` (1 - i/n)) `hexAdd` (coord2 `hexMult` (i/n))) pts

hexMult : HexCoord -> Float -> (Float, Float)
hexMult (x, y) n = (toFloat x * n, toFloat y * n)

hexAdd : (number, number) -> (number, number) -> (number, number)
hexAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-| Return a list of all `HexCoord`s within a given distance of a given `HexCoord` -}
range : HexCoord -> Int -> [HexCoord]
range (x, y) n =
    concatMap (\dx ->
        map (\dy ->
            hexCoord (x + dx) (y + dy))
        [max -n (-dx - n)..min n (-dx + n)])
    [-n..n]

rotation : Direction -> HexCoord -> HexCoord
rotation direction coord =
    let (x, y, z) = axialToCube coord
    in  case direction of
        Left  -> hexCoord -z -x
        Right -> hexCoord -y -z

{-| Return a list of `HexCoord`s that form a ring shape of size n around a given `HexCoord` -}
ring : HexCoord -> Int -> [HexCoord]
ring (x, y) r =
    let h = hexCoord (x - r) (y + r) -- move southwest r tiles
    in  if | r < 0 -> []
           | otherwise ->
                 scanl (\i h' ->
                     neighbor h' i)
                     h
                     (concatMap (\j -> repeat r j) [0..5])

{-| Return the neighbor of a given `HexCoord` immediately in the given direction. Directions are an `Int` from 0 to 5.
0 is immediately east, and they move counterclockwise from there. -}
neighbor : HexCoord -> Int -> HexCoord
neighbor (x, y) direction =
    let (dx, dy) = case direction of
                      0 -> (1  ,  0)
                      1 -> (1  , -1)
                      2 -> (0  , -1)
                      3 -> (-1 ,  0)
                      4 -> (-1 ,  1)
                      5 -> (0  ,  1)
                      _ -> (0  ,  0)
    in  hexCoord (x + dx) (y + dy)

hexRound : (Float, Float) -> HexCoord
hexRound (x, y) =
    let z            = -x - y
        (rx, ry, rz) = (round x, round y, round z)
        errX         = abs (rx - x)
        errY         = abs (ry - y)
        errZ         = abs (rz - z)
    in  if | errX > errY && errX > errZ -> ((-ry - rz), ry)
           | errY > errZ                -> (rx, (-rx - rz))
           | otherwise                  -> (rx, ry)

{-| Replace the value of the `Hex` at a given `HexCoord` -}
insert : HexCoord -> a -> HexGrid a -> Maybe (HexGrid a)
insert coord v grid = if not <| inGrid coord grid then Nothing else
    case grid of
        Rectangular s hs -> Just . Rectangular s <| Dict.insert coord v hs
        Hexagonal   r hs -> Just . Hexagonal   r <| Dict.insert coord v hs

{-| Like `insert`, but returns the original grid if given an invalid `HexCoord` instead of wrapping the result
in a `Maybe` type. -}
insertIfPossible : HexCoord -> a -> HexGrid a -> HexGrid a
insertIfPossible coord v grid = if not <| inGrid coord grid then grid else
    maybe grid id <| insert coord v grid

gridEqual : HexGrid a -> HexGrid a -> Bool
gridEqual grid1 grid2 =
    case grid1 of
        Rectangular s hs ->
            case grid2 of
                Rectangular s' hs' -> if | s /= s'                           -> False
                                         | Dict.toList hs /= Dict.toList hs' -> False
                                         | otherwise                         -> True
                _                  -> False
        Hexagonal   r hs ->
            case grid2 of
                Hexagonal   r' hs' -> if | r /= r'                           -> False
                                         | Dict.toList hs /= Dict.toList hs' -> False
                                         | otherwise                         -> True
                _                  -> False
