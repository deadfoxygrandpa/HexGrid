data HexGrid a = Rectangular [[Hex a]] | Hexagonal [[Hex a]]
type Hex a     = {value : a, coord : HexCoord}
type HexCoord  = {x : Int, y : Int}

rectangularHexGrid : (Int, Int) -> a -> HexGrid a
rectangularHexGrid (w, h) a = 
    let row x y        = map (\n -> {value = a, coord = {x = n, y = y}}) [0-x..w - 1 - x]
        rowPair offset = [row offset (offset * 2), row offset (offset * 2 + 1)]
    in  Rectangular <| concatMap (\n -> rowPair n) [0..(ceiling <| (toFloat h) / 2) - 1]

hexagonalHexGrid : Int -> a -> HexGrid a
hexagonalHexGrid r a =
    let row y = map (\n -> {value = a, coord = {x = n - (min y r), y = y - r}}) [0..r*2 - (abs (r - y))]
    in  Hexagonal <| map (\n -> row n) [0..2*r]

toHexList : HexGrid a -> [[Hex a]]
toHexList grid =
    case grid of
        Rectangular hs -> hs
        Hexagonal   hs -> hs
        
toPair : Hex a -> (Int, Int, a)        
toPair hex = (hex.coord.x, hex.coord.y, hex.value)

asText7 a = text . monospace . toText <| (take 7 <| (show a) ++ "  ")

showHexGrid : HexGrid a -> Element
showHexGrid grid =
    case grid of
        Rectangular hs -> flow down . map asText . map (\row -> map toPair row) <| hs
        Hexagonal   hs -> let rows = map (flow right . map asText) . map (\row -> map toPair row) <| hs
                              l    = length hs
                              w    = widthOf <| head (drop (l `div` 2) rows)
                              rows' = map (\row -> container w (heightOf row) middle row) rows
                          in  flow down rows'

main = showHexGrid <| hexagonalHexGrid 3 True -- rectangularHexGrid (6, 6)
