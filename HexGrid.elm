import Public.Preface.Preface (replace, splitAt)

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

inGrid : HexCoord -> HexGrid a -> Bool
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

insert : HexCoord -> a -> HexGrid a -> Maybe (HexGrid a)
insert ({x, y} as coord) z grid = if (not <| inGrid coord grid) then Nothing else
    case grid of
        Rectangular hs -> Just grid
        Hexagonal   hs -> let radius = (length hs) `div` 2
                              row    = head . drop (y + radius) <| hs
                              row'   = replace (x + radius + (min 0 y)) (Hex z (HexCoord x y)) row
                          in  Just <| Hexagonal <| replace (y + radius) row' hs 

main = showHexGrid . maybe (Hexagonal [[]]) id . insert (HexCoord 0 3) False <| hexagonalHexGrid 3 True
