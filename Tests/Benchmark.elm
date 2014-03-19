module Benchmark where

import Dict
import HexGrid

grid1 = HexGrid.rectangularHexGrid (44, 44) 0
grid2 = HexGrid.hexagonalHexGrid   25       0

smallGrid = HexGrid.hexagonalHexGrid 5 0

toList grid =
    case grid of
        HexGrid.Rectangular _ hs -> Dict.toList hs
        HexGrid.Hexagonal   _ hs -> Dict.toList hs

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

styler = styleGuide 25 6 red

discard _ = ()

port line1 : () -> ()
port line1 = \_ -> discard . HexGrid.line (HexGrid.hexCoord 0 0) <| HexGrid.hexCoord 1 0

port line10 : () -> ()
port line10 = \_ -> discard . HexGrid.line (HexGrid.hexCoord 0 0) <| HexGrid.hexCoord 10 0

port rotation : () -> ()
port rotation = \_ -> discard . HexGrid.rotation HexGrid.Left <| (HexGrid.hexCoord 4 5)

port inGridRect : () -> ()
port inGridRect = \_ -> discard . HexGrid.inGrid (HexGrid.hexCoord 10 10) <| grid1

port inGridHex : () -> ()
port inGridHex = \_ -> discard . HexGrid.inGrid (HexGrid.hexCoord 10 10) <| grid2

port rectangularHexGrid : () -> ()
port rectangularHexGrid = \_ -> discard . HexGrid.rectangularHexGrid (44, 44) <| 0

port hexagonalHexGrid : () -> ()
port hexagonalHexGrid = \_ -> discard . HexGrid.hexagonalHexGrid 25 <| 0

port showHexGridRect : () -> ()
port showHexGridRect = \_ -> discard . HexGrid.showHexGrid 25 styler <| grid1

port showHexGridHex : () -> ()
port showHexGridHex = \_ -> discard . HexGrid.showHexGrid 25 styler <| grid2

port showSmallHexGrid : () -> ()
port showSmallHexGrid = \_ -> discard . HexGrid.showHexGrid 250 styler <| smallGrid

port showSmallHexGridSmaller : () -> ()
port showSmallHexGridSmaller = \_ -> discard . HexGrid.showHexGrid 5 styler <| smallGrid

port pixelToHexCoord : () -> ()
port pixelToHexCoord = \_ -> discard . HexGrid.pixelToHexCoord 25 <| (50, 50)

port neighbors : () -> ()
port neighbors = \_ -> discard . HexGrid.neighbors <| (HexGrid.hexCoord 0 0)

port diagonals : () -> ()
port diagonals = \_ -> discard . HexGrid.diagonals <| (HexGrid.hexCoord 0 0)

port distance : () -> ()
port distance = \_ -> discard . HexGrid.distance (HexGrid.hexCoord 10 19) <| (HexGrid.hexCoord 0 0)

port range1 : () -> ()
port range1 = \_ -> discard . HexGrid.range (HexGrid.hexCoord 0 0) <| 1

port range10 : () -> ()
port range10 = \_ -> discard . HexGrid.range (HexGrid.hexCoord 0 0) <| 10

port ring1 : () -> ()
port ring1 = \_ -> discard . HexGrid.ring (HexGrid.hexCoord 0 0) <| 1

port ring10 : () -> ()
port ring10 = \_ -> discard . HexGrid.ring (HexGrid.hexCoord 0 0) <| 10

port insert : () -> ()
port insert = \_ -> discard . HexGrid.insert (HexGrid.hexCoord 0 0) 1 <| grid1

port insertIfPossible : () -> ()
port insertIfPossible = \_ -> discard . HexGrid.insertIfPossible (HexGrid.hexCoord 0 0) 1 <| grid1
