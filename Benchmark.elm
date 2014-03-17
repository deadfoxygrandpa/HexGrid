module Benchmark where

import Dict
import HexGrid

grid1 = HexGrid.rectangularHexGrid 44 0
grid2 = HexGrid.hexagonalHexGrid   25 0

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

discard _ = 0

port inGridRect : Int -> Int
port inGridRect = \n -> discard . HexGrid.inGrid (HexGrid.hexCoord 10 10) <| grid1

port inGridHex : Int -> Int
port inGridHex = \n -> discard . HexGrid.inGrid (HexGrid.hexCoord 10 10) <| grid2

port rectangularHexGrid : Int -> Int
port rectangularHexGrid = \n -> discard . HexGrid.rectangularHexGrid 44 <| 0

port hexagonalHexGrid : Int -> Int
port hexagonalHexGrid = \n -> discard . HexGrid.hexagonalHexGrid 25 <| 0

port showHexGridRect : Int -> Int
port showHexGridRect = \n -> discard . HexGrid.showHexGrid 25 styler <| grid1

port showHexGridHex : Int -> Int
port showHexGridHex = \n -> discard . HexGrid.showHexGrid 25 styler <| grid2

port pixelToHexCoord : Int -> Int
port pixelToHexCoord = \n -> discard . HexGrid.pixelToHexCoord 25 <| (50, 50)

port neighbors : Int -> Int
port neighbors = \n -> discard . HexGrid.neighbors <| (HexGrid.hexCoord 0 0)

port diagonals : Int -> Int
port diagonals = \n -> discard . HexGrid.diagonals <| (HexGrid.hexCoord 0 0)

port distance : Int -> Int
port distance = \n -> discard . HexGrid.distance (HexGrid.hexCoord 10 19) <| (HexGrid.hexCoord 0 0)

port range1 : Int -> Int
port range1 = \n -> discard . HexGrid.range (HexGrid.hexCoord 0 0) <| 1

port range10 : Int -> Int
port range10 = \n -> discard . HexGrid.range (HexGrid.hexCoord 0 0) <| 10

port ring1 : Int -> Int
port ring1 = \n -> discard . HexGrid.ring (HexGrid.hexCoord 0 0) <| 1

port ring10 : Int -> Int
port ring10 = \n -> discard . HexGrid.ring (HexGrid.hexCoord 0 0) <| 10

port insert : Int -> Int
port insert = \n -> discard . HexGrid.insert (HexGrid.hexCoord 0 0) 1 <| grid1

port insertIfPossible : Int -> Int
port insertIfPossible = \n -> discard . HexGrid.insertIfPossible (HexGrid.hexCoord 0 0) 1 <| grid1
