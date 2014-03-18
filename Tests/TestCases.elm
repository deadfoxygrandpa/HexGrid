module Tests.TestCases where

import Dict

import open ElmTest.Test
import open ElmTest.Assertion

import open HexGrid

testBinaryFunction : (a -> b -> c) -> [(String, (a, b), c)] -> [Test]
testBinaryFunction f = map (\(name, (a, b), expected) -> test name <| assertEqual (f a b) expected)

testUnaryFunction : (a -> b) -> [(String, a, b)] -> [Test]
testUnaryFunction f = map (\(name, a, expected) -> test name <| assertEqual (f a) expected)

rectangularHexGridTests : [Test]
rectangularHexGridTests = testBinaryFunction rectangularHexGrid
    [ ("Rectangular (0, 0) - 0", (0, 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - 1", (0, 1.0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - Negative", (0, -1),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - Width 2", (0, 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - Width 0", (0, 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - Height 0", (0, 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - Height 3", (0, 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular (0, 0) - Large value", (0, 2^32),
        Rectangular (0, 0) <| Dict.fromList [])
    ]

hexagonalHexGridTests : [Test]
hexagonalHexGridTests = testBinaryFunction hexagonalHexGrid
    [ ("Hexagonal 0 - 0", (1, 0),
        Hexagonal 0 <| Dict.fromList [])
    , ("Hexagonal 0 - 1", (1, 1),
        Hexagonal 0 <| Dict.fromList [])
    , ("Hexagonal 0 - Radius 2", (2, 0),
        Hexagonal 0 <| Dict.fromList [])
    , ("Hexagonal 0 - Radius 0", (0, 0),
        Hexagonal 0 <| Dict.fromList [])
    , ("Hexagonal 0 - Radius 5", (5, 0),
        Hexagonal 0 <| Dict.fromList [])
    , ("Hexagonal 0 - Negative", (0, -1),
        Hexagonal 0 <| Dict.fromList [])
    , ("Hexagonal 0 - Large value", (0, 2^32),
        Hexagonal 0 <| Dict.fromList [])
    ]

showHexGridTests : [Test]
showHexGridTests =
    let style n =
        let colorize = case n of
                        1 -> filled darkOrange
                        2 -> filled <| rgba 0 0 0 0.4
                        _ -> filled <| rgba 0 0 0 0.5
            shape    = case n of
                        1 -> \_ -> toForm . plainText <| "welp,"
                        2 -> rotate (degrees 30) . colorize . ngon 6
                        _ -> colorize . square--rotate (degrees 30) . colorize . ngon 6
        in shape in
    [ test "showHexGrid - Rectangular" <|
        assertEqual (show <| showHexGrid 1 style <| rectangularHexGrid 1 0) "<internal structure>"
    , test "showHexGrid - Hexagonal" <|
        assertEqual (show <| showHexGrid 1 style <| hexagonalHexGrid 1 0) "<internal structure>"
    ]

pixelToHexCoordTests : [Test]
pixelToHexCoordTests =
    let enumerateVertices name radius (x, y) =
            [ (\expected -> (name          , (radius, (x, y))  , expected))
            , (\expected -> (name ++ " - N", (radius, (x, y-1)), expected))
            , (\expected -> (name ++ " - S", (radius, (x, y+1)), expected))
            , (\expected -> (name ++ " - E", (radius, (x+1, y)), expected))
            , (\expected -> (name ++ " - W", (radius, (x-1, y)), expected))
            ]
    in testBinaryFunction pixelToHexCoord <|
    [ ("pixelToHexCoord - Origin", (1, (0, 0)), hexCoord 0 0)
    , ("pixelToHexCoord - Point 1", (25, (40, 70)), hexCoord 0 2)
    , ("pixelToHexCoord - Point 2", (25, (500, 300)), hexCoord 8 8)
    , ("pixelToHexCoord - Point 3", (25, (-20, -100)), hexCoord 1 -3)
    , ("pixelToHexCoord - Point 4", (25, (0, 200)), hexCoord -3 6)
    , ("pixelToHexCoord - Point 5", (25, (10000, 0)), hexCoord 231 0)
    ]
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 1" 25 (0, -25))
            [hexCoord 0 0, hexCoord 1 -1, hexCoord 0 0, hexCoord 1 -1, hexCoord 0 -1])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 2" 25 (21, -12))
            [hexCoord 0 0, hexCoord 1 -1, hexCoord 0 0, hexCoord 1 0, hexCoord 0 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 3" 25 (21, 12))
            [hexCoord 0 0, hexCoord 0 0, hexCoord 0 1, hexCoord 1 0, hexCoord 0 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 4" 25 (0, 24))
            [hexCoord 0 0, hexCoord 0 0, hexCoord 0 1, hexCoord 0 0, hexCoord 0 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 5" 25 (-21, 12))
            [hexCoord 0 0, hexCoord 0 0, hexCoord -1 1, hexCoord 0 0, hexCoord -1 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 6" 25 (-21, -12))
            [hexCoord 0 0, hexCoord 0 -1, hexCoord 0 0, hexCoord 0 0, hexCoord -1 0])

inGridTests : [Test]
inGridTests =
    let rectGrid = rectangularHexGrid 0 0
        hexGrid  = hexagonalHexGrid 5 0
    in testBinaryFunction inGrid
    [ ("inGrid - Rectangular (0, 0) Pass", (hexCoord 0 0, rectGrid), True)
    , ("inGrid - Rectangular (0, 0) Pass (not Origin)", (hexCoord 3 2, rectGrid), True)
    , ("inGrid - Rectangular (0, 0) Fail", (hexCoord 100 0, rectGrid), False)
    , ("inGrid - Hexagonal 0 Pass", (hexCoord 0 0, hexGrid), True)
    , ("inGrid - Hexagonal 0 Pass (not Origin)", (hexCoord 2 -3, hexGrid), True)
    , ("inGrid - Hexagonal 0 Fail", (hexCoord 0 100, hexGrid), False)
    ]

neighborsTests : [Test]
neighborsTests = testUnaryFunction neighbors
    [ ("neighbors - Origin", hexCoord 0 0,
        [hexCoord 1 0, hexCoord 1 -1, hexCoord 0 -1, hexCoord -1 0, hexCoord -1 1, hexCoord 0 1])
    , ("neighbors - Not Origin", hexCoord -5 10,
        [hexCoord -4 10, hexCoord -4 9, hexCoord -5 9, hexCoord -6 10, hexCoord -6 11, hexCoord -5 11])
    ]

diagonalsTests : [Test]
diagonalsTests = testUnaryFunction diagonals
    [ ("diagonals - Origin", hexCoord 0 0,
        [hexCoord 2 -1, hexCoord 1 -2, hexCoord -1 -1, hexCoord -2 1, hexCoord -1 2, hexCoord 1 1])
    , ("diagonals - Not Origin", hexCoord 3 7,
        [hexCoord 5 6, hexCoord 4 5, hexCoord 2 6, hexCoord 1 8, hexCoord 2 9, hexCoord 4 8])
    ]

distanceTests : [Test]
distanceTests = testBinaryFunction distance
    [ ("distance - 0", (hexCoord 0 0, hexCoord 0 0), 0)
    , ("distance - From Origin", (hexCoord 0 0, hexCoord 5 0), 5)
    , ("distance - No Origin", (hexCoord -3 0, hexCoord 5 0), 8)
    , ("distance - Horizontal", (hexCoord 0 0, hexCoord 10 0), 10)
    , ("distance - Vertical", (hexCoord 0 0, hexCoord 0 10), 10)
    , ("distance - Diagonal", (hexCoord 0 0, hexCoord 5 7), 7)
    , ("distance - Negative", (hexCoord 0 0, hexCoord -3 -3), 3)
    ]

-- line is not actually correctly implemented yet
--lineTests : [Test]
--lineTests = testBinaryFunction line
--    [ ("line - 0", (hexCoord 0 0, hexCoord 0 0), [])
--    , ("line - From Origin", (hexCoord 0 0, hexCoord 2 0),
--        [hexCoord 0 0, hexCoord 1 0, hexCoord 2 0])
--    , ("line - No Origin", (hexCoord 10 10, hexCoord 10 12),
--        [hexCoord 10 10, hexCoord 10 11, hexCoord 10 12])
--    , ("line - Horizontal", (hexCoord 0 0, hexCoord 3 0),
--        [hexCoord 0 0, hexCoord 1 0, hexCoord 2 0, hexCoord 3 0])
--    , ("line - Vertcal", (hexCoord 0 0, hexCoord 0 3),
--        [hexCoord 0 0, hexCoord 0 1, hexCoord 0 2, hexCoord 0 3])
--    , ("line - Diagonal", (hexCoord 0 0, hexCoord 3 3),
--        [hexCoord 0 0, hexCoord 1 1, hexCoord 2 2, hexCoord 3 3])
--    , ("line - Negative", (hexCoord 0 0, hexCoord -2 -2),
--        [hexCoord 0 0, hexCoord -1 -1, hexCoord -2 -2])
--    ]

rangeTests : [Test]
rangeTests = testBinaryFunction range
    [ ("range - 0", (hexCoord 0 0, 0),
        [hexCoord 0 0])
    , ("range - 1", (hexCoord 0 0, 1),
        [hexCoord -1 0, hexCoord -1 1, hexCoord 0 -1, hexCoord 0 0, hexCoord 0 1, hexCoord 1 -1, hexCoord 1 0])
    , ("range - 2", (hexCoord 0 0, 2),
        [hexCoord -2 0, hexCoord -2 1, hexCoord -2 2, hexCoord -1 -1, hexCoord -1 0, hexCoord -1 1, hexCoord -1 2, hexCoord 0 -2, hexCoord 0 -1, hexCoord 0 0, hexCoord 0 1, hexCoord 0 2, hexCoord 1 -2, hexCoord 1 -1, hexCoord 1 0, hexCoord 1 1, hexCoord 2 -2, hexCoord 2 -1, hexCoord 2 0])
    , ("range - Not Origin", (hexCoord 5 5, 1),
        [hexCoord 4 5, hexCoord 4 6, hexCoord 5 4, hexCoord 5 5, hexCoord 5 6, hexCoord 6 4, hexCoord 6 5])
    , ("range - Negative", (hexCoord 0 0, -2), [])
    ]

ringTests : [Test]
ringTests = testBinaryFunction ring
    [ ("ring - 0", (hexCoord 0 0, 0),
        [hexCoord 0 0])
    , ("ring - 1", (hexCoord 0 0, 1),
        [hexCoord -1 1, hexCoord 0 1, hexCoord 1 0, hexCoord 1 -1, hexCoord 0 -1, hexCoord -1 0, hexCoord -1 1])
    , ("ring - 2", (hexCoord 0 0, 2),
         [hexCoord -2 2, hexCoord -1 2, hexCoord 0 2, hexCoord 1 1, hexCoord 2 0, hexCoord 2 -1, hexCoord 2 -2, hexCoord 1 -2, hexCoord 0 -2, hexCoord -1 -1, hexCoord -2 0, hexCoord -2 1, hexCoord -2 2])
    , ("ring - Not Origin", (hexCoord 5 5, 1),
        [hexCoord 4 6, hexCoord 5 6, hexCoord 6 5, hexCoord 6 4, hexCoord 5 4, hexCoord 4 5, hexCoord 4 6])
    , ("ring - Negative", (hexCoord 0 0, -2), [])
    ]

-- TODO: Rectangular Hex Grid tests
insertTests : [Test]
insertTests =
    let grid = hexagonalHexGrid 1 0 in
    [ test "insert - Origin" <| assertEqual (insert (hexCoord 0 0) 1 grid) <|
        Just (Hexagonal 0 <| Dict.fromList [])
    , test "insert - Not Origin" <| assertEqual (insert (hexCoord 0 1) 1 grid) <|
        Just (Hexagonal 0 <| Dict.fromList [])
    , test "insert - Outside Grid" <| assertEqual (insert (hexCoord 10 10) 1 grid) Nothing
    , test "insert - Negative" <| assertEqual (insert (hexCoord -1 0) 1 grid) <|
        Just (Hexagonal 0 <| Dict.fromList [])
    ]

-- TODO: Rectangular Hex Grid tests
insertIfPossibleTests : [Test]
insertIfPossibleTests =
    let grid = hexagonalHexGrid 1 0 in
    [ test "insertIfPossible - Origin" <| assertEqual (insertIfPossible (hexCoord 0 0) 1 grid) <|
        Hexagonal 0 <| Dict.fromList []
    , test "insertIfPossible - Not Origin" <| assertEqual (insertIfPossible (hexCoord 0 1) 1 grid) <|
        Hexagonal 0 <| Dict.fromList []
    , test "insertIfPossible - Outside Grid" <| assertEqual (insertIfPossible (hexCoord 10 10) 1 grid) grid
    , test "insertIfPossible - Negative" <| assertEqual (insertIfPossible (hexCoord -1 0) 1 grid) <|
        Hexagonal 0 <| Dict.fromList []
    ]

tests : [Test]
tests = rectangularHexGridTests
     ++ hexagonalHexGridTests
     ++ showHexGridTests
     ++ pixelToHexCoordTests
     ++ inGridTests
     ++ neighborsTests
     ++ diagonalsTests
     ++ distanceTests
     --++ lineTests -- This function still needs to be implemented correctly!
     ++ rangeTests
     ++ ringTests
     ++ insertTests
     ++ insertIfPossibleTests
