module Tests.TestCases where

import Dict

import open ElmTest.Test
import open ElmTest.Assertion

import open HexGrid

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

testBinaryFunction : (a -> b -> c) -> [(String, (a, b), c)] -> [Test]
testBinaryFunction f = map (\(name, (a, b), expected) -> test name <| assertEqual (f a b) expected)

testGridFunction : (a -> b -> HexGrid c) -> [(String, (a, b), HexGrid c)] -> [Test]
testGridFunction f = map (\(name, (a, b), expected) -> test name . assert <| (f a b) `gridEqual` expected)

testUnaryFunction : (a -> b) -> [(String, a, b)] -> [Test]
testUnaryFunction f = map (\(name, a, expected) -> test name <| assertEqual (f a) expected)

rectangularHexGridTests : [Test]
rectangularHexGridTests = testGridFunction rectangularHexGrid
    [ ("Rectangular - 0", ((1, 1), 0),
        Rectangular (1,1) (Dict.fromList [((0,0),0)]))
    , ("Rectangular - 1", ((1, 1), 1),
        Rectangular (1,1) (Dict.fromList [((0,0),1)]))
    , ("Rectangular - Negative", ((1, 1), -1),
        Rectangular (1,1) (Dict.fromList [((0,0),-1)]))
    , ("Rectangular - Width 2", ((2, 2), 0),
        Rectangular (2,2) (Dict.fromList [((-1,-1),0),((-1,0),0),((0,-1),0),((0,0),0)]))
    , ("Rectangular - Width 0", ((0, 0), 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular - Height 0", ((0, 0), 0),
        Rectangular (0, 0) <| Dict.fromList [])
    , ("Rectangular - Height 3", ((3, 3), 0),
        Rectangular (3,3) (Dict.fromList [((-2,1),0),((-1,-1),0),((-1,0),0),((-1,1),0),((0,-1),0),((0,0),0),((0,1),0),((1,-1),0),((1,0),0)]))
    , ("Rectangular - Large value", ((1, 1), 2^32),
        Rectangular (1,1) (Dict.fromList [((0,0),2^32)]))
    ]

hexagonalHexGridTests : [Test]
hexagonalHexGridTests = testGridFunction hexagonalHexGrid
    [ ("Hexagonal - 0", (1, 0),
        Hexagonal 1 (Dict.fromList [((-1,0),0),((-1,1),0),((0,-1),0),((0,0),0),((0,1),0),((1,-1),0),((1,0),0)]))
    , ("Hexagonal - 1", (1, 1),
        Hexagonal 1 (Dict.fromList [((-1,0),1),((-1,1),1),((0,-1),1),((0,0),1),((0,1),1),((1,-1),1),((1,0),1)]))
    , ("Hexagonal - Radius 2", (2, 0),
        Hexagonal 2 (Dict.fromList [((-2,0),0),((-2,1),0),((-2,2),0),((-1,-1),0),((-1,0),0),((-1,1),0),((-1,2),0),((0,-2),0),((0,-1),0),((0,0),0),((0,1),0),((0,2),0),((1,-2),0),((1,-1),0),((1,0),0),((1,1),0),((2,-2),0),((2,-1),0),((2,0),0)]))
    , ("Hexagonal - Radius 0", (0, 0),
        Hexagonal 0 (Dict.fromList [((0,0),0)]))
    , ("Hexagonal - Radius 5", (5, 0),
        Hexagonal 5 (Dict.fromList [((-5,0),0),((-5,1),0),((-5,2),0),((-5,3),0),((-5,4),0),((-5,5),0),((-4,-1),0),((-4,0),0),((-4,1),0),((-4,2),0),((-4,3),0),((-4,4),0),((-4,5),0),((-3,-2),0),((-3,-1),0),((-3,0),0),((-3,1),0),((-3,2),0),((-3,3),0),((-3,4),0),((-3,5),0),((-2,-3),0),((-2,-2),0),((-2,-1),0),((-2,0),0),((-2,1),0),((-2,2),0),((-2,3),0),((-2,4),0),((-2,5),0),((-1,-4),0),((-1,-3),0),((-1,-2),0),((-1,-1),0),((-1,0),0),((-1,1),0),((-1,2),0),((-1,3),0),((-1,4),0),((-1,5),0),((0,-5),0),((0,-4),0),((0,-3),0),((0,-2),0),((0,-1),0),((0,0),0),((0,1),0),((0,2),0),((0,3),0),((0,4),0),((0,5),0),((1,-5),0),((1,-4),0),((1,-3),0),((1,-2),0),((1,-1),0),((1,0),0),((1,1),0),((1,2),0),((1,3),0),((1,4),0),((2,-5),0),((2,-4),0),((2,-3),0),((2,-2),0),((2,-1),0),((2,0),0),((2,1),0),((2,2),0),((2,3),0),((3,-5),0),((3,-4),0),((3,-3),0),((3,-2),0),((3,-1),0),((3,0),0),((3,1),0),((3,2),0),((4,-5),0),((4,-4),0),((4,-3),0),((4,-2),0),((4,-1),0),((4,0),0),((4,1),0),((5,-5),0),((5,-4),0),((5,-3),0),((5,-2),0),((5,-1),0),((5,0),0)]))
    , ("Hexagonal - Negative", (0, -1),
        Hexagonal 0 (Dict.fromList [((0,0),-1)]))
    , ("Hexagonal - Large value", (0, 2^32),
        Hexagonal 0 (Dict.fromList [((0,0),2^32)]))
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
        assertEqual (show <| showHexGrid 1 style <| rectangularHexGrid (1, 1) 0) "<internal structure>"
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
    let rectGrid = rectangularHexGrid (5, 5) 0
        hexGrid  = hexagonalHexGrid 5 0
    in testBinaryFunction inGrid
    [ ("inGrid - Rectangular Pass", (hexCoord 0 0, rectGrid), True)
    , ("inGrid - Rectangular Pass (not Origin)", (hexCoord 1 1, rectGrid), True)
    , ("inGrid - Rectangular Fail", (hexCoord 100 0, rectGrid), False)
    , ("inGrid - Hexagonal Pass", (hexCoord 0 0, hexGrid), True)
    , ("inGrid - Hexagonal Pass (not Origin)", (hexCoord 2 -3, hexGrid), True)
    , ("inGrid - Hexagonal Fail", (hexCoord 0 100, hexGrid), False)
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
    , ("distance - Diagonal", (hexCoord 0 0, hexCoord 5 7), 12)
    , ("distance - Negative", (hexCoord 0 0, hexCoord -3 -3), 6)
    ]

-- line is not actually correctly implemented yet
lineTests : [Test]
lineTests = testBinaryFunction line
    [ ("line - 0", (hexCoord 0 0, hexCoord 0 0), [])
    , ("line - From Origin", (hexCoord 0 0, hexCoord 2 0),
        [hexCoord 0 0, hexCoord 1 0, hexCoord 2 0])
    , ("line - No Origin", (hexCoord 10 10, hexCoord 10 12),
        [hexCoord 10 10, hexCoord 10 11, hexCoord 10 12])
    , ("line - Horizontal", (hexCoord 0 0, hexCoord 3 0),
        [hexCoord 0 0, hexCoord 1 0, hexCoord 2 0, hexCoord 3 0])
    , ("line - Vertcal", (hexCoord 0 0, hexCoord 0 3),
        [hexCoord 0 0, hexCoord 0 1, hexCoord 0 2, hexCoord 0 3])
    , ("line - Diagonal", (hexCoord 0 0, hexCoord 3 3),
        [hexCoord 0 0, hexCoord 1 0, hexCoord 1 1, hexCoord 2 1, hexCoord 2 2, hexCoord 3 2, hexCoord 3 3])
    , ("line - Negative", (hexCoord 0 0, hexCoord -2 -2),
        [hexCoord 0 0, hexCoord 0 -1, hexCoord -1 -1, hexCoord -1 -2, hexCoord -2 -2])
    ]

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
    [ test "insert - Origin" . assert <| gridEqual (maybe grid id <| insert (hexCoord 0 0) 1 grid) <|
        Hexagonal 1 (Dict.fromList [((-1,0),0),((-1,1),0),((0,-1),0),((0,0),1),((0,1),0),((1,-1),0),((1,0),0)])
    , test "insert - Not Origin" . assert <| gridEqual (maybe grid id <| insert (hexCoord 0 1) 1 grid) <|
        Hexagonal 1 (Dict.fromList [((-1,0),0),((-1,1),0),((0,-1),0),((0,0),0),((0,1),1),((1,-1),0),((1,0),0)])
    , test "insert - Outside Grid" <| assertEqual (insert (hexCoord 10 10) 1 grid) Nothing
    , test "insert - Negative" . assert <| gridEqual (maybe grid id <| insert (hexCoord -1 0) 1 grid) <|
         Hexagonal 1 (Dict.fromList [((-1,0),1),((-1,1),0),((0,-1),0),((0,0),0),((0,1),0),((1,-1),0),((1,0),0)])
    ]

-- TODO: Rectangular Hex Grid tests
insertIfPossibleTests : [Test]
insertIfPossibleTests =
    let grid = hexagonalHexGrid 1 0 in
    [ test "insertIfPossible - Origin" . assert <| gridEqual (insertIfPossible (hexCoord 0 0) 1 grid) <|
        Hexagonal 1 (Dict.fromList [((-1,0),0),((-1,1),0),((0,-1),0),((0,0),1),((0,1),0),((1,-1),0),((1,0),0)])
    , test "insertIfPossible - Not Origin" . assert <| gridEqual (insertIfPossible (hexCoord 0 1) 1 grid) <|
        Hexagonal 1 (Dict.fromList [((-1,0),0),((-1,1),0),((0,-1),0),((0,0),0),((0,1),1),((1,-1),0),((1,0),0)])
    , test "insertIfPossible - Outside Grid" . assert <| gridEqual (insertIfPossible (hexCoord 10 10) 1 grid) grid
    , test "insertIfPossible - Negative" . assert <| gridEqual (insertIfPossible (hexCoord -1 0) 1 grid) <|
        Hexagonal 1 (Dict.fromList [((-1,0),1),((-1,1),0),((0,-1),0),((0,0),0),((0,1),0),((1,-1),0),((1,0),0)])
    ]

axialToCubeTests : [Test]
axialToCubeTests = testUnaryFunction axialToCube
    [ ("axialToCube -  0,  0", hexCoord  0  0, ( 0,  0,  0))
    , ("axialToCube -  0, -1", hexCoord  0 -1, ( 0, -1,  1))
    , ("axialToCube -  1, -1", hexCoord  1 -1, ( 1, -1,  0))
    , ("axialToCube -  1,  0", hexCoord  1  0, ( 1,  0, -1))
    , ("axialToCube -  0,  1", hexCoord  0  1, ( 0,  1, -1))
    , ("axialToCube - -1,  1", hexCoord -1  1, (-1,  1,  0))
    , ("axialToCube - -1,  0", hexCoord -1  0, (-1,  0,  1))
    , ("axialToCube -  0, -3", hexCoord  0 -3, ( 0, -3,  3))
    , ("axialToCube -  3, -3", hexCoord  3 -3, ( 3, -3,  0))
    , ("axialToCube -  0, -3", hexCoord  0  3, ( 0,  3, -3))
    , ("axialToCube - -3,  3", hexCoord -3  3, (-3,  3,  0))
    , ("axialToCube - -3,  0", hexCoord -3  0, (-3,  0,  3))
    , ("axialToCube -  3,  0", hexCoord  3  0, ( 3,  0, -3))
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
     ++ lineTests -- This function still needs to be implemented correctly!
     ++ rangeTests
     ++ ringTests
     ++ insertTests
     ++ insertIfPossibleTests
     ++ axialToCubeTests
