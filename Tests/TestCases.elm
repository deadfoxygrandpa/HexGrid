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
    [ ("Rectangular - 0", ((1, 1), 0),
        Rectangular ([[{ coord = HexCoord 0 0, value = 0 }],[{ coord = HexCoord 0 1, value = 0 }]]))
    , ("Rectangular - 1", ((1, 1), 1.0),
        Rectangular ([[{ coord = HexCoord 0 0, value = 1 }],[{ coord = HexCoord 0 1, value = 1 }]]))
    , ("Rectangular - Negative", ((1,1), -1),
        Rectangular ([[{ coord = HexCoord 0 0, value = -1 }],[{ coord = HexCoord 0 1, value = -1 }]]))
    , ("Rectangular - Width 2", ((2,1), 0),
        Rectangular ([[{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord 0 1, value = 0 },{ coord = HexCoord 1 1, value = 0 }]]))
    , ("Rectangular - Width 0", ((0,1), 0),
        Rectangular [[],[]])
    , ("Rectangular - Height 0", ((1,0), 0),
        Rectangular [])
    , ("Rectangular - Height 3", ((5,3), 0),
        Rectangular ([[{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 },{ coord = HexCoord 2 0, value = 0 },{ coord = HexCoord 3 0, value = 0 },{ coord = HexCoord 4 0, value = 0 }],[{ coord = HexCoord 0 1, value = 0 },{ coord = HexCoord 1 1, value = 0 },{ coord = HexCoord 2 1, value = 0 },{ coord = HexCoord 3 1, value = 0 },{ coord = HexCoord 4 1, value = 0 }],[{ coord = HexCoord -1 2, value = 0 },{ coord = HexCoord 0 2, value = 0 },{ coord = HexCoord 1 2, value = 0 },{ coord = HexCoord 2 2, value = 0 },{ coord = HexCoord 3 2, value = 0 }],[{ coord = HexCoord -1 3, value = 0 },{ coord = HexCoord 0 3, value = 0 },{ coord = HexCoord 1 3, value = 0 },{ coord = HexCoord 2 3, value = 0 },{ coord = HexCoord 3 3, value = 0 }]]))
    , ("Rectangular - Large value", ((1, 1), 2^32),
        Rectangular ([[{ coord = HexCoord 0 0, value = 2^32 }],[{ coord = HexCoord 0 1, value = 2^32 }]]))
    ]

hexagonalHexGridTests : [Test]
hexagonalHexGridTests = testBinaryFunction hexagonalHexGrid
    [ ("Hexagonal - 0", (1, 0),
        Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 }]]))
    , ("Hexagonal - 1", (1, 1),
        Hexagonal ([[{ coord = HexCoord 0 -1, value = 1 },{ coord = HexCoord 1 -1, value = 1 }],[{ coord = HexCoord -1 0, value = 1 },{ coord = HexCoord 0 0, value = 1 },{ coord = HexCoord 1 0, value = 1 }],[{ coord = HexCoord -1 1, value = 1 },{ coord = HexCoord 0 1, value = 1 }]]))
    , ("Hexagonal - Radius 2", (2, 0),
        Hexagonal ([[{ coord = HexCoord 0 -2, value = 0 },{ coord = HexCoord 1 -2, value = 0 },{ coord = HexCoord 2 -2, value = 0 }],[{ coord = HexCoord -1 -1, value = 0 },{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 },{ coord = HexCoord 2 -1, value = 0 }],[{ coord = HexCoord -2 0, value = 0 },{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 },{ coord = HexCoord 2 0, value = 0 }],[{ coord = HexCoord -2 1, value = 0 },{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 },{ coord = HexCoord 1 1, value = 0 }],[{ coord = HexCoord -2 2, value = 0 },{ coord = HexCoord -1 2, value = 0 },{ coord = HexCoord 0 2, value = 0 }]]))
    , ("Hexagonal - Radius 0", (0, 0),
        Hexagonal ([[{ coord = HexCoord 0 0, value = 0 }]]))
    , ("Hexagonal - Radius 5", (5, 0),
        Hexagonal ([[{ coord = HexCoord 0 -5, value = 0 },{ coord = HexCoord 1 -5, value = 0 },{ coord = HexCoord 2 -5, value = 0 },{ coord = HexCoord 3 -5, value = 0 },{ coord = HexCoord 4 -5, value = 0 },{ coord = HexCoord 5 -5, value = 0 }],[{ coord = HexCoord -1 -4, value = 0 },{ coord = HexCoord 0 -4, value = 0 },{ coord = HexCoord 1 -4, value = 0 },{ coord = HexCoord 2 -4, value = 0 },{ coord = HexCoord 3 -4, value = 0 },{ coord = HexCoord 4 -4, value = 0 },{ coord = HexCoord 5 -4, value = 0 }],[{ coord = HexCoord -2 -3, value = 0 },{ coord = HexCoord -1 -3, value = 0 },{ coord = HexCoord 0 -3, value = 0 },{ coord = HexCoord 1 -3, value = 0 },{ coord = HexCoord 2 -3, value = 0 },{ coord = HexCoord 3 -3, value = 0 },{ coord = HexCoord 4 -3, value = 0 },{ coord = HexCoord 5 -3, value = 0 }],[{ coord = HexCoord -3 -2, value = 0 },{ coord = HexCoord -2 -2, value = 0 },{ coord = HexCoord -1 -2, value = 0 },{ coord = HexCoord 0 -2, value = 0 },{ coord = HexCoord 1 -2, value = 0 },{ coord = HexCoord 2 -2, value = 0 },{ coord = HexCoord 3 -2, value = 0 },{ coord = HexCoord 4 -2, value = 0 },{ coord = HexCoord 5 -2, value = 0 }],[{ coord = HexCoord -4 -1, value = 0 },{ coord = HexCoord -3 -1, value = 0 },{ coord = HexCoord -2 -1, value = 0 },{ coord = HexCoord -1 -1, value = 0 },{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 },{ coord = HexCoord 2 -1, value = 0 },{ coord = HexCoord 3 -1, value = 0 },{ coord = HexCoord 4 -1, value = 0 },{ coord = HexCoord 5 -1, value = 0 }],[{ coord = HexCoord -5 0, value = 0 },{ coord = HexCoord -4 0, value = 0 },{ coord = HexCoord -3 0, value = 0 },{ coord = HexCoord -2 0, value = 0 },{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 },{ coord = HexCoord 2 0, value = 0 },{ coord = HexCoord 3 0, value = 0 },{ coord = HexCoord 4 0, value = 0 },{ coord = HexCoord 5 0, value = 0 }],[{ coord = HexCoord -5 1, value = 0 },{ coord = HexCoord -4 1, value = 0 },{ coord = HexCoord -3 1, value = 0 },{ coord = HexCoord -2 1, value = 0 },{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 },{ coord = HexCoord 1 1, value = 0 },{ coord = HexCoord 2 1, value = 0 },{ coord = HexCoord 3 1, value = 0 },{ coord = HexCoord 4 1, value = 0 }],[{ coord = HexCoord -5 2, value = 0 },{ coord = HexCoord -4 2, value = 0 },{ coord = HexCoord -3 2, value = 0 },{ coord = HexCoord -2 2, value = 0 },{ coord = HexCoord -1 2, value = 0 },{ coord = HexCoord 0 2, value = 0 },{ coord = HexCoord 1 2, value = 0 },{ coord = HexCoord 2 2, value = 0 },{ coord = HexCoord 3 2, value = 0 }],[{ coord = HexCoord -5 3, value = 0 },{ coord = HexCoord -4 3, value = 0 },{ coord = HexCoord -3 3, value = 0 },{ coord = HexCoord -2 3, value = 0 },{ coord = HexCoord -1 3, value = 0 },{ coord = HexCoord 0 3, value = 0 },{ coord = HexCoord 1 3, value = 0 },{ coord = HexCoord 2 3, value = 0 }],[{ coord = HexCoord -5 4, value = 0 },{ coord = HexCoord -4 4, value = 0 },{ coord = HexCoord -3 4, value = 0 },{ coord = HexCoord -2 4, value = 0 },{ coord = HexCoord -1 4, value = 0 },{ coord = HexCoord 0 4, value = 0 },{ coord = HexCoord 1 4, value = 0 }],[{ coord = HexCoord -5 5, value = 0 },{ coord = HexCoord -4 5, value = 0 },{ coord = HexCoord -3 5, value = 0 },{ coord = HexCoord -2 5, value = 0 },{ coord = HexCoord -1 5, value = 0 },{ coord = HexCoord 0 5, value = 0 }]]))
    , ("Hexagonal - Negative", (0, -1),
        Hexagonal ([[{ coord = HexCoord 0 0, value = -1 }]]))
    , ("Hexagonal - Large value", (0, 2^32),
        Hexagonal ([[{ coord = HexCoord 0 0, value = 2^32 }]]))
    ]

toHexListTests : [Test]
toHexListTests = testUnaryFunction toHexList
    [ ("toHexList - Rectangular",
        Rectangular ([[{ coord = HexCoord 0 0, value = 0 }],[{ coord = HexCoord 0 1, value = 0 }]]),
        [[{ coord = HexCoord 0 0, value = 0 }],[{ coord = HexCoord 0 1, value = 0 }]])
    , ("toHexList - Hexagonal",
        Hexagonal ([[{ coord = HexCoord 0 0, value = 0 }]]),
        [[{ coord = HexCoord 0 0, value = 0 }]])
    ]

showHexGridTests : [Test]
showHexGridTests =
    let style = Dict.fromList
                    [ (0, SOutlined defaultLine)
                    , (1, SColor blue)
                    , (2, SColor red)
                    ] in
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
    [ ("pixelToHexCoord - Origin", (1, (0, 0)), HexCoord 0 0)
    , ("pixelToHexCoord - Point 1", (25, (40, 70)), HexCoord 0 2)
    , ("pixelToHexCoord - Point 2", (25, (500, 300)), HexCoord 8 8)
    , ("pixelToHexCoord - Point 3", (25, (-20, -100)), HexCoord 1 -3)
    , ("pixelToHexCoord - Point 4", (25, (0, 200)), HexCoord -3 6)
    , ("pixelToHexCoord - Point 5", (25, (10000, 0)), HexCoord 231 0)
    ]
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 1" 25 (0, -25))
            [HexCoord 0 0, HexCoord 1 -1, HexCoord 0 0, HexCoord 1 -1, HexCoord 0 -1])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 2" 25 (21, -12))
            [HexCoord 0 0, HexCoord 1 -1, HexCoord 0 0, HexCoord 1 0, HexCoord 0 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 3" 25 (21, 12))
            [HexCoord 0 0, HexCoord 0 0, HexCoord 0 1, HexCoord 1 0, HexCoord 0 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 4" 25 (0, 24))
            [HexCoord 0 0, HexCoord 0 0, HexCoord 0 1, HexCoord 0 0, HexCoord 0 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 5" 25 (-21, 12))
            [HexCoord 0 0, HexCoord 0 0, HexCoord -1 1, HexCoord 0 0, HexCoord -1 0])
    ++
    (map (\(f, expected) -> f expected) <|
        zip (enumerateVertices "pixelToHexCoord - Vertex 6" 25 (-21, -12))
            [HexCoord 0 0, HexCoord 0 -1, HexCoord 0 0, HexCoord 0 0, HexCoord -1 0])

inGridTests : [Test]
inGridTests =
    let rectGrid = rectangularHexGrid (5, 5) 0
        hexGrid  = hexagonalHexGrid 5 0
    in testBinaryFunction inGrid
    [ ("inGrid - Rectangular Pass", (HexCoord 0 0, rectGrid), True)
    , ("inGrid - Rectangular Pass (not Origin)", (HexCoord 3 2, rectGrid), True)
    , ("inGrid - Rectangular Fail", (HexCoord 100 0, rectGrid), False)
    , ("inGrid - Hexagonal Pass", (HexCoord 0 0, hexGrid), True)
    , ("inGrid - Hexagonal Pass (not Origin)", (HexCoord 2 -3, hexGrid), True)
    , ("inGrid - Hexagonal Fail", (HexCoord 0 100, hexGrid), False)
    ]

neighborsTests : [Test]
neighborsTests = testUnaryFunction neighbors
    [ ("neighbors - Origin", HexCoord 0 0,
        [HexCoord 1 0, HexCoord 1 -1, HexCoord 0 -1, HexCoord -1 0, HexCoord -1 1, HexCoord 0 1])
    , ("neighbors - Not Origin", HexCoord -5 10,
        [HexCoord -4 10, HexCoord -4 9, HexCoord -5 9, HexCoord -6 10, HexCoord -6 11, HexCoord -5 11])
    ]

diagonalsTests : [Test]
diagonalsTests = testUnaryFunction diagonals
    [ ("diagonals - Origin", HexCoord 0 0,
        [HexCoord 2 -1, HexCoord 1 -2, HexCoord -1 -1, HexCoord -2 1, HexCoord -1 2, HexCoord 1 1])
    , ("diagonals - Not Origin", HexCoord 3 7,
        [HexCoord 5 6, HexCoord 4 5, HexCoord 2 6, HexCoord 1 8, HexCoord 2 9, HexCoord 4 8])
    ]

distanceTests : [Test]
distanceTests = testBinaryFunction distance
    [ ("distance - 0", (HexCoord 0 0, HexCoord 0 0), 0)
    , ("distance - From Origin", (HexCoord 0 0, HexCoord 5 0), 5)
    , ("distance - No Origin", (HexCoord -3 0, HexCoord 5 0), 8)
    , ("distance - Horizontal", (HexCoord 0 0, HexCoord 10 0), 10)
    , ("distance - Vertical", (HexCoord 0 0, HexCoord 0 10), 10)
    , ("distance - Diagonal", (HexCoord 0 0, HexCoord 5 7), 7)
    , ("distance - Negative", (HexCoord 0 0, HexCoord -3 -3), 3)
    ]

-- line is not actually correctly implemented yet
lineTests : [Test]
lineTests = testBinaryFunction line
    [ ("line - 0", (HexCoord 0 0, HexCoord 0 0), [])
    , ("line - From Origin", (HexCoord 0 0, HexCoord 2 0),
        [HexCoord 0 0, HexCoord 1 0, HexCoord 2 0])
    , ("line - No Origin", (HexCoord 10 10, HexCoord 10 12),
        [HexCoord 10 10, HexCoord 10 11, HexCoord 10 12])
    , ("line - Horizontal", (HexCoord 0 0, HexCoord 3 0),
        [HexCoord 0 0, HexCoord 1 0, HexCoord 2 0, HexCoord 3 0])
    , ("line - Vertcal", (HexCoord 0 0, HexCoord 0 3),
        [HexCoord 0 0, HexCoord 0 1, HexCoord 0 2, HexCoord 0 3])
    , ("line - Diagonal", (HexCoord 0 0, HexCoord 3 3),
        [HexCoord 0 0, HexCoord 1 1, HexCoord 2 2, HexCoord 3 3])
    , ("line - Negative", (HexCoord 0 0, HexCoord -2 -2),
        [HexCoord 0 0, HexCoord -1 -1, HexCoord -2 -2])
    ]

rangeTests : [Test]
rangeTests = testBinaryFunction range
    [ ("range - 0", (HexCoord 0 0, 0),
        [HexCoord 0 0])
    , ("range - 1", (HexCoord 0 0, 1),
        [HexCoord -1 0, HexCoord -1 1, HexCoord 0 -1, HexCoord 0 0, HexCoord 0 1, HexCoord 1 -1, HexCoord 1 0])
    , ("range - 2", (HexCoord 0 0, 2),
        [HexCoord -2 0, HexCoord -2 1, HexCoord -2 2, HexCoord -1 -1, HexCoord -1 0, HexCoord -1 1, HexCoord -1 2, HexCoord 0 -2, HexCoord 0 -1, HexCoord 0 0, HexCoord 0 1, HexCoord 0 2, HexCoord 1 -2, HexCoord 1 -1, HexCoord 1 0, HexCoord 1 1, HexCoord 2 -2, HexCoord 2 -1, HexCoord 2 0])
    , ("range - Not Origin", (HexCoord 5 5, 1),
        [HexCoord 4 5, HexCoord 4 6, HexCoord 5 4, HexCoord 5 5, HexCoord 5 6, HexCoord 6 4, HexCoord 6 5])
    , ("range - Negative", (HexCoord 0 0, -2), [])
    ]

ringTests : [Test]
ringTests = testBinaryFunction ring
    [ ("ring - 0", (HexCoord 0 0, 0),
        [HexCoord 0 0])
    , ("ring - 1", (HexCoord 0 0, 1),
        [HexCoord -1 1, HexCoord 0 1, HexCoord 1 0, HexCoord 1 -1, HexCoord 0 -1, HexCoord -1 0, HexCoord -1 1])
    , ("ring - 2", (HexCoord 0 0, 2),
         [HexCoord -2 2, HexCoord -1 2, HexCoord 0 2, HexCoord 1 1, HexCoord 2 0, HexCoord 2 -1, HexCoord 2 -2, HexCoord 1 -2, HexCoord 0 -2, HexCoord -1 -1, HexCoord -2 0, HexCoord -2 1, HexCoord -2 2])
    , ("ring - Not Origin", (HexCoord 5 5, 1),
        [HexCoord 4 6, HexCoord 5 6, HexCoord 6 5, HexCoord 6 4, HexCoord 5 4, HexCoord 4 5, HexCoord 4 6])
    , ("ring - Negative", (HexCoord 0 0, -2), [])
    ]

-- TODO: Rectangular Hex Grid tests
insertTests : [Test]
insertTests =
    let grid = hexagonalHexGrid 1 0 in
    [ test "insert - Origin" <| assertEqual (insert (HexCoord 0 0) 1 grid) <|
        Just (Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 1 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 }]]))
    , test "insert - Not Origin" <| assertEqual (insert (HexCoord 0 1) 1 grid) <|
        Just (Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 1 }]]))
    , test "insert - Outside Grid" <| assertEqual (insert (HexCoord 10 10) 1 grid) Nothing
    , test "insert - Negative" <| assertEqual (insert (HexCoord -1 0) 1 grid) <|
        Just (Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 1 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 }]]))
    ]

-- TODO: Rectangular Hex Grid tests
insertIfPossibleTests : [Test]
insertIfPossibleTests =
    let grid = hexagonalHexGrid 1 0 in
    [ test "insertIfPossible - Origin" <| assertEqual (insertIfPossible (HexCoord 0 0) 1 grid) <|
        Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 1 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 }]])
    , test "insertIfPossible - Not Origin" <| assertEqual (insertIfPossible (HexCoord 0 1) 1 grid) <|
        Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 0 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 1 }]])
    , test "insertIfPossible - Outside Grid" <| assertEqual (insertIfPossible (HexCoord 10 10) 1 grid) grid
    , test "insertIfPossible - Negative" <| assertEqual (insertIfPossible (HexCoord -1 0) 1 grid) <|
        Hexagonal ([[{ coord = HexCoord 0 -1, value = 0 },{ coord = HexCoord 1 -1, value = 0 }],[{ coord = HexCoord -1 0, value = 1 },{ coord = HexCoord 0 0, value = 0 },{ coord = HexCoord 1 0, value = 0 }],[{ coord = HexCoord -1 1, value = 0 },{ coord = HexCoord 0 1, value = 0 }]])
    ]

tests : [Test]
tests = rectangularHexGridTests
     ++ hexagonalHexGridTests
     ++ toHexListTests
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
