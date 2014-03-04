module Main where

import Dict

import ElmTest.Runner.Console (runDisplay)
import open ElmTest.Test
import open ElmTest.Assertion

import open HexGrid

testBinopFunction : (a -> b -> c) -> [(String, (a, b), c)] -> [Test]
testBinopFunction f = map (\(name, (a, b), expected) -> test name <| assertEqual (f a b) expected)

testUnaryFunction : (a -> b) -> [(String, a, b)] -> [Test]
testUnaryFunction f = map (\(name, a, expected) -> test name <| assertEqual (f a) expected)

rectangularHexGridTests : [Test]
rectangularHexGridTests = testBinopFunction rectangularHexGrid
    [ ("Rectangular - 0", ((1, 1), 0), 
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = 0 }],[{ coord = { x = 0, y = 1 }, value = 0 }]])) 
    , ("Rectangular - 1", ((1, 1), 1.0), 
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = 1 }],[{ coord = { x = 0, y = 1 }, value = 1 }]]))
    , ("Rectangular - Negative", ((1,1), -1), 
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = -1 }],[{ coord = { x = 0, y = 1 }, value = -1 }]]))
    , ("Rectangular - Width 2", ((2,1), 0),
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = 0 },{ coord = { x = 1, y = 0 }, value = 0 }],[{ coord = { x = 0, y = 1 }, value = 0 },{ coord = { x = 1, y = 1 }, value = 0 }]]))
    , ("Rectangular - Width 0", ((0,1), 0),
        Rectangular [[],[]])
    , ("Rectangular - Height 0", ((1,0), 0),
        Rectangular [])
    , ("Rectangular - Height 3", ((5,3), 0),
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = 0 },{ coord = { x = 1, y = 0 }, value = 0 },{ coord = { x = 2, y = 0 }, value = 0 },{ coord = { x = 3, y = 0 }, value = 0 },{ coord = { x = 4, y = 0 }, value = 0 }],[{ coord = { x = 0, y = 1 }, value = 0 },{ coord = { x = 1, y = 1 }, value = 0 },{ coord = { x = 2, y = 1 }, value = 0 },{ coord = { x = 3, y = 1 }, value = 0 },{ coord = { x = 4, y = 1 }, value = 0 }],[{ coord = { x = -1, y = 2 }, value = 0 },{ coord = { x = 0, y = 2 }, value = 0 },{ coord = { x = 1, y = 2 }, value = 0 },{ coord = { x = 2, y = 2 }, value = 0 },{ coord = { x = 3, y = 2 }, value = 0 }],[{ coord = { x = -1, y = 3 }, value = 0 },{ coord = { x = 0, y = 3 }, value = 0 },{ coord = { x = 1, y = 3 }, value = 0 },{ coord = { x = 2, y = 3 }, value = 0 },{ coord = { x = 3, y = 3 }, value = 0 }]]))
    , ("Rectangular - Large value", ((1, 1), 2^32), 
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = 2^32 }],[{ coord = { x = 0, y = 1 }, value = 2^32 }]]))
    ]

hexagonalHexGridTests : [Test]
hexagonalHexGridTests = testBinopFunction hexagonalHexGrid
    [ ("Hexagonal - 0", (1, 0),
        Hexagonal ([[{ coord = { x = 0, y = -1 }, value = 0 },{ coord = { x = 1, y = -1 }, value = 0 }],[{ coord = { x = -1, y = 0 }, value = 0 },{ coord = { x = 0, y = 0 }, value = 0 },{ coord = { x = 1, y = 0 }, value = 0 }],[{ coord = { x = -1, y = 1 }, value = 0 },{ coord = { x = 0, y = 1 }, value = 0 }]]))
    , ("Hexagonal - 1", (1, 1),
        Hexagonal ([[{ coord = { x = 0, y = -1 }, value = 1 },{ coord = { x = 1, y = -1 }, value = 1 }],[{ coord = { x = -1, y = 0 }, value = 1 },{ coord = { x = 0, y = 0 }, value = 1 },{ coord = { x = 1, y = 0 }, value = 1 }],[{ coord = { x = -1, y = 1 }, value = 1 },{ coord = { x = 0, y = 1 }, value = 1 }]]))
    , ("Hexagonal - Radius 2", (2, 0),
        Hexagonal ([[{ coord = { x = 0, y = -2 }, value = 0 },{ coord = { x = 1, y = -2 }, value = 0 },{ coord = { x = 2, y = -2 }, value = 0 }],[{ coord = { x = -1, y = -1 }, value = 0 },{ coord = { x = 0, y = -1 }, value = 0 },{ coord = { x = 1, y = -1 }, value = 0 },{ coord = { x = 2, y = -1 }, value = 0 }],[{ coord = { x = -2, y = 0 }, value = 0 },{ coord = { x = -1, y = 0 }, value = 0 },{ coord = { x = 0, y = 0 }, value = 0 },{ coord = { x = 1, y = 0 }, value = 0 },{ coord = { x = 2, y = 0 }, value = 0 }],[{ coord = { x = -2, y = 1 }, value = 0 },{ coord = { x = -1, y = 1 }, value = 0 },{ coord = { x = 0, y = 1 }, value = 0 },{ coord = { x = 1, y = 1 }, value = 0 }],[{ coord = { x = -2, y = 2 }, value = 0 },{ coord = { x = -1, y = 2 }, value = 0 },{ coord = { x = 0, y = 2 }, value = 0 }]]))
    , ("Hexagonal - Radius 0", (0, 0),
        Hexagonal ([[{ coord = { x = 0, y = 0 }, value = 0 }]]))
    , ("Hexagonal - Radius 5", (5, 0),
        Hexagonal ([[{ coord = { x = 0, y = -5 }, value = 0 },{ coord = { x = 1, y = -5 }, value = 0 },{ coord = { x = 2, y = -5 }, value = 0 },{ coord = { x = 3, y = -5 }, value = 0 },{ coord = { x = 4, y = -5 }, value = 0 },{ coord = { x = 5, y = -5 }, value = 0 }],[{ coord = { x = -1, y = -4 }, value = 0 },{ coord = { x = 0, y = -4 }, value = 0 },{ coord = { x = 1, y = -4 }, value = 0 },{ coord = { x = 2, y = -4 }, value = 0 },{ coord = { x = 3, y = -4 }, value = 0 },{ coord = { x = 4, y = -4 }, value = 0 },{ coord = { x = 5, y = -4 }, value = 0 }],[{ coord = { x = -2, y = -3 }, value = 0 },{ coord = { x = -1, y = -3 }, value = 0 },{ coord = { x = 0, y = -3 }, value = 0 },{ coord = { x = 1, y = -3 }, value = 0 },{ coord = { x = 2, y = -3 }, value = 0 },{ coord = { x = 3, y = -3 }, value = 0 },{ coord = { x = 4, y = -3 }, value = 0 },{ coord = { x = 5, y = -3 }, value = 0 }],[{ coord = { x = -3, y = -2 }, value = 0 },{ coord = { x = -2, y = -2 }, value = 0 },{ coord = { x = -1, y = -2 }, value = 0 },{ coord = { x = 0, y = -2 }, value = 0 },{ coord = { x = 1, y = -2 }, value = 0 },{ coord = { x = 2, y = -2 }, value = 0 },{ coord = { x = 3, y = -2 }, value = 0 },{ coord = { x = 4, y = -2 }, value = 0 },{ coord = { x = 5, y = -2 }, value = 0 }],[{ coord = { x = -4, y = -1 }, value = 0 },{ coord = { x = -3, y = -1 }, value = 0 },{ coord = { x = -2, y = -1 }, value = 0 },{ coord = { x = -1, y = -1 }, value = 0 },{ coord = { x = 0, y = -1 }, value = 0 },{ coord = { x = 1, y = -1 }, value = 0 },{ coord = { x = 2, y = -1 }, value = 0 },{ coord = { x = 3, y = -1 }, value = 0 },{ coord = { x = 4, y = -1 }, value = 0 },{ coord = { x = 5, y = -1 }, value = 0 }],[{ coord = { x = -5, y = 0 }, value = 0 },{ coord = { x = -4, y = 0 }, value = 0 },{ coord = { x = -3, y = 0 }, value = 0 },{ coord = { x = -2, y = 0 }, value = 0 },{ coord = { x = -1, y = 0 }, value = 0 },{ coord = { x = 0, y = 0 }, value = 0 },{ coord = { x = 1, y = 0 }, value = 0 },{ coord = { x = 2, y = 0 }, value = 0 },{ coord = { x = 3, y = 0 }, value = 0 },{ coord = { x = 4, y = 0 }, value = 0 },{ coord = { x = 5, y = 0 }, value = 0 }],[{ coord = { x = -5, y = 1 }, value = 0 },{ coord = { x = -4, y = 1 }, value = 0 },{ coord = { x = -3, y = 1 }, value = 0 },{ coord = { x = -2, y = 1 }, value = 0 },{ coord = { x = -1, y = 1 }, value = 0 },{ coord = { x = 0, y = 1 }, value = 0 },{ coord = { x = 1, y = 1 }, value = 0 },{ coord = { x = 2, y = 1 }, value = 0 },{ coord = { x = 3, y = 1 }, value = 0 },{ coord = { x = 4, y = 1 }, value = 0 }],[{ coord = { x = -5, y = 2 }, value = 0 },{ coord = { x = -4, y = 2 }, value = 0 },{ coord = { x = -3, y = 2 }, value = 0 },{ coord = { x = -2, y = 2 }, value = 0 },{ coord = { x = -1, y = 2 }, value = 0 },{ coord = { x = 0, y = 2 }, value = 0 },{ coord = { x = 1, y = 2 }, value = 0 },{ coord = { x = 2, y = 2 }, value = 0 },{ coord = { x = 3, y = 2 }, value = 0 }],[{ coord = { x = -5, y = 3 }, value = 0 },{ coord = { x = -4, y = 3 }, value = 0 },{ coord = { x = -3, y = 3 }, value = 0 },{ coord = { x = -2, y = 3 }, value = 0 },{ coord = { x = -1, y = 3 }, value = 0 },{ coord = { x = 0, y = 3 }, value = 0 },{ coord = { x = 1, y = 3 }, value = 0 },{ coord = { x = 2, y = 3 }, value = 0 }],[{ coord = { x = -5, y = 4 }, value = 0 },{ coord = { x = -4, y = 4 }, value = 0 },{ coord = { x = -3, y = 4 }, value = 0 },{ coord = { x = -2, y = 4 }, value = 0 },{ coord = { x = -1, y = 4 }, value = 0 },{ coord = { x = 0, y = 4 }, value = 0 },{ coord = { x = 1, y = 4 }, value = 0 }],[{ coord = { x = -5, y = 5 }, value = 0 },{ coord = { x = -4, y = 5 }, value = 0 },{ coord = { x = -3, y = 5 }, value = 0 },{ coord = { x = -2, y = 5 }, value = 0 },{ coord = { x = -1, y = 5 }, value = 0 },{ coord = { x = 0, y = 5 }, value = 0 }]]))
    , ("Hexagonal - Negative", (0, -1),
        Hexagonal ([[{ coord = { x = 0, y = 0 }, value = -1 }]]))
    , ("Hexagonal - Large value", (0, 2^32),
        Hexagonal ([[{ coord = { x = 0, y = 0 }, value = 2^32 }]]))
    ]

toHexListTests : [Test]
toHexListTests = testUnaryFunction toHexList
    [ ("toHexList - Rectangular",
        Rectangular ([[{ coord = { x = 0, y = 0 }, value = 0 }],[{ coord = { x = 0, y = 1 }, value = 0 }]]),
        [[{ coord = { x = 0, y = 0 }, value = 0 }],[{ coord = { x = 0, y = 1 }, value = 0 }]])
    , ("toHexList - Hexagonal",
        Hexagonal ([[{ coord = { x = 0, y = 0 }, value = 0 }]]),
        [[{ coord = { x = 0, y = 0 }, value = 0 }]])
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

tests : [Test]
tests = rectangularHexGridTests 
     ++ hexagonalHexGridTests 
     ++ toHexListTests
     ++ showHexGridTests



console = runDisplay tests        