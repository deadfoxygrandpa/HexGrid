module Tests where

import open Public.ElmTest.ElmTest

import open HexGrid

testRectangularHexGrid = testFunction2 "rectangularHexGrid" rectangularHexGrid
    [ (((1,1), True), Rectangular ([[{ coord = { x = 0, y = 0 }, value = True }],[{ coord = { x = 0, y = 1 }, value = True }]]))
    , (((1,1), False), Rectangular ([[{ coord = { x = 0, y = 0 }, value = False }],[{ coord = { x = 0, y = 1 }, value = False }]]))
    , (((2,1), True), Rectangular ([[{ coord = { x = 0, y = 0 }, value = True },{ coord = { x = 1, y = 0 }, value = True }],[{ coord = { x = 0, y = 1 }, value = True },{ coord = { x = 1, y = 1 }, value = True }]]))
    , (((0,1), True), Rectangular [[],[]])
    , (((1,0), True), Rectangular [])
    , (((5,3), False), Rectangular ([[{ coord = { x = 0, y = 0 }, value = False },{ coord = { x = 1, y = 0 }, value = False },{ coord = { x = 2, y = 0 }, value = False },{ coord = { x = 3, y = 0 }, value = False },{ coord = { x = 4, y = 0 }, value = False }],[{ coord = { x = 0, y = 1 }, value = False },{ coord = { x = 1, y = 1 }, value = False },{ coord = { x = 2, y = 1 }, value = False },{ coord = { x = 3, y = 1 }, value = False },{ coord = { x = 4, y = 1 }, value = False }],[{ coord = { x = -1, y = 2 }, value = False },{ coord = { x = 0, y = 2 }, value = False },{ coord = { x = 1, y = 2 }, value = False },{ coord = { x = 2, y = 2 }, value = False },{ coord = { x = 3, y = 2 }, value = False }],[{ coord = { x = -1, y = 3 }, value = False },{ coord = { x = 0, y = 3 }, value = False },{ coord = { x = 1, y = 3 }, value = False },{ coord = { x = 2, y = 3 }, value = False },{ coord = { x = 3, y = 3 }, value = False }]]))
    ]    

main = runPrettyTests <| testRectangularHexGrid