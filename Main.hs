import Control.Concurrent

blinker = [ [ 0, 0, 0 ],
            [ 1, 1, 1 ],
            [ 0, 0, 0 ] ]

glider = [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ]

-- get absolute position
(i, j) <+> (x, y) = (i + x, j + y)

-- count living neighbours
neighbours board (x, y) = sum (map (state board) neighbours)
  where
    neighbours = filter validCoord (map (<+> (x, y)) directions)
    validCoord (x, y) = 0 <= x && x < length (head board) && 0 <= y && y < length board
    directions = [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]

-- state of position on board
state board (x, y) = board !! fromIntegral y !! fromIntegral x

-- next state of cell with current and neighbours
nextState 1 2 = 1 -- lives
nextState _ 3 = 1 -- lives / reproduce
nextState _ _ = 0 -- lonely / overcrowded / barren

-- recursive loop to navigate each tick
tick board = do
    putStrLn (unlines [[if c == 1 then '#' else '-' | c <- r] | r <- board])
    threadDelay 1000000
    tick [[ nextState (state board (x, y)) (neighbours board (x, y))
          | x <- [0..length (head board) - 1]] | y <- [0..length board - 1]]

main = do tick glider
