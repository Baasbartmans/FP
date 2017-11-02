module Main(main) where
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    --import Graphics.Gloss.Game

    data Player = Player {x :: Int, y :: Int}

    getKey :: Event -> Key
    getKey (EventKey key _ _ _) = key


    {-inputHandler :: Event -> Player -> Player
    inputHandler e Player {x = x', y = y'} | getKey e == (Char 'd') = Player {x = (x'+1), y = y'}
                                           | getKey e == (Char 'a') = Player {x = (x'-1),  y = y'}
                                           | otherwise              = Player {x', y'}

    -}
    
    
    width, height, offset :: Int
    width = 300
    height = 300
    offset = 100
    
    window :: Display
    window = InWindow "Pong" (width, height) (offset, offset)
    
    background :: Color
    background = black
    
    main :: IO ()
    main = do
        -- load alle plaatjes
        player <- loadBMP "D:/floor.bmp"
        
        --teken de plaatjes
        display window background (pictures [
            drawing,
            translate 30 50 $ player])
        
