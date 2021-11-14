module Grid where

import Objects.Objects
import World
import Helper
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

initBlock :: Point -> BlockType -> Block
initBlock blockType b = MkBlock blockType b Blank 

instance Positioned Block where
    getPosition b = block_position b
    setPosition b p = b { block_position = p}

instance HitBox Block where
    getWidth b = 1
    getHeight b = 1

instance RenderableObject Block where
    getColor b = case block_type b of 
                    Solid -> white
                    _     -> withAlpha 0 white
    getPicture = block_picture

makeGrid :: [[Char]] -> [[Block]]
makeGrid xss = zipWith (curry makeLine) (reverse xss) [0 .. ]
      
makeLine :: ([Char], Int) -> [Block]
makeLine (xs, iy) = zipWith (curry res) xs [0 .. ]
            where res (c, x) = case c of
                                'x' -> initBlock (x, y) Solid
                                _   -> initBlock (x, y) Void  
                  y = fromIntegral iy

loadGrid :: [[Block]] -> IO [[Block]]
loadGrid bs@((x:_):_)   | block_picture x == Blank = mapM (\b -> mapM getBlockPicture b) bs
                        | otherwise = return bs

getBlockPicture :: Block -> IO Block
getBlockPicture b = case block_type b of 
                    Solid -> do
                            bpic <- loadBMP "block_red.bmp"
                            let bpic' = translate 0.5 0.5 $ scale (1 / 32) (1 / 32) bpic -- block is sized 32 x 32
                            return $ b {block_picture = bpic'}
                    _ -> return $ b {block_picture = (Polygon [(0, 0), (1, 0), (1, 1), (0, 1), (0, 0)])}