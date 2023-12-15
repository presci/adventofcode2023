-- docker run -i -t -v /Users/<<home>>/workspace/hask:/tmp/app debian
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe

data Item = Item (String, Int) deriving Show
data RGB = RGB Int Int Int deriving Show
data Game = Game Int [RGB] deriving Show

puzzle :: (Int, Int, Int)  -- RGB
puzzle = (12, 13, 14)

itemParser :: Parser Item
itemParser = do
    quantity <- many1 digit
    spaces
    color <- many1 letter
    return $ Item (color, (read::String->Int) quantity)

blockParser :: Parser RGB
blockParser = do
    draw <- sepBy itemParser (string ", ")
    let r = lookup00 "red" draw
        b = lookup00 "blue" draw
        g = lookup00 "green" draw
    return $ RGB r g b

drawParser :: Parser [RGB]
drawParser = do
    sepBy blockParser (string "; ")

gameParser :: Parser Game
gameParser = do
    string "Game"
    spaces
    i <-  many1 digit
    string ":"
    spaces
    Game ((read::String->Int)i) <$> drawParser


lookup00:: String -> [Item] -> Int
lookup00 _ [] = 0
lookup00 arg0 (Item (k, v):xss ) = if k == arg0 then v else lookup00 arg0 xss

maxrgb::RGB -> RGB -> RGB
maxrgb (RGB a b c) (RGB a0 b0 c0) = RGB  (max a a0) (max b b0) (max c c0)


getGame :: String -> Maybe Game
getGame arg0 = case parse gameParser "" arg0 of
    Right g -> Just g
    _ -> Nothing

getvalidGame :: Game -> Int
getvalidGame (Game i rgb) = if isvalid rgb puzzle then i else 0
    where
        isvalid ::[RGB] -> (Int, Int, Int) -> Bool
        isvalid  [] _ = True
        isvalid ((RGB r0 g0 b0):xss) p@(r,g,b) = not (r0 > r || g0 > g || b0 > b) && isvalid xss p

ans2 :: Game -> Int
ans2 (Game i rgb) = helper $  foldl maxrgb (RGB 0 0 0) rgb
    where
        helper::RGB -> Int
        helper (RGB x y z ) = x * y * z

main:: IO()
main = do
    contents <- lines <$> readFile "test.txt"
    let k = mapMaybe getGame contents
    print . sum $ map getvalidGame  k
    print . sum $ map ans2 k
