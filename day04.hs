-- docker run -i -t -v /Users/<<home>>/workspace/hask:/tmp/app debian
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe
import qualified Data.Map as M
import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V

-- Card id [wins] [draw] win cards
data Card = Card Int [Int] [Int] Int Int deriving Show

intParser :: Parser Int
intParser = ((read::String -> Int) <$> many1 digit) <* spaces

intArrParser :: Parser [Int]
intArrParser = sepBy intParser spaces

cardParser :: Parser Card
cardParser = do
    _  <- string "Card" <* spaces
    n <- intParser
    _ <- char ':' <* spaces
    wins <- intArrParser
    _ <- char '|' <* spaces
    draws <- intArrParser
    return $ Card n wins draws (m wins draws) 1
    where
        m :: [Int] -> [Int] -> Int
        m [] _ = 0
        m (x:xs) d= if x `elem` d  then  1 + m xs d  else   m xs d

parseCard:: String -> Maybe Card
parseCard arg0 = case parse cardParser "" arg0 of
    Right card -> Just card
    _ -> Nothing

wins ::Int -> Int
wins 0 = 0
wins 1 = 1
wins x = 2 * wins (x - 1)


updateScratch :: Vector (Int, Int) -> Vector (Int, Int)
updateScratch = helper 0
    where
        helper::Int -> Vector (Int, Int) -> Vector (Int, Int)
        helper ind vec = case vec !? ind of
            Just node -> helper (ind + 1) $ vec // update (ind + 1) node vec
            _ -> vec


update ::Int -> (Int, Int) -> V.Vector (Int, Int) -> [(Int, (Int, Int))]
update ind k@(wn, ct) arg0 = mapMaybe (\a -> fn (a, k))  (take wn [ind..])
    where
        fn :: (Int, (Int, Int)) -> Maybe (Int, (Int, Int))
        fn (a, (w, c)) = case arg0 !? a of
            Just (wins, cards) -> Just (a , (wins, cards + ct))
            _ -> Nothing



main::IO()
main = do
    content <- lines <$> readFile "test.txt"
    let cards = mapMaybe parseCard content
    let k = foldl (\acc (Card _ _ _ w _ ) -> acc + wins w ) 0 cards
    print k
    let vec = foldl (\acc (_, b) -> acc + b) 0 $  updateScratch . V.fromList $ map (\(Card _ _ _ w ct) -> (w, ct)) cards
    print vec
