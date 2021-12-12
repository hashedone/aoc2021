{-# LANGUAGE TupleSections #-}

import System.IO
import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Data.Either
import Data.Bifunctor
import Data.Tuple.Extra

-- Solution

part1 :: [(Int, Int)] -> Int
part1 = uncurry (*) . (both $ foldr1 (+)) . unzip

-- I tried, but come on, I am not able to pointfreeze it. Here is what I get from pointfree.io:
-- ap (ap . ap (ap . ((,,) .) . (. fst) . (+) . fst3) (ap ((.) . (+) . snd3) ((. fst) . (*) . thd3))) ((. snd) . (+) . thd3)
-- I am nowhere near to do this. Pointfree is nice, but some primitives are not pointfee-friendly
apply :: (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
apply (x, y, aim) (dx, daim) = (x + dx, y + aim * dx, aim + daim)

part2 :: [(Int, Int)] -> Int
part2 = uncurry (*) . (fst3 &&& snd3) . foldl apply (0, 0, 0)

-- Parsing

forward :: Parser (Int, Int)
forward = string "forward" *> spaces *> many digit <&> (, 0) . read

up :: Parser (Int, Int)
up = string "up" *> spaces *> many digit <&> (0, ) . negate . read

down :: Parser (Int, Int)
down = string "down" *> spaces *> many digit <&> (0, ) . read

parser :: Parser (Int, Int)
parser = spaces *> (forward <|> up <|> down) <* spaces <* eof

-- Glue

main :: IO ()
main = (print . (part1 &&& part2) . rights . map (parse parser "") . filter (/= "") . lines) =<< getContents
