import Data.Maybe
import Data.List
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Parsed = Parsed {
    pseeds :: [Integer],
    mapping :: [Mapping]
} deriving (Show)

data Mapping = Mapping {
    name :: String,
    ranges :: [MapRange]
} deriving (Show)

data MapRange = MapRange {
    dstStart :: Integer,
    srcStart :: Integer,
    rngLength :: Integer
} deriving (Show)

seeds = do
    sds <- firstline
    endOfLine
    spaces
    mp <- many1 block
    eof
    return Parsed{pseeds = sds, mapping = mp}

firstline = string "seeds:" >> many (char ' ') >> numbers
numbers = sepBy1 number (char ' ')
number = fmap (read::String->Integer) (many1 digit)

block = do
    nm <- many1 (letter <|> char '-')
    many1 (char ' ')
    string "map:"
    endOfLine
    rs <- many1 mapRange
    return Mapping{name = nm, ranges = rs}

mapRange = do
    dst <- number
    many1 (char ' ')
    src <- number
    many1 (char ' ')
    length <- number
    endOfLine
    spaces
    return MapRange{dstStart = dst, srcStart = src, rngLength = length}

parsePart5 :: String -> Either ParseError Parsed
parsePart5 = parse seeds "(unknown)"

remapPart :: Integer -> MapRange -> Maybe Integer
remapPart n rng =
    if srcStart rng <= n && n < srcStart rng + rngLength rng
        then Just $ n + (dstStart rng - srcStart rng)
        else Nothing

remap :: Integer -> [MapRange] -> Integer
remap seedNum mapRanges = 
    let xs = mapMaybe (remapPart seedNum) mapRanges in
        if null xs then seedNum
        -- unless ranges overlap there should be just one.
        else head xs

applyMapping :: Integer -> [Mapping] -> Integer
applyMapping seedNum ms = foldl (\v m -> remap v (ranges m)) seedNum ms

solution parsedData =
 minimum $ map (\sd -> applyMapping sd (mapping parsedData)) (pseeds parsedData)

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parsePart5 input of
        Left err -> print err
        Right p -> print $ solution p