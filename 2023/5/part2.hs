-- Solutions to both part 1 and part 2.

import Data.List
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Debug.Trace

data SeedRng = SeedRng {
    seedStart :: Integer,
    seedCount :: Integer
} deriving (Show)

data Parsed = Parsed {
    pseeds :: [SeedRng],
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

firstline = string "seeds:" >> many (char ' ') >> sepBy1 parseSeed (char ' ')
parseSeed = do
    start <- number
    many1 (char ' ')
    count <- number
    return SeedRng{seedStart = start, seedCount = count}

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

-- A "steppy" function is kind of like a step function but not.
-- It's f(x) = x + step(x).
-- This is chosen as a cleaner way to represent the range mapping.
-- Gaps in between ranges or past the last range are expressed as steps to 0.
newtype SteppyFunc = SteppyFunc [Chunk]

-- For print debugging.
instance Show SteppyFunc where
    show (SteppyFunc cs) = intercalate "\n" (map (\c -> (show (cStart c)) ++ " " ++ (show (cDelta c))) cs)

data Chunk = Chunk {
    cStart :: Integer,
    cDelta :: Integer
} deriving (Show)

steppify :: [MapRange] -> SteppyFunc
steppify ms =
    let sorted = sortBy (\a b -> compare (srcStart a) (srcStart b)) ms
    in SteppyFunc $ foldr
        (\m xs ->
            let s = srcStart m; d = dstStart m; l = rngLength m
                a = Chunk{cStart = s, cDelta = d - s}
                b = Chunk{cStart = s + l, cDelta = 0}
            in
                a:
                -- If there is a gap before the chunk to the right, add a
                -- delta 0 chunk to fill the space in between.
                case xs of
                    z:_ -> [b | cStart b < cStart z]
                    _ -> [b]
                ++
                xs)
        []
        sorted

dedup :: Eq a => [a] -> [a]
dedup (x:y:ys)
  | x == y    = dedup (y:ys)
  | otherwise = x  : dedup (y:ys)
dedup ys = ys

compose :: SteppyFunc -> SteppyFunc -> SteppyFunc
compose f' f =
    -- The approach to composing two steppys is to figure out where there might
    -- be a jump discontinuity, and use that to build a new step function.
    -- The composed functions expand to:
    --   Let f(x) = x + step(x), and f'(x) = x + step'(x)
    --   In f'(f(x)) = x + step(x) + step'(x + step(x))
    -- At a glance like the result would be another steppy function, but it might have more
    -- steps than either f or g. For lack of a better solution, create the composed steppy
    -- function by looking for discontinuities and evaluating it those points.
    -- Look for discontinuities at:
    -- Case 1: An x where step(x) changes.
    -- Case 2: An x where step'(x + step(x)) changes. Solved by brute forcing
    --   all possible x in the relationship
    --   (discontinuous points in domain of step') = x + step(x)
    let (SteppyFunc f's, SteppyFunc fs) = (f', f)
        case1 = map cStart fs
        -- the brute force. there are more x here than satify the relationship, but false positives are OK.
        -- we just need to make sure we probe enough points when generating the composed function.
        -- we'll simplify it later.
        case2 = [a - b | a <- map cStart f's, b <- map cDelta fs]
        -- Always test 0 for good measure.
        possibleX = dedup $ sort $ 0 : filter (>0) (case1 ++ case2)
        f'' i = apply f' (apply f i)
    in SteppyFunc $ simplify [Chunk{cStart = x, cDelta = f'' x - x} | x <- possibleX]

simplify :: [Chunk] -> [Chunk]
simplify (x1:x2:xs)
    | cDelta x1 == cDelta x2 =  simplify (x1:xs)
    | otherwise = x1 : simplify (x2:xs)
simplify xs = xs

deltaToLeft :: [Chunk] -> Integer -> Integer
deltaToLeft (x1:x2:xs) i =
    if cStart x1 <= i && i < cStart x2 then cDelta x1
    else deltaToLeft (x2:xs) i
deltaToLeft (x1:xs) i = if cStart x1 <= i then cDelta x1 else 0
deltaToLeft [] i = 0

apply :: SteppyFunc -> Integer -> Integer
apply (SteppyFunc cs) i = i + deltaToLeft cs i

chunkBoundaries :: [Chunk] -> SeedRng -> [Integer]
chunkBoundaries xs rng =
    concatMap
    (\x ->
    let
        rngLow = seedStart rng
        rngHigh = seedStart rng + seedCount rng
        boundary = cStart x
    in [boundary | rngLow <= boundary && boundary < rngHigh ])
    xs

makeSteppys :: Parsed -> [SteppyFunc]
makeSteppys parsedData = map (steppify . ranges) (mapping parsedData)

solnPart1 :: Parsed -> Integer
solnPart1 parsedData =
    let f = foldl1 (flip compose) (makeSteppys parsedData)
    in minimum $ map (apply f) (concat [[seedStart p, seedCount p] | p <- pseeds parsedData])

solnPart2 :: Parsed -> Integer
solnPart2 parsedData =
    let seeds = pseeds parsedData
        f = foldl1 (flip compose) (makeSteppys parsedData)
        SteppyFunc cs = f
        possibleMins = concat [seedStart rng
                               : (seedStart rng + seedCount rng - 1)
                               : chunkBoundaries cs rng | rng <- seeds]
    in minimum $ map (apply f) possibleMins

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parsePart5 input of
        Left err -> print err
        Right p -> putStrLn ("part 1: " ++ show (solnPart1 p) ++ ", part 2: " ++ show (solnPart2 p))