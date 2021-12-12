import Data.Maybe
import Text.Regex.TDFA
import System.Environment


lakewood = Hotel Lakewood 3 110 90 80 80
bridgewood = Hotel Bridgewood 4 160 60 110 50
ridgewood = Hotel Ridgewood 5 220 150 100 40

data Day = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Enum, Show)

type Dates = [Day]

data HotelName = Lakewood
    | Bridgewood
    | Ridgewood
    deriving (Eq, Show)

data CustomerType = Regular | Rewards deriving (Eq, Show)

data Hotel = Hotel
    { name :: HotelName
    , rating :: Int
    , weekdayRegularRate :: Int
    , weekendRegularRate :: Int
    , weekdayRewardsRate :: Int
    , weekendRewardsRate :: Int
    }
    deriving (Eq, Show)

instance Ord Hotel where
    h1 <= h2 = rating h1 <= rating h2

parseCustomerType :: String -> Maybe CustomerType
parseCustomerType s = getCustomerType (s =~ "^Regular|Rewards")

parseDates :: String -> Maybe [Day]
parseDates s 
    | length matches /= length days = Nothing
    | otherwise = Just days
    where   days = mapMaybe getDate matches
            matches = getAllTextMatches (s =~ "\\([a-z]{3,4}\\)") :: [String]

getCustomerType :: String -> Maybe CustomerType
getCustomerType c
    | c == "Regular"    = Just Regular
    | c == "Rewards"    = Just Rewards
    | otherwise         = Nothing

getDate :: String -> Maybe Day
getDate d
    | d == "(mon)"  = Just Monday
    | d == "(tues)" = Just Tuesday
    | d == "(wed)"  = Just Wednesday
    | d == "(thur)"  = Just Thursday
    | d == "(fri)"  = Just Friday
    | d == "(sat)"  = Just Saturday
    | d == "(sun)"  = Just Sunday
    | otherwise     = Nothing

getRate :: CustomerType -> Day -> Hotel -> Int
getRate Regular d
    | d `elem` [Saturday, Sunday] = weekendRegularRate
    | otherwise = weekdayRegularRate
getRate Rewards d
    | d `elem` [Saturday, Sunday] = weekendRewardsRate
    | otherwise = weekdayRewardsRate

priceForHotel :: CustomerType -> [Day] -> Hotel -> Int
priceForHotel ct days hotel =
    sum $ map (getRate ct) days <*> [hotel]

bestPrice :: [Hotel] -> CustomerType -> [Day] -> HotelName
bestPrice hs ct ds = name . snd . minimum $ zip prices hs
    where   prices = map (priceForHotel ct ds) hs

bestPrice' :: String -> Maybe HotelName
bestPrice' s = 
    let hotels = [lakewood, bridgewood, ridgewood]
        customerType = parseCustomerType s
        days = parseDates s
    in bestPrice hotels <$> customerType <*> days 


main :: IO ()
main = do
    putStrLn "Hotel Reservation"
    args <- getArgs
    case args of
        ["-f", file] -> do
            c <- readFile file
            let inputs = lines c
            mapM_ (print . bestPrice') inputs
        [input] -> do
            print $ bestPrice' input
        _ -> putStrLn "Wrong number of arguments"