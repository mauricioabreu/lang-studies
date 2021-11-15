-- Fill in the DistanceConversions module first, and import it here
-- create a higher-order function for converting an area between two dimensions
-- this will take the function for converting a distance, and an area to convert
-- using the functions defined in the DistanceConversions module
-- Example areaConv inchesToCentimetres 9 = 58.0644
import DistanceConversions
    ( yardsToFeet, feetToInches, inchesToCentimetres )

areaConv :: (Float -> Float) -> Float -> Float
areaConv linearConversion area = linearConversion $ linearConversion area

-- define a function for converting square inches into square centimetres
sqInToSqCm :: Float -> Float
sqInToSqCm = inchesToCentimetres . inchesToCentimetres

-- define a function for converting square chains (22 yards) to square metres
sqChainsToSqM :: Float -> Float
sqChainsToSqM = sqChainsToM . sqChainsToM
    where chainsToYards = (22 *)
          chainsToCm = inchesToCentimetres . feetToInches . yardsToFeet . chainsToYards
          sqChainsToM = (/ 100) . chainsToCm