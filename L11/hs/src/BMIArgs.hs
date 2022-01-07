module BMIArgs where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs

    let weight = head args
    let height = head $ tail args

    let weightN = read weight :: Float
    let heightN = read height :: Float
    let bmi = weightN / (heightN * heightN)
    let msg = "Your BMI is: " ++ (show bmi)
    putStrLn msg