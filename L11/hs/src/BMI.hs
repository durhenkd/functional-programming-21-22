module BMI where


main :: IO ()
main = do
    putStrLn "Please enter your weight (kilograms): "
    weight <- getLine
    putStrLn "Please enter your height (meters): "
    height <- getLine

    let weightN = read weight :: Float
    let heightN = read height :: Float
    let bmi = weightN / (heightN * heightN)
    let msg = "Your BMI is: " ++ (show bmi)
    putStrLn msg