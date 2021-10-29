
module Date exposing (Month(..), createDate, daysInMonth, monthToInt, compareMonth, isLeapYear)

type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
type Date = Date {day: Int, month: Month, year: Int}

{- E 4.6.9 You may have noticed that the createDate function is not always correct: it doesn’t handle leap years!
            1. Modify the Date.elm module, by adding a new function with the signature
                isLeapYear : Int -> Bool , that checks if a given year between 1970 and 3000 is a leap year.
            2. Then modify the daysInMonth function to account for leap years.
            3. Finally update the createDate function to use the corrected version of daysInMonth 
            
            Hint: You should change the signature to daysInMonth : Month -> Int -> Int , where the second
                parameter is the year. -}


isLeapYear : Int -> Bool
isLeapYear year = if ((modBy 400 year) == 0) then True else if ((modBy 100 year) == 0) then False else if ((modBy 4 year) == 0) then True else False


createDate : Int -> Month -> Int -> Maybe Date
createDate day month year =
  let
    between start end num = (start <= num) && (num <= end)
    lastDay = daysInMonth month year
  in
    if not (between 1 lastDay day) then
      Nothing
    else if not (between 1970 3000 year) then
      Nothing
    else
      Just (Date {day = day, month = month, year = year})


daysInMonth : Month -> Int -> Int
daysInMonth month year =
  case month of
    Jan -> 31
    Feb -> if (isLeapYear year) then 29 else 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

monthToInt : Month -> Int
monthToInt month =
  case month of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
  compare (monthToInt m1) (monthToInt m2)

