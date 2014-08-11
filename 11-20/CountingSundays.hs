module Main where

data Month = January | February | March | April | May | June | July
             | August  | September | October | November | December

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
  deriving (Eq, Show)

type Year = Int

monthsInYear :: [Month]
monthsInYear = [January, February, March, April, May, June, July, August, September, October, November, December]

getNextDay :: Day -> Day
getNextDay Monday = Tuesday
getNextDay Tuesday = Wednesday
getNextDay Wednesday = Thursday 
getNextDay Thursday = Friday 
getNextDay Friday = Saturday 
getNextDay Saturday = Sunday
getNextDay Sunday = Monday 

getNextMonth :: Month -> Month
getNextMonth January = February 
getNextMonth February = March 
getNextMonth March = April 
getNextMonth April = May 
getNextMonth May = June 
getNextMonth June = July 
getNextMonth July = August 
getNextMonth August = September 
getNextMonth September = October 
getNextMonth October = November 
getNextMonth November = December 
getNextMonth December = January 

getLastDayOfMonth :: Day -> Int -> Int -> Day
getLastDayOfMonth day monthDays currentDay 
  | monthDays == currentDay = day
  | otherwise = getLastDayOfMonth (getNextDay day) monthDays (currentDay+1) 

getDaysOfMonth :: Day -> Int -> Int -> Day -> [Int]
getDaysOfMonth day monthDays currentDay dayRequested
  | day == dayRequested && currentDay == monthDays = [currentDay]
  | day == dayRequested = currentDay : getDaysOfMonth (getNextDay day) monthDays (currentDay+1) dayRequested
  | currentDay == monthDays = []
  | otherwise = getDaysOfMonth (getNextDay day) monthDays (currentDay+1) dayRequested

isLeapYear :: Year -> Bool 
isLeapYear year = (year `mod` 4 == 0) && (year `mod` 100 /= 0 || year `mod` 400 == 0)  

getMaxDays :: Year -> Month -> Int
getMaxDays _ January = 31
getMaxDays year February  
  | isLeapYear year = 29 
  | otherwise = 28
getMaxDays _ March = 31
getMaxDays _ April = 30
getMaxDays _ May = 31
getMaxDays _ June = 30
getMaxDays _ July = 31
getMaxDays _ August = 31
getMaxDays _ September = 30
getMaxDays _ October = 31
getMaxDays _ November = 30
getMaxDays _ December = 31

main :: IO ()
main = undefined