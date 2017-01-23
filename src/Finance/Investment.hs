-- | Investment monad

module Finance.Investment where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Control.Monad.State


class (Eq a, Num a, Show a) => Funds a

-- | Investment state
data InvestmentState a = InvestmentState
  { inputSum :: a
  -- ^ The deposit, real money behind the investment
  , outputSum :: a
  -- ^ Withdraw account, the earnings from the investment
  , balanceSum :: a
  -- ^ The internal money, currently accumulated as a result of the investment (subject to withdraw)
  , startTime :: UTCTime
  -- ^ Investment creation date
  , currentTime :: UTCTime
  -- ^ Current date
  , logBook :: [String]
  -- ^ Log of funds movements, etc.
  }

{-
instance Show InvestmentState where
  show (InvestmentState i o b st ct) = "InvestmentState {inputSum = " ++ s i ++ ", " ++
    "outputSum = " ++ s o ++ ", bufferSum = " ++ s b ++ ", startDate = " ++ s st ++ ", currentTime = " ++ s ct ++ "}"
    where
      s e = show e
-}

-- | The investment monad type
type Investment a r = StateT (InvestmentState a) IO r


-- | Construct new investment state
makeInvestmentState :: Funds a => a -> a -> a -> UTCTime -> InvestmentState a
makeInvestmentState is os bs t = InvestmentState is os bs t t [s]
  where s = "Log book created at " ++ show t


-- | The investment's balance's change
balanceChange :: Funds a => (a -> a) -> Investment a ()
balanceChange f = do
  bsum <- gets balanceSum
  let bsum' = f bsum
  modify (\ist -> ist {balanceSum = bsum'})


-- | Increase balance value
balancePlus :: Funds a => a -> Investment a ()
balancePlus p = balanceChange ( + p)

-- | Decrease balance value
balanceMinus :: Funds a => a -> Investment a ()
balanceMinus m = balanceChange (\b -> b - m)

-- | Convenience balance no-op
balanceSame :: Investment a ()
balanceSame = return ()


-- | Add N seconds to UTCTime
addUTCTimeSeconds :: Int -> UTCTime -> UTCTime
addUTCTimeSeconds nsec = addUTCTime (toEnum nsec * 10^12)


-- | Run the investment monad with the given precision (minimal time step resolution)
runInvestmentPrec :: (UTCTime -> UTCTime) -> Investment a r -> InvestmentState a -> IO (r, InvestmentState a)
runInvestmentPrec tstep i = runStateT i'
  where
    i' = do
      curt <- gets currentTime
      modify (\ist -> ist {currentTime = tstep curt})
      -- do user-provided actions
      i


-- | Run the investment monad once with minimal allowed time step (1 minute)
runInvestmentOneMinute :: Investment a r -> InvestmentState a -> IO (r, InvestmentState a)
runInvestmentOneMinute = runInvestmentPrec f
  where f = addUTCTimeSeconds 60

-- | Eval the investment monad for N minutes, giving back the changed state
evalInvestmentMinutes :: Integer -> Investment a r -> InvestmentState a -> IO (InvestmentState a)
evalInvestmentMinutes n _ oi | n < 0 = return oi
evalInvestmentMinutes 0 _ oi = return oi
evalInvestmentMinutes n im oi = do
  ni <- runInvestmentOneMinute im oi
  evalInvestmentMinutes (n - 1) im (snd ni)

-- | Run the investment monad N hours
evalInvestmentHours n = evalInvestmentMinutes (n * 60)

-- | Run the investment monad N days
evalInvestmentDays n = evalInvestmentHours (n * 24)


--------------------------------------------------------
-- Various getters to be used within Investment monad
--------------------------------------------------------


-- | Withdraw funds (transfer from buffer balance to the output sum), returns True on success
withdraw :: Funds a => a -> Investment a Bool
withdraw x = do
  bsum <- gets balanceSum
  osum <- gets outputSum
  -- XXX use signum over typeclass Num
  let si = signum (bsum - x)
  if si == 0 || si == 1
    then do
      let bsum' = bsum - x
      let osum' = osum + x
      _ <- modify (\ist -> ist {balanceSum = bsum', outputSum = osum'})
      -- make a record in the log book
      writeLog $ "Withdraw of " ++ show x
      return True
    else do
      writeLog $ "FAILED withdraw of " ++ show x
      return False

-- | Deposit funds (increase 'inputSum' indicating that even more money has been invested)
deposit :: Funds a => a -> Investment a ()
deposit x = do
  isum <- gets inputSum
  modify (\ist -> ist {inputSum = isum + x})
  writeLog $ "Deposit of " ++ show x


getDeposit, getEarnings, getBalance :: Funds a => Investment a a
-- | Get current input sum from the investment
getDeposit = gets inputSum
-- | Get current output sum from the investment
getEarnings = gets outputSum
-- | Get current balance from the investment
getBalance = gets balanceSum

-- | Get current UTC time from the investment
getUTCTime :: Investment a UTCTime
getUTCTime = gets currentTime


-- | Get current time: 24-hour and minute string (i.e. "13:15")
getHourAndMinute :: Investment a String
getHourAndMinute = do
  curt <- getUTCTime
  let hm = formatTime defaultTimeLocale "%R" curt
  return hm

-- | Synonym for 'getHourAndMinute'
getTime = getHourAndMinute


-- | Get current month number (1..12)
getMonthNum :: Investment a Int
getMonthNum = do
  curt <- getUTCTime
  let (_, mn, _) = toGregorian $ utctDay curt
  return mn

-- | Get current month name ("January")
getMonth :: Investment a String
getMonth = do
  curt <- getUTCTime
  let mn = formatTime defaultTimeLocale "%B" curt
  return mn

-- | Get current month day num (1..31)
getMonthDay :: Investment a Int
getMonthDay = do
  curt <- getUTCTime
  let (_, _, md) = toGregorian $ utctDay curt
  return md


-- | Get current day and month string ("27.01")
getDayAndMonth :: Investment a String
getDayAndMonth = do
  curt <- getUTCTime
  let dam = formatTime defaultTimeLocale "%d.%m" curt
  return dam

-- | Synonym for 'getDayAndMonth'
getDate = getDayAndMonth


-- | Get current full date ("27.01.2017")
getFullDate :: Investment a String
getFullDate = do 
  curt <- getUTCTime
  let dam = formatTime defaultTimeLocale "%d.%m.%Y" curt
  return dam


-- | Get current weekday name ("Sunday")
getWeekDay :: Investment a String
getWeekDay = do
  curt <- getUTCTime
  let wd = formatTime defaultTimeLocale "%A" (utctDay curt)
  return wd

-- | Synonym for 'getWeekDay'
getToday = getWeekDay


-- | Append a string to the financeLog (prepends it with the current time)
writeLog :: String -> Investment a ()
writeLog s = do
  l <- gets logBook
  fd <- getFullDate
  d <- getToday
  t <- getTime
  let s' = fd ++ ", " ++ d ++ ", " ++ t ++ ": " ++ s
  modify (\ist -> ist {logBook = l ++ [s']})

-- | Convenience function returning the log book from 'InvestmentState' as string
readLog = unlines . logBook
