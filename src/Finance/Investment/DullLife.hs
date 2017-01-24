-- | 

module Finance.Investment.DullLife where

import Finance.Investment
import Data.Time.Clock
import Control.Monad.Trans
import Data.Monoid



ist :: UTCTime -> InvestmentState Double
ist t = makeInvestmentState 0 0 0 t


-- salary, each month, 23rd
salary :: Investment Double ()
salary = do
  dn <- getMonthDay
  t <- getTime
  if dn == 23 && t == "18:00"
    then do
      m <- getMonth
      b <- getBalance
      balancePlus 36000
      balancePlus 18000
      balancePlus 18000
      writeLog $ "Salary, 72000"
    else balanceSame


birthday :: Investment Double ()
birthday = do
  d <- getDayAndMonth
  t <- getTime
  if d == "07.04" && t == "20:00"
    then do
      balanceMinus 10000
      writeLog "Organised birthday party (10000)"
    else balanceSame


bars :: Investment Double ()
bars = do
  d <- getToday
  t <- getTime
  if d == "Friday" && t == "21:00"
    then do
      balanceMinus 3000
    else balanceSame

-- spend 1000 on food every odd day at 8 p.m.
supermarkets :: Investment Double ()
supermarkets = do
  d <- getMonthDay
  t <- getTime
  if odd d && t == "20:00"
    then balanceMinus 1500
    else balanceSame

rent :: Investment Double ()
rent = do
  d <- getMonthDay
  t <- getTime
  if d == 22 && t == "19:00"
    then do
      balanceMinus 23000
      writeLog $ "Paid apartment rent (23000)"
    else balanceSame


dullLife :: Investment Double ()
dullLife = do
  salary
  supermarkets
  bars
  birthday
  rent
  
