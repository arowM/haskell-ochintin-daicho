{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

{- |
Module      :  Business.OchintinDaicho

Copyright   :  Kadzuya Okamoto 2017
License     :  MIT

Stability   :  experimental
Portability :  unknown

This module exports core functions and types for payroll books.
-}
module Business.OchintinDaicho
  (
  -- * Usage examples
  -- $setup

  -- * Constructors
    datePayment
  -- * Types
  , Person(..)
  , Payment(..)
  , PaymentCore(..)
  , PaymentMeta(..)
  , Payments
  , Hours

  -- * Pretty printers
  , ppr

  -- * Converters
  , toBookkeeping

  -- * Convenient functions for 年末調整
  , _年度内課税支給金額
  , _年度内社会保険控除額
  , _年度内所得税控除額
  ) where

import Business.Bookkeeping
  ( Amount
  , Category(..)
  , CategoryType(..)
  , CreditCategory(..)
  , Date
  , DateTransactions
  , DebitCategory(..)
  , Month
  , Journal
  , Transactions
  , Year
  , activity
  , dateTrans
  , month
  , tAmount
  , year
  )
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.MonoTraversable (oforM_, osum)
import Data.String (IsString(..))
import Data.Transaction (Transaction, action, tMap)


{- $setup
>>> import Business.Bookkeeping hiding (ppr)
>>> :{
let
  taroYamada :: Person
  taroYamada = Person
    { name = "山田 太郎"
    , birthday = "昭和60年1月1日"
    , address = "東京都新宿区"
    , job = "エンジニア"
    , left = ""
    , reason = ""
    , history = ""
    , sex = "男"
    , payments = \y -> case y of
      2017 -> do
        datePayment 1 10 $
          Payment
            ( PaymentMeta
              { _賃金計算期間 = (2016, 12)
              , _労働日数 = 13
              , _労働時間数 = 13 * 5
              , _休日労働時間数 = 0
              , _早出残業時間数 = 0
              , _深夜労働時間数 = 0
              }
            )
            ( PaymentCore
              { _課税支給額 = 250000
              , _控除社会保険料 = \debit amount -> do
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "健康保険料(社員負担分)"
                  $ if (200000 < amount && amount < 300000)
                    then 15000
                    else error "undefined 健康保険料"
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "厚生年金(社員負担分)"
                  $ if (200000 < amount && amount < 300000)
                    then 20000
                    else error "undefined 厚生年金"
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "雇用保険料(社員負担分)"
                  $ if (200000 < amount && amount < 300000)
                    then 1000
                    else error "undefined 雇用保険料"
              , _所得税額 = \debit amount ->
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "源泉所得税"
                  $ if (200000 < amount && amount < 250000)
                    then 5000
                    else error $ "undefined 所得税額 for " <> show amount
              , _非課税支給額 = \credit -> do
                dateTrans
                  (DebitCategory $ Category "旅費・交通費" Expenses)
                  credit
                  "立替交通費"
                  3000
              , _その他控除 = \debit ->
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "親睦会費"
                  5000
              }
            )
        datePayment 2 10 $
          Payment
            ( PaymentMeta
              { _賃金計算期間 = (2017, 1)
              , _労働日数 = 12
              , _労働時間数 = 12 * 5
              , _休日労働時間数 = 0
              , _早出残業時間数 = 0
              , _深夜労働時間数 = 0
              }
            )
            ( PaymentCore
              { _課税支給額 = 250000
              , _控除社会保険料 = \debit amount -> do
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "健康保険料(社員負担分)"
                  $ if (200000 < amount && amount < 300000)
                    then 15000
                    else error "undefined 健康保険料"
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "厚生年金(社員負担分)"
                  $ if (200000 < amount && amount < 300000)
                    then 20000
                    else error "undefined 厚生年金"
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "雇用保険料(社員負担分)"
                  $ if (200000 < amount && amount < 300000)
                    then 1000
                    else error "undefined 雇用保険料"
              , _所得税額 = \debit amount ->
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "源泉所得税"
                  $ if (200000 < amount && amount < 250000)
                    then 5000
                    else error $ "undefined 所得税額2 for " <> show amount
              , _非課税支給額 = \credit -> do
                dateTrans
                  (DebitCategory $ Category "旅費・交通費" Expenses)
                  credit
                  "立替交通費"
                  4000
              , _その他控除 = \debit ->
                dateTrans debit
                  (CreditCategory $ Category "預り金" Liabilities)
                  "親睦会費"
                  5000
              }
            )
      _ ->
        pure ()
    }
:}
-}

{-| 対象年度内の課税支給額総計

>>> _年度内課税支給金額 taroYamada 2017
Amount {unAmount = 500000}
-}
_年度内課税支給金額 :: Person -> Year -> Amount
_年度内課税支給金額 Person {..} =
  osum . tMap (_課税支給額 . core . snd) . payments

{-| 対象年度内の社会保険料等控除額総計

>>> _年度内社会保険控除額 taroYamada 2017
Amount {unAmount = 72000}
-}
_年度内社会保険控除額 :: Person -> Year -> Amount
_年度内社会保険控除額 Person {..} =
  osum . tMap (_算出保険料 . snd) . payments

{-| 対象年度内の所得税控除額総計

>>> _年度内所得税控除額 taroYamada 2017
Amount {unAmount = 10000}
-}
_年度内所得税控除額 :: Person -> Year -> Amount
_年度内所得税控除額 Person {..} =
  osum . tMap (_算出所得税 . snd) . payments

{-| A pretty printer for payroll books.

>>> ppr taroYamada 2017
氏名: 山田 太郎
性別: 男
<BLANKLINE>
== 2017年支給分 ==
<BLANKLINE>
支給日: 1月10日
賃金計算期間: 2016年12月
労働日数: 13日
労働時間数: 65時間
休日労働時間数: 0時間
早出残業時間数: 0時間
深夜労働時間数: 0時間
課税支給額: 250000円
控除社会保険料: 36000円
社会保険料等控除の金額: 214000円
所得税額: 5000円
非課税支給額: 3000円
その他控除: 5000円
--> 実支払額: 207000円
<BLANKLINE>
支給日: 2月10日
賃金計算期間: 2017年1月
労働日数: 12日
労働時間数: 60時間
休日労働時間数: 0時間
早出残業時間数: 0時間
深夜労働時間数: 0時間
課税支給額: 250000円
控除社会保険料: 36000円
社会保険料等控除の金額: 214000円
所得税額: 5000円
非課税支給額: 4000円
その他控除: 5000円
--> 実支払額: 208000円
<BLANKLINE>
-}
ppr :: Person -> Year -> IO ()
ppr Person {..} y = do
  T.putStrLn $ "氏名: " <> name
  T.putStrLn $ "性別: " <> sex
  T.putStrLn ""
  T.putStrLn . T.concat $
    [ "== "
    , tshowIntegral $ y
    , "年支給分 =="
    ]
  T.putStrLn ""
  oforM_ (payments y) $ \((m, d), payment@(Payment (PaymentMeta {..}) (PaymentCore {..}))) -> do
    let
      (targetYear, targetMonth) = _賃金計算期間
      _算出保険料' = _算出保険料 payment
      _算出所得税' = _算出所得税 payment
      _非課税支給額総計' = _非課税支給額総計 payment
      _その他控除総計' = _その他控除総計 payment
      _実支払額' = _実支払額 payment
    T.putStrLn $ "支給日: " <> tshowIntegral m <> "月" <> tshowIntegral d <> "日"
    T.putStrLn $ "賃金計算期間: " <> tshowIntegral targetYear <> "年" <> tshowIntegral targetMonth <> "月"
    T.putStrLn $ "労働日数: " <> tshow _労働日数 <> "日"
    T.putStrLn $ "労働時間数: " <> tshowIntegral _労働時間数 <> "時間"
    T.putStrLn $ "休日労働時間数: " <> tshowIntegral _休日労働時間数 <> "時間"
    T.putStrLn $ "早出残業時間数: " <> tshowIntegral _早出残業時間数 <> "時間"
    T.putStrLn $ "深夜労働時間数: " <> tshowIntegral _深夜労働時間数 <> "時間"
    T.putStrLn $ "課税支給額: " <> tshowIntegral _課税支給額 <> "円"
    T.putStrLn $ "控除社会保険料: " <> tshowIntegral _算出保険料' <> "円"
    T.putStrLn $ "社会保険料等控除の金額: " <> tshowIntegral (_課税支給額 - _算出保険料') <> "円"
    T.putStrLn $ "所得税額: " <> tshowIntegral _算出所得税' <> "円"
    T.putStrLn $ "非課税支給額: " <> tshowIntegral _非課税支給額総計' <> "円"
    T.putStrLn $ "その他控除: " <> tshowIntegral _その他控除総計' <> "円"
    T.putStrLn $ "--> 実支払額: " <> tshowIntegral _実支払額' <> "円"
    T.putStrLn ""

{-|

>>> :{
Business.Bookkeeping.ppr $ toBookkeeping taroYamada 2017
  (DebitCategory $ Category "給与手当" Expenses)
  (CreditCategory $ Category "普通預金" Liabilities)
:}
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 給与支払い
tDebit: 給与手当 (Expenses)
tCredit: 普通預金 (Liabilities)
tAmount: 204000
<BLANKLINE>
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 立替交通費
tDebit: 旅費・交通費 (Expenses)
tCredit: 普通預金 (Liabilities)
tAmount: 3000
<BLANKLINE>
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 健康保険料(社員負担分)
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 15000
<BLANKLINE>
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 厚生年金(社員負担分)
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 20000
<BLANKLINE>
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 雇用保険料(社員負担分)
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 1000
<BLANKLINE>
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 源泉所得税
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 5000
<BLANKLINE>
tDay: 2017-01-10
tDescription: 12月度
tSubDescription: 親睦会費
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 5000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 給与支払い
tDebit: 給与手当 (Expenses)
tCredit: 普通預金 (Liabilities)
tAmount: 204000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 立替交通費
tDebit: 旅費・交通費 (Expenses)
tCredit: 普通預金 (Liabilities)
tAmount: 4000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 健康保険料(社員負担分)
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 15000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 厚生年金(社員負担分)
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 20000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 雇用保険料(社員負担分)
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 1000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 源泉所得税
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 5000
<BLANKLINE>
tDay: 2017-02-10
tDescription: 1月度
tSubDescription: 親睦会費
tDebit: 給与手当 (Expenses)
tCredit: 預り金 (Liabilities)
tAmount: 5000
<BLANKLINE>

-}
toBookkeeping :: Person -> Year -> DebitCategory -> CreditCategory -> Transactions
toBookkeeping Person {..} y debit credit =
  year y $
    oforM_ (payments y) $ \((m, d), payment@(Payment (PaymentMeta {..}) (PaymentCore {..}))) -> do
      let
        (_, targetMonth) = _賃金計算期間
        _算出保険料' = _算出保険料 payment
      month m $
        activity d (fromString $ (show . toInteger) targetMonth <> "月度") $ do
          dateTrans debit credit "給与支払い" $
            _課税支給額
            - _算出保険料'
            - _算出所得税 payment
            - _その他控除総計 payment
          _非課税支給額 credit
          _控除社会保険料 debit _課税支給額
          _所得税額 debit (_課税支給額 - _算出保険料')
          _その他控除 debit

datePayment :: Month -> Date -> Payment -> Payments
datePayment m d p =
  action ((m, d), p)

{- ==============
 -     Types
 - ============== -}

{-| Main data type to represent payroll books for a person.
 - It can be also used as a 労働者名簿
 -}
data Person = Person
  { name :: Text  -- 氏名
  , birthday :: Text  -- 生年月日
  , sex :: Text   -- 性別
  , address :: Text -- 住所
  , job :: Text   -- 業務の種類
  , left :: Text  -- 退職または死亡年月日
  , reason :: Text  -- 退職または死亡の理由
  , history :: Text   -- 履歴
  , payments :: Year -> Payments
  }

{-| A type for handling `Payment` values.
 -}
type Payments = Transaction ((Month, Date), Payment)

data Payment = Payment
  { meta :: PaymentMeta
  , core :: PaymentCore
  }

data PaymentMeta = PaymentMeta
  { _賃金計算期間 :: (Year, Month)
  , _労働日数 :: Int
  , _労働時間数 :: Hours
  , _休日労働時間数 :: Hours
  , _早出残業時間数 :: Hours
  , _深夜労働時間数 :: Hours
  } deriving (Show, Read, Eq)

data PaymentCore = PaymentCore
  { _課税支給額 :: Amount
  , _非課税支給額 :: CreditCategory -> DateTransactions
  , _控除社会保険料 :: DebitCategory -> Amount -> DateTransactions
  , _所得税額 :: DebitCategory -> Amount -> DateTransactions
  , _その他控除 :: DebitCategory -> DateTransactions
  }

newtype Hours = Hours { _unHours :: Int }
  deriving (Show, Read, Ord, Eq, Num, Enum, Integral, Real)


-- Helper functions
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show


tshowIntegral :: (Integral a) => a -> T.Text
tshowIntegral = T.pack . show . toInteger


dummyDebit :: DebitCategory
dummyDebit = DebitCategory $ Category "DUMMY!" Expenses

dummyCredit :: CreditCategory
dummyCredit = CreditCategory $ Category "DUMMY!" Liabilities

_算出保険料 :: Payment -> Amount
_算出保険料 (Payment _ PaymentCore {..}) = sumAmounts $ _控除社会保険料 dummyDebit _課税支給額

_算出所得税 :: Payment -> Amount
_算出所得税 payment@(Payment _ PaymentCore {..}) = sumAmounts $ _所得税額 dummyDebit (_課税支給額 - _算出保険料 payment)

_非課税支給額総計 :: Payment -> Amount
_非課税支給額総計 (Payment _ PaymentCore {..}) = sumAmounts $ _非課税支給額 dummyCredit

_その他控除総計 :: Payment -> Amount
_その他控除総計 (Payment _ PaymentCore {..}) = sumAmounts $ _その他控除 dummyDebit

_実支払額 :: Payment -> Amount
_実支払額 payment@(Payment _ PaymentCore {..}) = _課税支給額 - _算出保険料 payment - _算出所得税 payment - _その他控除総計 payment + _非課税支給額総計 payment

runDummyActivity :: DateTransactions -> Transaction Journal
runDummyActivity = year 1 . month 1 . activity 1 ""

sumAmounts :: DateTransactions -> Amount
sumAmounts = osum . tMap tAmount . runDummyActivity
