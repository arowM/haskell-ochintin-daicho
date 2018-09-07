{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

{- |
Module      :  Business.OchintinDaicho.Person

Copyright   :  Kadzuya Okamoto 2017
License     :  MIT

Stability   :  experimental
Portability :  unknown

This module exports core functions and types for payroll books.
-}
module Business.OchintinDaicho.Person
  (
  -- * Pretty printers
    ppr
  , markdown
  ) where

import Business.OchintinDaicho (Person(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

ppr :: Person -> IO ()
ppr Person {..} = do
  T.putStrLn $ "氏名: " <> name
  T.putStrLn $ "生年月日: " <> birthday
  T.putStrLn $ "性別: " <> sex
  T.putStrLn $ "住所: " <> address
  T.putStrLn $ "業務の種類: " <> job
  T.putStrLn $ "退職または死亡年月日: " <> left
  T.putStrLn $ "退職または死亡の理由: " <> reason
  T.putStrLn $ "履歴: " <> history


markdown :: Person -> Text
markdown Person {..} =
  T.unlines
    [ "## 氏名: " <> name
    , ""
    , "* 生年月日: " <> birthday
    , "* 性別: " <> sex
    , "* 住所: " <> address
    , "* 業務の種類: " <> job
    , "* 退職または死亡年月日: " <> left
    , "* 退職または死亡の理由: " <> reason
    , "* 履歴: " <> history
    ]

