module Except.TypeDef where

data Exception =
  ParseException String |
  ArgumentException

instance Show Exception where
  show (ParseException str) = show str
  show ArgumentException = "Invalid arguments"

instance Eq Exception where
  (ParseException _) == (ParseException _) = True
  ArgumentException == ArgumentException = True
  _ == _ = False
