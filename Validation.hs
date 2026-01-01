module Validation where

import Types

validateItem :: ShoppingItem -> Either String ShoppingItem
validateItem item
  | null (itemName item) = Left "Item name cannot be empty"
  | quantity item <= 0 = Left "Quantity must be positive"
  | price item < 0 = Left "Price cannot be negative"
  | otherwise = Right item

parseCategory :: String -> Either String Category
parseCategory str = case str of
  "food" -> Right Food
  "electronics" -> Right Electronics
  "clothing" -> Right Clothing
  "books" -> Right Books
  "household" -> Right Household
  "other" -> Right Other
  _ -> Left $ "Unknown category. Choose from: " ++ show ([minBound..maxBound] :: [Category])

parsePriority :: String -> Either String Priority
parsePriority str = case str of
  "low" -> Right Low
  "medium" -> Right Medium
  "high" -> Right High
  _ -> Left "Priority must be: low, medium, or high"

parseInt :: String -> Either String Int
parseInt str = case reads str of
  [(n, "")] -> Right n
  _ -> Left "Not a valid number"

parseDouble :: String -> Either String Double
parseDouble str = case reads str of
  [(n, "")] -> Right n
  _ -> Left "Not a valid price"