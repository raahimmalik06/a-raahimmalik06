{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics (Generic)

data Category = Food 
              | Electronics 
              | Clothing 
              | Books 
              | Household 
              | Other
              deriving (Show, Eq, Read, Enum, Bounded)

data Priority = Low | Medium | High 
              deriving (Show, Eq, Read, Enum)

data ShoppingItem = ShoppingItem
  { itemId     :: Int          -- Unique identifier
  , itemName   :: String       -- Name of item
  , quantity   :: Int          -- Quantity needed
  , price      :: Double       -- Price per unit
  , category   :: Category     -- Category (polymorphic)
  , priority   :: Priority     -- Priority level
  , purchased  :: Bool         -- Whether purchased
  } deriving (Show, Eq, Generic)

type ShoppingList = [ShoppingItem]

class Printable a where
  printItem :: a -> String

instance Printable ShoppingItem where
  printItem item = 
    let status = if purchased item then "[✓]" else "[ ]"
        priceStr = "$" ++ show (price item)
        totalCost = "$" ++ show (price item * fromIntegral (quantity item))
    in unwords [show (itemId item), status, itemName item, 
                "x" ++ show (quantity item), 
                "@" ++ priceStr, "=" ++ totalCost,
                "(" ++ show (category item) ++ ", " ++ show (priority item) ++ ")"]

data Command = AddCommand ShoppingItem
             | RemoveCommand Int
             | UpdateCommand Int ShoppingItem
             | ViewCommand (Maybe Category)
             | CalculateCommand
             | FilterByPriority Priority
             | SaveCommand String
             | LoadCommand String
             | ExitCommand
             | InvalidCommand String
             deriving (Show, Eq)