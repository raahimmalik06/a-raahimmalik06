module Main where

import System.IO
import System.Exit
import Control.Monad (when)
import Data.List (intercalate)
import Types
import Operations
import Validation
import FileIO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "========================================"
  putStrLn "    🛒 HASKELL SHOPPING LIST MANAGER    "
  putStrLn "========================================"
  putStrLn "Functional Programming Project - PRG2214"
  putStrLn ""
  mainLoop []  -- Start with empty list

mainLoop :: ShoppingList -> IO ()
mainLoop list = do
  displayMenu
  choice <- getLine
  case choice of
    "1" -> addItemFlow list >>= mainLoop
    "2" -> viewListFlow list >> mainLoop list
    "3" -> removeItemFlow list >>= mainLoop
    "4" -> updateItemFlow list >>= mainLoop
    "5" -> filterItemsFlow list >> mainLoop list
    "6" -> calculateFlow list >> mainLoop list
    "7" -> saveFlow list >> mainLoop list
    "8" -> loadFlow >>= mainLoop
    "9" -> exportFlow list >> mainLoop list
    "0" -> do
      putStrLn "Goodbye! 👋"
      exitSuccess
    _ -> do
      putStrLn "Invalid choice. Please try again."
      mainLoop list

displayMenu :: IO ()
displayMenu = do
  putStrLn "\n════════════════ MAIN MENU ════════════════"
  putStrLn "1. ➕ Add New Item"
  putStrLn "2. 👁️  View List"
  putStrLn "3. ❌ Remove Item"
  putStrLn "4. ✏️  Update Item"
  putStrLn "5. 🔍 Filter Items"
  putStrLn "6. 🧮 Calculate Totals"
  putStrLn "7. 💾 Save List to File"
  putStrLn "8. 📂 Load List from File"
  putStrLn "9. 📄 Export Formatted Report"
  putStrLn "0. 🚪 Exit"
  putStrLn "══════════════════════════════════════════"
  putStr "Enter your choice (0-9): "

addItemFlow :: ShoppingList -> IO ShoppingList
addItemFlow list = do
  putStrLn "\n➕ ADD NEW ITEM"
  putStrLn "──────────────"
  
  newId <- return $ nextId list
  
  putStr "Item name: "
  name <- getLine
  
  putStr "Quantity: "
  qtyStr <- getLine
  let qtyEither = parseInt qtyStr
  
  putStr "Price per unit: $"
  priceStr <- getLine
  let priceEither = parseDouble priceStr
  
  putStr "Category (food/electronics/clothing/books/household/other): "
  catStr <- getLine
  let catEither = parseCategory catStr
  
  putStr "Priority (low/medium/high): "
  priStr <- getLine
  let priEither = parsePriority priStr
  
  case (qtyEither, priceEither, catEither, priEither) of
    (Right qty, Right price, Right cat, Right pri) -> do
      let newItem = ShoppingItem newId name qty price cat pri False
      case validateItem newItem of
        Right validItem -> do
          putStrLn $ "✅ Item added: " ++ itemName validItem
          return $ addItem validItem list
        Left err -> do
          putStrLn $ "❌ Validation error: " ++ err
          return list
    (Left err, _, _, _) -> do
      putStrLn $ "❌ Quantity error: " ++ err
      return list
    (_, Left err, _, _) -> do
      putStrLn $ "❌ Price error: " ++ err
      return list
    (_, _, Left err, _) -> do
      putStrLn $ "❌ Category error: " ++ err
      return list
    (_, _, _, Left err) -> do
      putStrLn $ "❌ Priority error: " ++ err
      return list

viewListFlow :: ShoppingList -> IO ()
viewListFlow list = do
  putStrLn "\n👁️  VIEW OPTIONS"
  putStrLn "─────────────"
  putStrLn "1. View All Items"
  putStrLn "2. View by Category"
  putStrLn "3. View by Priority"
  putStrLn "4. View Unpurchased Only"
  putStrLn "5. Sort by Priority"
  putStrLn "6. Sort by Category"
  putStr "Choice: "
  choice <- getLine
  
  let itemsToShow = case choice of
        "1" -> list
        "2" -> do
          putStr "Enter category: "
          catStr <- getLine
          case parseCategory catStr of
            Right cat -> filterByCategory cat list
            Left err -> do
              putStrLn $ "Error: " ++ err
              return list
        "3" -> do
          putStr "Enter priority (low/medium/high): "
          priStr <- getLine
          case parsePriority priStr of
            Right pri -> filterByPriority pri list
            Left err -> do
              putStrLn $ "Error: " ++ err
              return list
        "4" -> getUnpurchased list
        "5" -> sortByPriority list
        "6" -> sortByCategory list
        _ -> list
  
  if null itemsToShow
    then putStrLn "No items to display."
    else do
      putStrLn "\n📋 SHOPPING LIST"
      putStrLn "───────────────"
      mapM_ (putStrLn . printItem) itemsToShow
      putStrLn $ "\nTotal items: " ++ show (length itemsToShow)
      putStrLn $ "Total cost: $" ++ show (totalCost itemsToShow)

calculateFlow :: ShoppingList -> IO ()
calculateFlow list = do
  putStrLn "\n🧮 CALCULATION RESULTS"
  putStrLn "────────────────────"
  
  let total = totalCost list
  let byCategory = costByCategory list
  let unpurchased = getUnpurchased list
  let unpurchasedCost = totalCost unpurchased
  
  putStrLn $ "Total Items: " ++ show (length list)
  putStrLn $ "Total Cost: $" ++ show total
  putStrLn $ "Unpurchased Items: " ++ show (length unpurchased)
  putStrLn $ "Remaining Cost: $" ++ show unpurchasedCost
  putStrLn "\nCost by Category:"
  mapM_ (\(cat, cost) -> 
    putStrLn $ "  • " ++ show cat ++ ": $" ++ show cost) byCategory

saveFlow :: ShoppingList -> IO ()
saveFlow list = do
  putStr "Enter filename (default: shopping_list.txt): "
  filename <- getLine
  let file = if null filename then "shopping_list.txt" else filename
  saveList file list

loadFlow :: IO ShoppingList
loadFlow = do
  putStr "Enter filename to load (default: shopping_list.txt): "
  filename <- getLine
  let file = if null filename then "shopping_list.txt" else filename
  
  result <- loadList file
  case result of
    Right loadedList -> do
      putStrLn $ "✅ Loaded " ++ show (length loadedList) ++ " items"
      return loadedList
    Left err -> do
      putStrLn $ "❌ Error: " ++ err
      return []

exportFlow :: ShoppingList -> IO ()
exportFlow list = do
  putStr "Enter filename for report (default: shopping_report.txt): "
  filename <- getLine
  let file = if null filename then "shopping_report.txt" else filename
  exportFormatted file list

removeItemFlow :: ShoppingList -> IO ShoppingList
removeItemFlow list = do
  putStrLn "\n❌ REMOVE ITEM"
  putStrLn "─────────────"
  if null list
    then do
      putStrLn "List is empty."
      return list
    else do
      putStrLn "Current items:"
      mapM_ (putStrLn . printItem) list
      putStr "Enter ID to remove: "
      idStr <- getLine
      case parseInt idStr of
        Right id -> do
          let newList = removeItem id list
          if length newList == length list
            then do
              putStrLn "Item not found."
              return list
            else do
              putStrLn "✅ Item removed."
              return newList
        Left err -> do
          putStrLn $ "❌ Error: " ++ err
          return list

updateItemFlow :: ShoppingList -> IO ShoppingList
updateItemFlow list = do
  putStrLn "\n✏️  UPDATE ITEM"
  putStrLn "─────────────"
  if null list
    then do
      putStrLn "List is empty."
      return list
    else do
      putStrLn "Current items:"
      mapM_ (putStrLn . printItem) list
      putStr "Enter ID to update: "
      idStr <- getLine
      case parseInt idStr of
        Right id -> 
          case filter (\item -> itemId item == id) list of
            [] -> do
              putStrLn "Item not found."
              return list
            [oldItem] -> do
              putStrLn $ "Updating: " ++ printItem oldItem
              putStrLn "Enter new details (press Enter to keep current):"
              
              putStr $ "Name [" ++ itemName oldItem ++ "]: "
              nameInput <- getLine
              let name = if null nameInput then itemName oldItem else nameInput
              
              putStr $ "Quantity [" ++ show (quantity oldItem) ++ "]: "
              qtyInput <- getLine
              let qty = if null qtyInput then quantity oldItem 
                       else case parseInt qtyInput of
                              Right n -> n
                              Left _ -> quantity oldItem
              
              putStr $ "Price [$" ++ show (price oldItem) ++ "]: "
              priceInput <- getLine
              let price = if null priceInput then price oldItem
                         else case parseDouble priceInput of
                                Right p -> p
                                Left _ -> price oldItem
              
              putStr $ "Category [" ++ show (category oldItem) ++ "]: "
              catInput <- getLine
              let cat = if null catInput then category oldItem
                       else case parseCategory catInput of
                              Right c -> c
                              Left _ -> category oldItem
              
              putStr $ "Priority [" ++ show (priority oldItem) ++ "]: "
              priInput <- getLine
              let pri = if null priInput then priority oldItem
                       else case parsePriority priInput of
                              Right p -> p
                              Left _ -> priority oldItem
              
              putStr $ "Purchased? [y/N]: "
              purInput <- getLine
              let purchased = case map toLower purInput of
                                "y" -> True
                                _ -> False
              
              let updatedItem = ShoppingItem id name qty price cat pri purchased
              case validateItem updatedItem of
                Right validItem -> do
                  putStrLn "✅ Item updated."
                  return $ map (\item -> if itemId item == id then validItem else item) list
                Left err -> do
                  putStrLn $ "❌ Validation error: " ++ err
                  return list
            _ -> do
              putStrLn "Error: Multiple items with same ID."
              return list
        Left err -> do
          putStrLn $ "❌ Error: " ++ err
          return list

filterItemsFlow :: ShoppingList -> IO ()
filterItemsFlow list = do
  putStrLn "\n🔍 FILTER ITEMS"
  putStrLn "─────────────"
  putStrLn "1. Filter by Category"
  putStrLn "2. Filter by Priority"
  putStrLn "3. Show Unpurchased"
  putStrLn "4. Show High Priority"
  putStr "Choice: "
  choice <- getLine
  
  let filteredList = case choice of
        "1" -> do
          putStr "Enter category: "
          catStr <- getLine
          case parseCategory catStr of
            Right cat -> filterByCategory cat list
            Left err -> do
              putStrLn $ "Error: " ++ err
              return []
        "2" -> do
          putStr "Enter priority: "
          priStr <- getLine
          case parsePriority priStr of
            Right pri -> filterByPriority pri list
            Left err -> do
              putStrLn $ "Error: " ++ err
              return []
        "3" -> getUnpurchased list
        "4" -> filterByPriority High list
        _ -> list
  
  if null filteredList
    then putStrLn "No items match the filter."
    else do
      putStrLn "\n📋 FILTERED ITEMS"
      putStrLn "────────────────"
      mapM_ (putStrLn . printItem) filteredList
      putStrLn $ "\nFound " ++ show (length filteredList) ++ " items"
      putStrLn $ "Total cost: $" ++ show (totalCost filteredList)

toLower :: Char -> Char
toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c