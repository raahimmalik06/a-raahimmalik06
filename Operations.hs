module Operations where

import Types

addItem :: ShoppingItem -> ShoppingList -> ShoppingList
addItem item list = item : list

removeItem :: Int -> ShoppingList -> ShoppingList
removeItem id = filter (\item -> itemId item /= id)

updateItem :: Int -> ShoppingItem -> ShoppingList -> ShoppingItem
updateItem targetId newItem [] = error "Item not found"
updateItem targetId newItem (item:items)
  | itemId item == targetId = newItem { itemId = targetId }
  | otherwise = updateItem targetId newItem items

filterByCategory :: Category -> ShoppingList -> ShoppingList
filterByCategory cat = filter (\item -> category item == cat)

filterByPriority :: Priority -> ShoppingList -> ShoppingList
filterByPriority pri = filter (\item -> priority item == pri)

getUnpurchased :: ShoppingList -> ShoppingList
getUnpurchased = filter (not . purchased)

totalCost :: ShoppingList -> Double
totalCost = foldr (\item acc -> acc + price item * fromIntegral (quantity item)) 0.0

costByCategory :: ShoppingList -> [(Category, Double)]
costByCategory list = 
  let categories = [minBound..maxBound] :: [Category]
  in map (\cat -> (cat, totalCost (filterByCategory cat list))) categories

markAsPurchased :: Int -> ShoppingList -> ShoppingList
markAsPurchased id list = 
  map (\item -> if itemId item == id then item { purchased = True } else item) list

nextId :: ShoppingList -> Int
nextId [] = 1
nextId items = maximum (map itemId items) + 1

sortByPriority :: ShoppingList -> ShoppingList
sortByPriority = sortByPriorityDesc
  where
    priorityValue High = 3
    priorityValue Medium = 2
    priorityValue Low = 1
    sortByPriorityDesc = sortBy (\a b -> compare (priorityValue b) (priorityValue a))

sortByCategory :: ShoppingList -> ShoppingList
sortByCategory = sortBy (\a b -> compare (category a) (category b))

sortBy :: (ShoppingItem -> ShoppingItem -> Ordering) -> ShoppingList -> ShoppingList
sortBy cmp = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys)
      | cmp x y == GT = x : y : ys
      | otherwise = y : insert x ys