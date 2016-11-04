module AddTwoNumbers where

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

addTwoNumbersAndPrint :: Int -> Int -> IO Int
addTwoNumbersAndPrint x y = do
  let result = addTwoNumbers x y
  print result
  return result

main :: IO ()
main = do
  addTwoNumbersAndPrint 3 5
  return ()
