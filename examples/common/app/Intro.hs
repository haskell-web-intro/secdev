module Intro where

removeBigNumbers :: Int -> Bool
removeBigNumbers x = x < 6

staticTypes :: String -> Bool
staticTypes lang
  | lang == "javascript" = False
  | lang == "ruby"       = False
  | lang == "haskell"    = True
  | lang == "scala"      = True
  | otherwise            = True

data Languages = JavaScript | Ruby | Haskell | Scala | Python deriving (Eq, Show)

betterStaticTypes :: Languages -> Bool
betterStaticTypes lang = case lang of
  JavaScript  -> False
  Ruby        -> False
  Haskell     -> True
  Scala       -> True
  Python      -> False

main :: IO ()
main = do
  print $ filter removeBigNumbers  [0,1,2,3,4,5,6,7,8,9,10]
  print $ filter staticTypes       ["javascript", "ruby", "python", "haskell"]
  print $ filter betterStaticTypes [JavaScript, Ruby, Haskell, Scala, Python]
--  print $ filter betterStaticTypes [Haskell, "bash"]
