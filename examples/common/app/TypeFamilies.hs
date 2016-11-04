{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module TypeFamilies where

data Democrat   = Obama   | LBJ         deriving Show
data Republican = Lincoln | Eisenhower  deriving Show

class Show (Policy p) => Politician p where
  data Policy p :: *
  govern :: p -> Policy p

instance Politician Democrat where
  data Policy Democrat = Obamacare | CivilRights  deriving Show
  govern Obama = Obamacare
  govern LBJ   = CivilRights

instance Politician Republican where
  data Policy Republican = EstablishNASA | ThirteenthAmendment deriving Show
  govern Lincoln    = ThirteenthAmendment
  govern Eisenhower = EstablishNASA

data Strategy = Subsidize | Amend | Legislate deriving Show

class (Politician p) => Enact p where
  enact :: p -> Strategy

instance Enact Democrat where
  enact p = case govern p of
    Obamacare   -> Subsidize
    CivilRights -> Legislate

instance Enact Republican where
  enact p = case govern p of
    ThirteenthAmendment -> Amend
    EstablishNASA       -> Subsidize

main :: IO ()
main = do
  print $ govern Obama
  print $ enact LBJ
