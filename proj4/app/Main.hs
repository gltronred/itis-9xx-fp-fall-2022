{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Functor.Identity

import Control.Lens
import Control.Lens.TH

data Room = Room
  { _table :: Maybe Table
  , _persons :: [Person]
  }
  deriving (Eq,Show,Read)

data Table = Table
  { _legs :: Int
  , _colour :: Colour
  }
  deriving (Eq,Show,Read)

data Colour = White | Red | Green | Blue | Black
  deriving (Eq,Show,Read)

data Person = Person
  { _surname :: String
  , _name :: String
  , _patronym :: Maybe String
  }
  deriving (Eq,Show,Read)

makeLenses ''Person
makeLenses ''Colour
makeLenses ''Table
makeLenses ''Room

rooms :: [Room]
rooms = [ Room{ _table=Just $ Table{ _legs=3, _colour=White }
              , _persons=
                [ Person{ _surname="A"
                        , _name="B"
                        , _patronym=Just "C"}
                , Person{ _surname="D"
                        , _name="E"
                        , _patronym=Nothing}]}
        , Room{ _table=Nothing
              , _persons=
                [ Person{ _surname="G"
                        , _name="H"
                        , _patronym=Just "I"} ]}]

-- Сломать ножку у каждого стола в каждой комнате
--
-- Пример неудобной работы с глубоко вложенными
-- структурами
breakTableLeg :: [Room] -> [Room]
breakTableLeg rooms =
  [ r{ _table = case _table r of
          Nothing -> Nothing
          Just t -> Just t{ _legs = _legs t - 1 }
     }
  | r <- rooms ]

-- Попытка 2: вытаскиваем отдельные функции
--
-- Можно ли этот подход обобщить и сделать
-- композируемым?
breakTableLeg2 :: [Room] -> [Room]
breakTableLeg2 = map breakTableInRoom
  where
    breakTableInRoom :: Room -> Room
    breakTableInRoom r = r{_table = breakTable<$>_table r}
    breakTable :: Table -> Table
    breakTable t = t{ _legs = _legs t - 1 }

--------------------------------------------------------

-- Попытка написать ООП в стиле ФП
data LensTry0 s a = LensTry0
  { getter :: s -> a
  , setter :: a -> s -> s
  }

_fst0 :: LensTry0 (a,b) a
_fst0 = LensTry0
  { getter = \(a,b) -> a
  , setter = \newA (oldA,oldB) -> (newA,oldB)
  }

ix0 :: Int -> LensTry0 [a] a
ix0 i = LensTry0
  { getter = (!! i)
  , setter = setIdx i
  }

setIdx :: Int -> a -> [a] -> [a]
setIdx idx new list
  | idx < 0 = error "negative index"
  | idx>= 0 && null list = error "index too large"
  | otherwise = case list of
      old:rest -> if idx == 0
                  then new : rest
                  else old : setIdx (idx-1) new rest

-- Что, если мы хотим изменить список?
-- modifyList :: (a -> a) -> [a] -> [a]
--
-- Либо копируем setIdx,
-- либо дважды обходим (getter, потом setter)
--
-- Неудачная идея LensTry0, поменяем

type LensTry1 s a = (a -> a) -> s -> (a, s)

toLensTry0 :: LensTry1 s a -> LensTry0 s a
toLensTry0 modifier = LensTry0
  { getter = fst . modifier id
  , setter = \new oldS -> snd $ modifier (const new) oldS }

ix1 :: Int -> LensTry1 [a] a
ix1 idx f list
  | idx < 0 = error "negative index"
  | idx>= 0 && null list = error "index too large"
  | otherwise = case list of
      old:rest
        | idx==0 -> (old, f old : rest)
        | otherwise -> let
            (oldA, newRest) = ix1 (idx-1) f rest
            in (oldA, old : newRest)

_fst1 :: LensTry1 (a,b) a
_fst1 f (a,b) = (a, (f a, b))

-- Плохо композируются: из (s -> (a,s)) не очень похоже
-- на (a -> a)

type LensTry2 f s a = (a -> f a) -> s -> f s

ix2 :: Functor f => Int -> LensTry2 f [a] a
ix2 idx f list
  | idx < 0 = error "negative index"
  | idx>= 0 && null list = error "index too large"
  | otherwise = case list of
      old:rest
        | idx==0 -> (: rest) <$> f old
        | otherwise -> (old :) <$> ix2 (idx-1) f rest

_fst2 :: Functor f => LensTry2 f (a,b) a
_fst2 f (a,b) = (\x -> (x,b)) <$> f a

-- Теперь можем эмулировать геттеры и сеттеры, используя
-- разные функторы:

setter2 :: LensTry2 Identity s a -> a -> s -> s
setter2 lens new oldStruct
  = runIdentity $ lens (Identity . const new) oldStruct

getter2 lens oldStruct
  = fst $ lens (\x -> (x,x)) oldStruct

-- Можем композировать линзы:
-- > setter2 (_fst2 . ix2 3) 'a' ("hello", 123213)
-- ("helao",123213)
-- > getter2 (_fst2 . ix2 2) ("hello", 123213)
-- 'l'

type LensTry3 f s t a b = (a -> f b) -> s -> f t

breakTableLegWithLenses :: [Room] -> [Room]
breakTableLegWithLenses = error "Implement!"

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
