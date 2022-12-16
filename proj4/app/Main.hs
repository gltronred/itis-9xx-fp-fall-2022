module Main where

data Room = Room
  { table :: Maybe Table
  , persons :: [Person]
  }
  deriving (Eq,Show,Read)

data Table = Table
  { legs :: Int
  , colour :: Colour
  }
  deriving (Eq,Show,Read)

data Colour = White | Red | Green | Blue | Black
  deriving (Eq,Show,Read)

data Person = Person
  { surname :: String
  , name :: String
  , patronym :: Maybe String
  }
  deriving (Eq,Show,Read)

rooms :: [Room]
rooms = [ Room{ table=Just $ Table{ legs=3, colour=White }
              , persons=
                [ Person{ surname="A"
                        , name="B"
                        , patronym=Just "C"}
                , Person{ surname="D"
                        , name="E"
                        , patronym=Nothing}]}
        , Room{ table=Nothing
              , persons=
                [ Person{ surname="G"
                        , name="H"
                        , patronym=Just "I"} ]}]

-- Сломать ножку у каждого стола в каждой комнате
--
-- Пример неудобной работы с глубоко вложенными
-- структурами
breakTableLeg :: [Room] -> [Room]
breakTableLeg rooms =
  [ r{ table = case table r of
          Nothing -> Nothing
          Just t -> Just t{ legs = legs t - 1 }
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
    breakTableInRoom r = r{table = breakTable<$>table r}
    breakTable :: Table -> Table
    breakTable t = t{ legs = legs t - 1 }

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


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
