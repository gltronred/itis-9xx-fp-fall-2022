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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
