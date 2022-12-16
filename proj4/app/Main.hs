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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
