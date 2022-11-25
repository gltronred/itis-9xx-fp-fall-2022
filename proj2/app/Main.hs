{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List
import System.Random

-- Банк: список счетов (пар "фамилия"-"количество денег")
type TBank = TVar [(String, Int)]

addMoney :: TBank -> String -> Int -> STM ()
addMoney bank name amount = do
  accounts <- readTVar bank
  let newAccs = case findIndex ((==name) . fst) accounts of
        Nothing -> (name, amount) : accounts
        Just i -> take i accounts ++
                  [(name, snd (accounts!!i) + amount)] ++
                  drop (i+1) accounts
  writeTVar bank newAccs


withdraw :: TBank -> String -> Int -> STM Bool
withdraw bank name amount = do
  accounts <- readTVar bank
  let macctId = findIndex ((==name) . fst) accounts
  case macctId of
    Nothing -> pure False
    Just i -> do
      let oldAmt = snd $ accounts!!i
      if oldAmt >= amount
        then do
          let newAccs = take i accounts ++
                        [(name, oldAmt - amount)] ++
                        drop (i+1) accounts
          writeTVar bank newAccs
          pure True
        else pure False

transfer :: TBank -> String -> String -> Int -> STM ()
transfer bank from to amt = do
  b <- withdraw bank from amt
  when b $ addMoney bank to amt

printBank :: String -> TBank -> IO ()
printBank prefix bank = do
  putStrLn prefix
  accs <- readTVarIO bank
  forM_ accs $ \(name, amt) -> do
    putStrLn $ name ++ "\t" ++ show amt

randomTransfers :: TBank -> [String] -> Int -> IO ()
randomTransfers bank accounts count = replicateM_ count $ do
  let n = length accounts
  f <- randomRIO (0,n-1)
  t <- randomRIO (0,n-1)
  let from = accounts !! f
      to = accounts !! t
  amt <- randomRIO (0,1000)
  -- putStrLn $ from ++ "\t->\t" ++ to ++ ":\t" ++ show amt
  -- printBank "" bank
  atomically $ transfer bank from to amt

main :: IO ()
main = do
  bank <- newTVarIO []

  let actions =
        [ ("Alice", 1000)
        , ("Bob", 2000)
        , ("Alice", 100)
        , ("Bob", 200)
        , ("Charlie", 3000)
        ]
  forConcurrently_ actions $ \(who,amt) -> do
    atomically $ addMoney bank who amt
    printBank ("=== " ++ who ++ " " ++ show amt ++ "==") bank
  printBank "Initial" bank

  atomically $ withdraw bank "Dave" 100
  atomically $ withdraw bank "Charlie" 300
  atomically $ withdraw bank "Alice" 300000
  printBank "Withdrawals" bank

  atomically $ transfer bank "Bob" "Alice" 500
  printBank "Transfers" bank

  accounts <- map fst <$> readTVarIO bank
  oldMoney <- sum . map snd <$> readTVarIO bank

  asyncs <- forM [1..100] $ \_ -> async $
    randomTransfers bank accounts 10000

  foldM (\() a -> wait a) () asyncs

  newMoney <- sum . map snd <$> readTVarIO bank
  printBank "Random" bank
  print (oldMoney, newMoney)
