{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List

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


withdraw :: TBank -> String -> Int -> STM ()
withdraw bank name amount = do
  accounts <- readTVar bank
  let macctId = findIndex ((==name) . fst) accounts
  case macctId of
    Nothing -> pure ()
    Just i -> do
      let oldAmt = snd $ accounts!!i
      when (oldAmt >= amount) $ do
        let newAccs = take i accounts ++
                      [(name, oldAmt - amount)] ++
                      drop (i+1) accounts
        writeTVar bank newAccs

transfer :: TBank -> String -> String -> Int -> STM ()
transfer bank from to amt = do
  withdraw bank from amt
  addMoney bank to   amt

printBank :: String -> TBank -> IO ()
printBank prefix bank = do
  putStrLn prefix
  accs <- readTVarIO bank
  forM_ accs $ \(name, amt) -> do
    putStrLn $ name ++ "\t" ++ show amt

main :: IO ()
main = do
  bank <- newTVarIO []
  atomically $ addMoney bank "Alice" 1000
  atomically $ addMoney bank "Bob" 2000
  atomically $ addMoney bank "Alice" 100
  atomically $ addMoney bank "Bob" 200
  atomically $ addMoney bank "Charlie" 3000
  printBank "Initial" bank
  atomically $ withdraw bank "Dave" 100
  atomically $ withdraw bank "Charlie" 300
  atomically $ withdraw bank "Alice" 300000
  printBank "Withdrawals" bank
  atomically $ transfer bank "Bob" "Alice" 500
  printBank "Transfers" bank
