{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

fun :: TVar Int -> Int -> IO ()
fun sv i = do
  atomically $ modifyTVar' sv (+i)

main :: IO ()
main = do
  sv <- newTVarIO 0
  forM_ [1..5] $ \i -> forkIO $ fun sv i
  threadDelay 1000000
  s <- atomically $ readTVar sv
  print s
