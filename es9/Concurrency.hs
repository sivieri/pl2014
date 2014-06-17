module Concurrency where

import Control.Concurrent
import Control.Concurrent.MVar

-- forkIO :: IO () -> IO ThreadId, lightweight thread
-- forkOS :: IO () -> IO ThreadId, OS thread
-- IO because communication happens through shared state

-- MVar monad: we put data into it, we modify it concurrently in a blocking or
-- non-blocking way

threadA valueToSendMVar valueReceivedMVar endMVar
    = do putMVar valueToSendMVar 72
         v <- takeMVar valueReceivedMVar -- blocking
         putStrLn (show v)
         putMVar endMVar "the end"

threadB valueToReceiveMVar valueToSendMVar
    = do z <- takeMVar valueToReceiveMVar -- blocking
         putMVar valueToSendMVar (10 * z)

main 
    = do aMVar <- newEmptyMVar
         bMVar <- newEmptyMVar
         endMVar <- newEmptyMVar
         forkOS (threadA aMVar bMVar endMVar)
         forkOS (threadB aMVar bMVar)
         c <- takeMVar endMVar -- blocking
         putStrLn ("ended with " ++ (show c))
