-- |this module provides a simple mechanism for adding IO operations
-- to a queue and running them in a single thread. This is useful if
-- the IO operations have side-effects which could collide if run from
-- multiple threads. For example, creating an image thumbnail and
-- storing it on disk, running latex, etc.
module Clckwrks.IOThread where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Chan (Chan,newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception
import Control.Monad (forever)

newtype IOThread a b = IOThread (Chan (a, MVar (Either SomeException b)))

-- |start the IO thread.
startIOThread :: (a -> IO b) -- ^ the IO function that does all the work
              -> IO (ThreadId, IOThread a b) -- ^ a ThreadId which can be used to kill the IOThread, and a handle that can be used to issue requests to the thread.
startIOThread f =
    do c <- newChan
       tid <- forkIO $ ioThread f c
       return (tid, IOThread c)
    where
      ioThread f c =
          forever $ do (a, mvar) <- readChan c
                       b <- try $ f a
                       putMVar mvar b

-- |issue a request to the IO thread and get back the result
-- if the thread function throws an exception 'ioRequest' will rethrow the exception.
ioRequest :: (IOThread a b) -- ^ handle to the IOThread
          -> a -- ^ argument to the function in the IOThread
          -> IO b -- ^ value returned by the function in the IOThread
ioRequest (IOThread chan) a =
    do resp <- newEmptyMVar 
       writeChan chan (a, resp)
       e <- readMVar resp
       case e of
         (Right r) ->  return r
         (Left err) -> throwIO err
            
