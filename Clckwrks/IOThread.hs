-- |this module provides a simple mechanism for adding IO operations
-- to a queue and running them in a single thread. This is useful if
-- the IO operations have side-effects which could collide if run from
-- multiple threads. For example, creating an image thumbnail and
-- storing it on disk, running LaTeX, etc.
module Clckwrks.IOThread where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.Chan (Chan,newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception
import Control.Monad (forever)

data IOThread a b = IOThread { ioThreadId :: ThreadId
                             , ioThreadChan :: (Chan (a, MVar (Either SomeException b)))
                             }

-- |start the IO thread.
startIOThread :: (a -> IO b) -- ^ the IO function that does all the work
              -> IO (IOThread a b) -- ^ a handle to the IOThread
startIOThread f =
    do c <- newChan
       tid <- forkIO $ ioThread f c
       return (IOThread tid c)
    where
      ioThread f c =
          forever $ do (a, mvar) <- readChan c
                       b <- try $ f a
                       putMVar mvar b

-- |kill the IOThread
-- 
-- WARNING: no attempt is made to wait for the queue to empty... we should probably have safer version that waits for the operations to complete?
killIOThread :: IOThread a b -> IO ()
killIOThread iot = killThread (ioThreadId iot)

-- |issue a request to the IO thread and get back the result
-- if the thread function throws an exception 'ioRequest' will rethrow the exception.
ioRequest :: (IOThread a b) -- ^ handle to the IOThread
          -> a -- ^ argument to the function in the IOThread
          -> IO b -- ^ value returned by the function in the IOThread
ioRequest iot a =
    do resp <- newEmptyMVar 
       writeChan (ioThreadChan iot) (a, resp)
       e <- readMVar resp
       case e of
         (Right r) ->  return r
         (Left err) -> throwIO err
