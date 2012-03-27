module Clckwrks.Markup.Markdown where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import           Control.Monad.Trans     (MonadIO(liftIO))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Text.HTML.SanitizeXSS   (sanitizeBalance)
import           System.Exit             (ExitCode(ExitFailure, ExitSuccess))
import           System.IO               (hClose, hGetContents)
import           System.Process          (waitForProcess, runInteractiveProcess)

-- | run the text through the 'markdown' executable and, if
-- successful, run the output through xss-sanitize / sanitizeBalance
-- to prevent injection attacks.
markdown :: (MonadIO m) =>
            Maybe [String] -- ^ override command-line flags
         -> Text -- ^ markdown text
         -> m (Either Text Text) -- ^ Left error, Right html
markdown mArgs txt = liftIO $
    do let args = case mArgs of
                    Nothing -> ["--html4tags"]
                    (Just a) -> a
       (inh, outh, errh, ph) <- runInteractiveProcess "markdown" args Nothing Nothing
       _ <- forkIO $ do T.hPutStr inh txt
                        hClose inh
       mvOut <- newEmptyMVar
       _ <- forkIO $ do c <- hGetContents outh
                        putMVar mvOut c
       mvErr <- newEmptyMVar
       _ <- forkIO $ do c <- T.hGetContents errh
                        putMVar mvErr c
       ec <- waitForProcess ph
       case ec of
         (ExitFailure _) ->
             do e <- readMVar mvErr
                return (Left e)
         ExitSuccess ->
             do m <- readMVar mvOut
                return (Right ({- sanitizeBalance -} (T.pack m)))
