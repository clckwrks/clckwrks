module Clckwrks.Markup.Pandoc where

import           Clckwrks.Types                (Trust(..))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import           Control.Monad.Trans     (MonadIO(liftIO))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Text.HTML.SanitizeXSS   (sanitizeBalance)
import           System.Exit             (ExitCode(ExitFailure, ExitSuccess))
import           System.IO               (hClose)
import           System.Process          (waitForProcess, runInteractiveProcess)

-- | run the text through the 'markdown' executable. If successful,
-- and the input is marked 'Untrusted', run the output through
-- xss-sanitize / sanitizeBalance to prevent injection attacks.
pandoc :: (MonadIO m) =>
            Maybe [String]       -- ^ override command-line flags
         -> Trust                -- ^ do we trust the author
         -> Text                 -- ^ markdown text
         -> m (Either Text Text) -- ^ Left error, Right html
pandoc mArgs trust txt = liftIO $
    do let args = case mArgs of
                    Nothing -> []
                    (Just a) -> a
       (inh, outh, errh, ph) <- runInteractiveProcess "pandoc" args Nothing Nothing
       _ <- forkIO $ do T.hPutStr inh txt
                        hClose inh
       mvOut <- newEmptyMVar
       _ <- forkIO $ do c <- T.hGetContents outh
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
                return (Right ((if (trust == Untrusted) then sanitizeBalance else id) m))
