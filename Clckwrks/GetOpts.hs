{-# LANGUAGE RecordWildCards #-}
module Clckwrks.GetOpts where

import Control.Applicative   ((<$>))
import Clckwrks.Monad        (ClckwrksConfig(..), TLSSettings(..))
import Data.Maybe            (fromMaybe)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Directory      (doesFileExist)
import System.Exit           (exitFailure, exitSuccess)

------------------------------------------------------------------------------
-- Command line options
------------------------------------------------------------------------------

-- | command-line Flags
data Flag
    = ModifyConfig (ClckwrksConfig -> ClckwrksConfig)
    | Help
    | Version

-- | Flag selectors
isHelp, isVersion :: Flag -> Bool
isHelp    flag = case flag of Help    -> True; _ -> False
isVersion flag = case flag of Version -> True; _ -> False

-- | Command line options.
clckwrksOpts :: ClckwrksConfig -> [OptDescr Flag]
clckwrksOpts def =
    [ Option [] ["help"]          (NoArg Help)                    "Display this help message"
    , Option [] ["http-port"]     (ReqArg setPort "port")         ("Port to bind http server, default: " ++ show (clckPort def))
    , Option [] ["https-port"]    (ReqArg setTLSPort "port")      ("Port to bind https server, default:" ++ maybe "disabled." show (clckTLSPort <$> (clckTLS def)))
    , Option [] ["tls-cert"]      (ReqArg setTLSCert "path")      ("Path to tls .cert file. (required for https).")
    , Option [] ["tls-key"]       (ReqArg setTLSKey "path")       ("Path to tls .key file. (required for https).")
    , Option [] ["tls-ca"]        (ReqArg setTLSCA "path")        ("Path to tls .pem file. (required for some certs).")
    , Option [] ["tls-rev-proxy"] (NoArg setTLSRevProxy)          ("Act as a reverse proxy and trust X-Forwarded-Proto for TLS.")
    , Option [] ["hide-port"]     (NoArg setHidePort)             "Do not show the port number in the URL"
    , Option [] ["hostname"]      (ReqArg setHostname "hostname") ("Server hostname, default: " ++ show (clckHostname def))
    , Option [] ["jquery-path"]   (ReqArg setJQueryPath   "path") ("path to jquery directory, default: " ++ show (clckJQueryPath def))
    , Option [] ["jqueryui-path"] (ReqArg setJQueryUIPath "path") ("path to jqueryui directory, default: " ++ show (clckJQueryUIPath def))
    , Option [] ["jstree-path"]   (ReqArg setJSTreePath   "path") ("path to jstree directory, default: " ++ show (clckJSTreePath def))
    , Option [] ["json2-path"]    (ReqArg setJSON2Path    "path") ("path to json2 directory, default: " ++ show (clckJSON2Path def))
    , Option [] ["top"]           (ReqArg setTopDir       "path") ("path to directory that holds the state directory, uploads, etc")
    , Option [] ["enable-analytics"] (NoArg setAnalytics)         "enable google analytics tracking"
    ]
    where
      nullTLSSettings     = TLSSettings { clckTLSPort = 443
                                        , clckTLSCert = Nothing
                                        , clckTLSKey  = Nothing
                                        , clckTLSCA   = Nothing
                                        , clckTLSRev  = False
                                        }
      modifyTLS f cc =
          Just $ case clckTLS cc of
            Nothing -> f nullTLSSettings
            (Just tls) -> f tls
      noop            _   = ModifyConfig $ id
      setPort         str = ModifyConfig $ \c -> c { clckPort         = read str }
      setTLSPort      str = ModifyConfig $ \c -> c { clckTLS          = modifyTLS (\tls -> tls { clckTLSPort = read str }) c }
      setTLSCert      str = ModifyConfig $ \c -> c { clckTLS          = modifyTLS (\tls -> tls { clckTLSCert = Just str }) c }
      setTLSKey       str = ModifyConfig $ \c -> c { clckTLS          = modifyTLS (\tls -> tls { clckTLSKey = Just str }) c }
      setTLSCA        str = ModifyConfig $ \c -> c { clckTLS          = modifyTLS (\tls -> tls { clckTLSCA = Just str }) c }
      setTLSRevProxy      = ModifyConfig $ \c -> c { clckTLS          = modifyTLS (\tls -> tls { clckTLSRev = True}) c }
      setHostname     str = ModifyConfig $ \c -> c { clckHostname     = str      }
      setHidePort         = ModifyConfig $ \c -> c { clckHidePort     = True     }
      setJQueryPath   str = ModifyConfig $ \c -> c { clckJQueryPath   = str      }
      setJQueryUIPath str = ModifyConfig $ \c -> c { clckJQueryUIPath = str      }
      setJSTreePath   str = ModifyConfig $ \c -> c { clckJSTreePath   = str      }
      setJSON2Path    str = ModifyConfig $ \c -> c { clckJSON2Path    = str      }
      setTopDir       str = ModifyConfig $ \c -> c { clckTopDir       = Just str }
      setAnalytics        = ModifyConfig $ \c -> c { clckEnableAnalytics = True  }

-- | Parse the command line arguments into a list of flags. Exits with usage
-- message, in case of failure.
parseArgs :: [OptDescr Flag] -> [String] -> IO (ClckwrksConfig -> IO ClckwrksConfig)
parseArgs opts args =
    do case getOpt Permute opts args of
         (flags,_,[]) ->
             if any isHelp flags
             then do putStr (helpMessage opts)
                     exitSuccess
             else do let f config = checkTLS $ foldr (.) id [f | (ModifyConfig f) <- flags ] config
                     return f
         (_,_,errs)   ->
             do putStr ("Failure while parsing command line:\n"++unlines errs)
                putStr (helpMessage opts)
                exitFailure
    where
      checkTLS cc =
          case clckTLS cc of
            Nothing -> return cc
            (Just TLSSettings{..}) ->
                do mCertError <-
                       case clckTLSCert of
                          Nothing ->
                            if clckTLSRev
                              then return Nothing
                              else return (Just "--tls-cert not set.")
                          (Just certPath) -> do
                            b <- doesFileExist certPath
                            if b
                              then return Nothing
                              else return (Just $ "Can not find the file " ++ certPath)
                   mKeyError <-
                       case clckTLSKey of
                          Nothing ->
                            if clckTLSRev
                              then return Nothing
                              else return (Just "--tls-key not set.")
                          (Just keyPath) -> do
                            b <- doesFileExist keyPath
                            if b
                              then return Nothing
                              else return (Just $ "Can not find the file " ++ keyPath)
                   case (mCertError, mKeyError) of
                     (Nothing, Nothing) -> return cc
                     _ -> do putStrLn "It seems you are trying to enable https support. To do that you need to use both the --tls-cert and --tls-key flags."
                             putStrLn $ unlines [fromMaybe "" mCertError, fromMaybe "" mKeyError]
                             putStrLn $ helpMessage opts
                             exitFailure

-- | A simple usage message listing all flags possible.
helpMessage :: [OptDescr Flag] -> String
helpMessage opts =
    usageInfo header opts
    where
      header = "Usage: clckwrks [OPTION...]"

