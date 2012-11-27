module Clckwrks.GetOpts where

import Clckwrks.Monad
import System.Console.GetOpt -- (Permute, OptDescr, getOpt)
import System.Environment
import System.Exit


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
    , Option [] ["hide-port"]     (NoArg setHidePort)             "Do not show the port number in the URL"
    , Option [] ["hostname"]      (ReqArg setHostname "hostname") ("Server hostename, default: " ++ show (clckHostname def))
    , Option [] ["jquery-path"]   (ReqArg setJQueryPath   "path") ("path to jquery directory, default: " ++ show (clckJQueryPath def))
    , Option [] ["jqueryui-path"] (ReqArg setJQueryUIPath "path") ("path to jqueryui directory, default: " ++ show (clckJQueryUIPath def))
    , Option [] ["jstree-path"]   (ReqArg setJSTreePath   "path") ("path to jstree directory, default: " ++ show (clckJSTreePath def))
    , Option [] ["json2-path"]    (ReqArg setJSON2Path    "path") ("path to json2 directory, default: " ++ show (clckJSON2Path def))
    , Option [] ["top"]           (ReqArg setTopDir       "path") ("path to directory that holds the state directory, uploads, etc")
    , Option [] ["static"]        (ReqArg noop "ignored")         "unused"
    , Option [] ["logs"]          (ReqArg noop "ignored")         "unimplemented"
    , Option [] ["log-mode"]      (ReqArg noop "ignored")         "unimplemented"
    , Option [] ["enable-analytics"] (NoArg setAnalytics)         "enable google analytics tracking"
    ]
    where
      noop            _   = ModifyConfig $ id
      setPort         str = ModifyConfig $ \c -> c { clckPort         = read str }
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
parseArgs :: [OptDescr Flag] -> [String] -> IO (ClckwrksConfig -> ClckwrksConfig)
parseArgs opts args =
    do case getOpt Permute opts args of
         (flags,_,[]) ->
             if any isHelp flags
             then do putStr (helpMessage opts)
                     exitSuccess
             else do let f config = foldr (.) id [f | (ModifyConfig f) <- flags ] config
                     return f
         (_,_,errs)   ->
             do putStr ("Failure while parsing command line:\n"++unlines errs)
                putStr (helpMessage opts)
                exitFailure


-- | A simple usage message listing all flags possible.
helpMessage :: [OptDescr Flag] -> String
helpMessage opts =
    usageInfo header opts
    where
      header = "Usage: clckwrks [OPTION...]"

