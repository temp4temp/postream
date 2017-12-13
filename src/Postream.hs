{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Conduit
import Control.Concurrent.Async (concurrently)
import Data.Conduit.TMChan
import Data.Conduit.Network
import Data.Word8 (toUpper)
import Data.ByteString      (pack)

import System.Environment (getArgs)
import System.Exit (exitSuccess)


--lightloop :: AppData -> 


termC :: (Show i, Eq i, MonadIO m) => i -> Conduit i m i
termC t = do
  rs <- await 
  case rs of
    Nothing -> return ()
    (Just x) -> case x == t of
        True -> do
          liftIO $ exitSuccess
        False -> do
          liftIO $ print x
          yield x
          termC t


upper :: IO ()
upper = do 
  runTCPServer (serverSettings 4000 "*") (\ appData -> 
    appSource appData 
    $$ omapCE toUpper =$ appSink appData)

double :: IO ()
double = do
  runTCPServer (serverSettings 4001 "*") (\ appData -> 
    (appSource appData .| (termC "T\n") .| concatMapCE ( \ w -> pack [w,w])) $$ appSink appData)

telnet :: Int -> IO ()
telnet port = do
  runTCPClient (clientSettings port "localhost") $ \server -> 
    void $ concurrently
      (stdinC $$ appSink server)
      (appSource server $$ stdoutC)

class Monad m => MonadTerm m where 
  getLn :: m String
  putLn :: String -> m ()

main :: IO ()
main = do
  args <- getArgs
  start args 
  where 
    start ("u":_) = upper
    start ("d":_) = double
    start ("t":p:_) = telnet (read p)
    start _ = print "Usage postream [u|d|t]"
