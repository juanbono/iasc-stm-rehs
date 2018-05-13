------------------------------------------------------------
-- Server.
-- This modules declares a server that can accept connections
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs.Server where

import Control.Concurrent (forkFinally)
import Control.Monad      (forever, void)
import Network            (Socket, PortID(..), accept, listenOn, withSocketsDo)
import Text.Printf        (printf)
import System.IO          (Handle(..), BufferMode(..),
                           hGetLine, hClose, hPutStrLn, hSetNewlineMode, universalNewlineMode, hSetBuffering)

import Rehs (Table)
import Rehs.IO (newTableIO, updateAndReadSlotIO)
import Rehs.Commands (parseSlotTransactionLine)

startServer :: Int -> IO ()
startServer port = withSocketsDo $ do
    table <- newTableIO

    serverSocket <- listenServerPort port
    forever $ acceptClient serverSocket table

acceptClient :: Socket -> Table ->  IO ()
acceptClient serverSocket table = do
    (handle, host, port') <- accept serverSocket
    _ <- printf "[Server] Accepted connection from %s\n" host
    void $ forkFinally (handleClient handle table) (\_ -> closeClient handle host)

closeClient :: Handle -> String -> IO ()
closeClient handle host = do
    hClose handle
    void $ printf "[Server] Closed connection from %s\n" host

listenServerPort :: Int -> IO Socket
listenServerPort port = do
    socket <- listenOn (PortNumber (fromIntegral port))
    printf "[Server] Serving on port %d\n" port
    return socket

handleClient :: Handle -> Table -> IO ()
handleClient handle table = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    forever $ handleCommands handle table

handleCommands :: Handle -> Table -> IO ()
handleCommands handle table = do
    line <- hGetLine handle

    let transaction = parseSlotTransactionLine line

    updatedSlot <- updateAndReadSlotIO transaction table
    hPutStrLn handle $ show updatedSlot

