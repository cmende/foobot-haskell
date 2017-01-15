module Main where

import           Control.Exception.Base
import           Control.Monad.Reader
import           Data.List
import           Network
import           System.IO
import           Text.Printf

server = "irc.freenode.net"
port = 6667
channel = "##foob0t"
nick = "failb0t"

data Bot = Bot { socket :: Handle }

type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop       = runReaderT run

connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify = bracket_
      (printf "Connecting to %s..." server >> hFlush stdout)
      (putStrLn "done.")

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :foob0t")
    write "JOIN" channel
    asks socket >>= listen

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf    "> %s %s\n" s t

listen :: Handle -> Net ()
listen h = loop $ do
  s <- fmap init (io (hGetLine h))
  io (putStrLn s)
  if ping s then pong s else eval (clean s)
  where
    loop a = a >> loop a
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval x
  | "!" `isPrefixOf` x            = runCmd (drop 1 x)
  | (nick ++ ": ") `isPrefixOf` x = runCmd (drop (length nick + 2) x)
  | otherwise                     = return ()

runCmd :: String -> Net ()
runCmd _ = privmsg channel "test"

privmsg :: String -> String -> Net ()
privmsg t s = write "PRIVMSG" (t ++ " :" ++ s)

io :: IO a -> Net a
io = liftIO
