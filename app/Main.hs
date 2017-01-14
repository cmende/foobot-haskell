module Main where

import           Data.List
import           Network
import           System.IO
import           Text.Printf

server = "irc.freenode.net"
port = 6667
channel = "##foob0t"
nick = "failb0t"

main :: IO ()
main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick ++ " 0 * :foob0t")
  write h "JOIN" channel
  listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = loop $ do
  t <- hGetLine h
  let s = init t
  if ping s then pong s else eval h (clean s)
  putStrLn s
  where
    loop a = a >> loop a
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval _ _ = return ()
