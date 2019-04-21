{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Applicative            ((<*>))
import Control.Exception              (Exception, throwIO)
import Control.Monad                  (forever, mapM_, return)
import Data.ByteString                (ByteString, concat)
import Data.Eq                        (Eq)
import Data.Function                  (($))
import Data.Functor                   ((<$>))
import Data.List                      (head, map, (++))
import Data.Maybe                     (Maybe (Just, Nothing))
import Data.Text                      (Text, intercalate, strip)
import Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import Data.Typeable                  (Typeable)
import Database.SQLite.Simple.FromRow (FromRow (fromRow))
import Database.SQLite.Simple.ToRow   (ToRow (toRow))
import Database.SQLite.Simple.Types
import GHC.Integer                    (Integer)
import System.IO                      (IO, print, putStrLn)
import Text.RawString.QQ              (r)
import Text.Show                      (Show, show)

import Database.SQLite.Simple    as DB
import Network.Socket            as Sock
import Network.Socket.ByteString as SockBS

type UserName      = Text
type Shell         = Text
type HomeDirectory = Text
type RealName      = Text
type Phone         = Text

type UserRow = ( Null
               , UserName
               , Shell
               , HomeDirectory
               , RealName
               , Phone
               )

data User = User
  { userId        :: Integer
  , userName      :: UserName
  , shell         :: Shell
  , homeDirectory :: HomeDirectory
  , realName      :: RealName
  , phone         :: Phone
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow User{..} = toRow ( userId
                         , userName
                         , shell
                         , homeDirectory
                         , realName
                         , phone
                         )

data DuplicateDataEx = DuplicateDataEx
                       deriving (Eq, Show, Typeable)

instance Exception DuplicateDataEx

createUsersTable :: Query
createUsersTable = [r|
  CREATE TABLE IF NOT EXISTS users
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , username TEXT UNIQUE
  , shell TEXT
  , homedirectory TEXT
  , realname TEXT
  , phone TEXT
  )
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsersQuery :: Query
allUsersQuery = "SELECT * FROM users"

getUserByUserNameQuery :: Query
getUserByUserNameQuery = "SELECT * FROM users WHERE username = ?"

getUser :: Connection -> UserName -> IO (Maybe User)
getUser conn un = do
  results <- query conn getUserByUserNameQuery (Only un)
  case results of
    []                -> return Nothing
    [user]            -> return $ Just user
    _                 -> throwIO DuplicateDataEx

createDatabase      :: IO ()
createDatabase  = do
  conn <- open "fingerd.db"
  execute_ conn createUsersTable
  execute conn insertUserQuery userRow
  rows <- query_ conn allUsersQuery
  mapM_ print (rows :: [User])
  DB.close conn
  where userRow     :: UserRow
        userRow = ( Null
                  , "macalimlim"
                  , "/run/current-system/sw/bin/zsh"
                  , "/home/macalimlim"
                  , "Michael Angelo Calimlim"
                  , "1234567890"
                  )

returnUsers :: Connection -> Socket -> IO ()
returnUsers conn sock  = do
  rows <- query_ conn allUsersQuery
  let userNames        = map userName rows
      newlineSeparated = intercalate "\n" userNames
  sendAll sock $ encodeUtf8 newlineSeparated

formatUser :: User -> ByteString
formatUser User{..} = concat [ "Login: ", encodeUtf8 userName, "\t\t\t\t"
                             , "Name: ", encodeUtf8 realName, "\n"
                             , "Directory: ", encodeUtf8 homeDirectory, "\t\t\t"
                             , "Shell: ", encodeUtf8 shell, "\n"
                             ]

returnUser :: Connection -> Socket -> UserName -> IO ()
returnUser conn sock un = do
  maybeUser <- getUser conn $ strip un
  case maybeUser of
    Nothing              -> putStrLn ("Cannot find user: " ++ show un)
    Just user            -> sendAll sock $ formatUser user

handleQuery :: Connection -> Socket -> IO ()
handleQuery conn sock = do
  msg <- SockBS.recv sock 1024
  case msg of
    "\r\n"                -> returnUsers conn sock
    name                  -> returnUser conn sock $ decodeUtf8 name

handleQueries :: Connection -> Socket -> IO ()
handleQueries conn sock = forever $ do
  (s, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery conn s
  Sock.close s

main :: IO ()
main                                                      = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "79")
  let serverAddr                                          = head addrInfos
  sock      <- socket (addrFamily serverAddr) Stream defaultProtocol
  Sock.bind sock (addrAddress serverAddr)
  listen sock 1
  conn      <- open "fingerd.db"
  handleQueries conn sock
  DB.close conn
  Sock.close sock

{-

Chapter Exercises

1. Try using the sqlite3 command line interface to add a new user
or modify an existing user in finger.db.

2. Write an executable separate of fingerd and debug which allows
you to add new users to the database.

3. Add the ability to modify an existing user in the database.

4. Bound on a different port, try creating a “control socket” that
permits inserting new data into the database while the server is
running. This will probably require, at minimum, learning how
to use forkIO and the basics of concurrency in Haskell among
other things. Design the format for representing the user rows
passed over the TCP socket yourself. For bonus points, write
your own client executable that takes the arguments from the
command line as well.

5. Celebrate completing this massive book.

-}
