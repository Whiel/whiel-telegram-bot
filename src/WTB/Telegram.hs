{-# LANGUAGE OverloadedStrings #-}
module WTB.Telegram (newBot,
                     botInfo,
                     botSend,
                     botUpdateLoop,
                     botDrop,
                     
                     Token,
                     Bot (..),
                     Message (..),
                     Chat(..)) where

import Data.Text
import Data.Maybe
import Data.Either.Unwrap
import Network.HTTP.Client      (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Web.Telegram.API.Bot

data Bot = Bot {
  token :: Token
  ,manager :: Manager
  ,lastId :: Int
  }

largest :: (Ord a) => [a] -> Maybe a
largest [] = Nothing
largest x  = Just (Prelude.maximum x)

newBot :: Text -> IO Bot
newBot token = do
  let bot_token = Token token
  manager <- newManager tlsManagerSettings
  return (Bot bot_token manager 0)


botInfo bot = do
  getMe (token bot) (manager bot)

dispatchSingleUpdate :: Update -> Bot -> (Bot -> Message -> IO Bool) -> IO (Maybe(Bool,Int))
dispatchSingleUpdate update bot f
  | isNothing(message update) = return (Nothing)
  | otherwise = do
      let msg = (fromJust . message) update
      continue <- f bot msg
      return $ Just (continue, update_id update)
      
  
dispatchUpdates' :: [Update] -> Int -> Bot -> (Bot -> Message -> IO Bool) -> IO (Bool,Int)
dispatchUpdates' [] maxId _ _ = return (True,maxId)
dispatchUpdates' (x:xs) maxId bot f = do
  ret <- dispatchSingleUpdate x bot f
  case ret of
    Just (False,lastId) -> return (False, max lastId maxId)
    Just (True,lastId) -> dispatchUpdates' xs (max lastId maxId) bot f
    Nothing -> return (True, maxId)

dispatchUpdates (Response r _) b f  = dispatchUpdates' r (-1) b f

botUpdateLoop :: Bot -> (Bot -> Message -> IO Bool) -> IO Bot
botUpdateLoop bot handler = do
  updates  <- (getUpdates
                (token bot)
                ((Just . lastId) bot) (Just 100) (Just 10) 
                (manager bot))
  (continue,maxId) <- dispatchUpdates (fromRight updates) bot handler
  let nextBot = bot { lastId = (max (lastId bot) (1+maxId)) }
  if continue then (botUpdateLoop nextBot handler) else return (nextBot)


botDrop :: Bot -> Int -> IO Bot
botDrop bot time = do
  updates  <- (getUpdates
                (token bot)
                ((Just . lastId) bot) (Just 100) (Just time) 
                (manager bot))
  let Response us _ = fromRight updates
  let maxId = maybe 0 id $ largest $ Prelude.map update_id us
  let nextBot = bot { lastId = (max (lastId bot) (1+maxId)) }
  if (Prelude.length us) == 100 then (botDrop nextBot time) else return (nextBot)

  

botSend bot chatId text = do
  let sendreq = sendMessageRequest (pack $ show chatId) text
  sendMessage (token bot) sendreq (manager bot) 
  
