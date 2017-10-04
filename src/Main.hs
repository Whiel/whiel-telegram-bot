{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Text

import WTB.Config
import WTB.Telegram                

unwrap :: Show a => Either a b -> IO (Maybe b)
unwrap (Left e) = do print e ; return (Nothing)
unwrap (Right x) = return ( Just x )

craftResponse "/salute" = (Just "I am the λ and the Type, I am the data that is the instruction, I am The Machine.",True)
craftResponse "/die" = (Just "I shall return.", False)
craftResponse text = (Just "I am also pretty naïve and cannot do anything.",True)

updateHandler :: Bot -> Message -> IO Bool
updateHandler bot message = do
  case text message of
    Just text -> do
      let chatId = (chat_id . chat) message
      print $ (show chatId) ++ " : " ++ (unpack text)
      let (reply,cont) = craftResponse text
      if (isJust reply) then
        (do
            botSend bot chatId (fromJust reply)
            return (cont))
        else
        return (cont)        
      return (cont)        
    Nothing -> return(True)

main :: IO ()
main = do
  config_e <- read_config "cfg/config.json"
  case config_e of
    Right config ->
      do
        let token = bot_token config
        bot <- newBot $ pack token
        bot' <- botDrop bot 2
        e_info <- botInfo bot'
        info <- unwrap e_info
        print info
        x <- botUpdateLoop bot' updateHandler
        return ()
    Left error ->
      do
        putStrLn "Error parsing configuration:"
        putStrLn error
  
