{-# LANGUAGE OverloadedStrings #-}

module Skypelog
  where
  
import Data.Text (Text)
  
data ConversationEntry = ConversationEntry
    { messageId :: Int
    , convoId   :: Int
    , author    :: Text
    , bodyXml   :: Text
    } deriving Show
    
data Conversation = Conversation
    { conversation :: [ConversationEntry] }
  deriving Show