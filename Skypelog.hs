{-# LANGUAGE OverloadedStrings #-}

module Skypelog
  where
  
import Data.Text (Text, pack, intercalate, empty, replace)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Applicative ((<$>), (<*>))
import Data.List (groupBy, nub, sort)
import Data.Map (fromListWith, Map (..), toList)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import qualified Data.Text.IO as TIO (putStrLn, writeFile)
import Text.Blaze.Html.Renderer.Text (renderHtml )
import Text.Blaze.Html (toHtml)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import System.Locale 
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as B8 (putStrLn, writeFile)
  
data ConversationEntry = ConversationEntry
    { messageId :: Int
    , convoID   :: Int
    , author    :: Text
    , bodyXml   :: Maybe ConversationMsg
    , timestamp :: Int
    } deriving (Show, Eq)
    
instance FromRow ConversationEntry where
    fromRow = ConversationEntry <$> field <*> field <*> field <*> field <*> field 
   
instance Ord ConversationEntry where
    compare x y 
        | timestamp x == timestamp y = EQ
        | timestamp x <= timestamp y = LT
        | otherwise                  = GT
  
type ConversationMsg = Text  
  
main :: IO ()
main = do
   conn <- open "main.db"
   r <- query_ conn "SELECT id, convo_id, author, body_xml, timestamp FROM messages" :: IO [ConversationEntry]
   let r' = encodeUtf8 . intercalate "\n=========================\n" . map pretyConvo $ groupConvos r
   B8.writeFile "output.txt" r'
   B8.putStrLn r'
   close conn
   
groupConvos :: [ConversationEntry] ->  [(Int, [ConversationEntry])]
groupConvos ces = toList . fromListWith (++) $ map (\ce -> (convoID ce, [ce])) ces

pretyConvo :: (Int, [ConversationEntry]) -> Text
pretyConvo (id, ces') = "Conversation ID: " 
                    <> (pack $ show id) 
                    <> "\n"
                    <> pretyAuthors ces
                    <> "\n"
                    <> (intercalate "\n" $ map pretyMsg ces)
                    <> "\n\n"
  where ces = sort ces'
                    
pretyMsg :: ConversationEntry -> Text
pretyMsg ce = case bodyXml ce of 
    Nothing -> empty
    Just cm -> author ce <> " (" <> (prettyTime $ timestamp ce) <> "): " <> decodeXMLentities cm
    
pretyAuthors :: [ConversationEntry] -> Text
pretyAuthors ces = "Conversation with: " <> (intercalate ", " . nub $ map author ces)

prettyTime :: Int -> Text
prettyTime n = pack . formatTime defaultTimeLocale "%FT%T" . posixSecondsToUTCTime $ fromIntegral n

decodeXMLentities :: Text -> Text
decodeXMLentities x = Prelude.foldr (uncurry Data.Text.replace) x $ Prelude.zip ["&quot;", "&amp;", "&apos;", "&lt;", "&gt;"] ["\"", "&", "'", "<", ">"]
