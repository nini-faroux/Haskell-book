{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
module Level02.Types
  ( Topic
  , CommentText
  , ContentType (..)
  , RqType (..)
  , Error (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

newtype Topic = Topic Text
  deriving Show

newtype CommentText = CommentText Text
  deriving Show

data RqType = 
    AddRq Topic CommentText 
  | ViewRq Topic 
  | ListRq 

data Error = 
    TopicNotFound 
  | EmptyTopic
  | EmptyComment
  | ContentTypeNotSupported
  | InvalidRequest
  deriving (Eq, Show)

data ContentType = 
    PlainText 
  | Json

renderContentType
  :: ContentType
  -> ByteString 
renderContentType content =
  case content of 
      PlainText -> "text/plain"
      Json      -> "application/json"

mkTopic
  :: Text
  -> Either Error Topic
mkTopic topic =
  case topic of 
      "" -> Left EmptyTopic 
      _  -> Right (Topic topic)

getTopic
  :: Topic
  -> Text
getTopic (Topic t) = t 

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText comment =
  case comment of 
      "" -> Left EmptyComment 
      _  -> Right (CommentText comment)

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) = t
