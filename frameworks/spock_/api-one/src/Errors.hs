{-# LANGUAGE OverloadedStrings #-}

module Errors where

import           Web.Spock
import           Data.Aeson                hiding (json)
import           Network.HTTP.Types.Status

import           Data.Text
import           Data.Text.Encoding        (decodeUtf8)
import           Control.Monad.IO.Class    (MonadIO)

import           ApiTypes

handler :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
handler code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

handler' :: Status -> ActionCtxT ctx IO ()
handler' (Status code message) = handler code (decodeUtf8 message)
