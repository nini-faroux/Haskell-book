{-# LANGUAGE OverloadedStrings #-}

module Errors (handler, handler') where

import           Data.Aeson                hiding (json)
import           Network.HTTP.Types.Status
import           Web.Spock

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Text
import           Data.Text.Encoding        (decodeUtf8)

handler :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
handler code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

handler' :: Status -> ActionCtxT ctx IO ()
handler' (Status code message) = handler code (decodeUtf8 message)
