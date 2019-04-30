{-# LANGUAGE OverloadedStrings #-}

module Errors (handler, handler') where

import           Web.Spock                 (ActionCtxT, json)
import           Data.Aeson                (Value(String), object, (.=))
import           Network.HTTP.Types.Status

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Text                 (Text)
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
