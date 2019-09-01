{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status201, status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType(..), Error(..), 
                                           RqType(..), mkCommentText, mkTopic, 
                                           renderContentType, getTopic)

mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status content = responseLBS status [("Content-Type", renderContentType content)] 
  
resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200 

resp201 
    :: ContentType 
    -> LBS.ByteString 
    -> Response 
resp201 = mkResponse status201

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404 

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- Take raw request information and construct         
-- one of our types.                                                                
--------------------------------------------------------------------------------------

mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType 
mkAddRequest topic comment = 
  mkTopic topic >>= \t -> 
    mkCommentText (l2bs comment) >>= \c -> 
            return (AddRq t c)
  where 
    l2bs = decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic =
  case mkTopic topic of 
      Left e -> Left e 
      Right top -> Right (ViewRq top)

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse err 
  | err == TopicNotFound = resp404 PlainText "404"
  | otherwise            = resp400 PlainText "Empty Comment Text" 

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest request = do
  let method = requestMethod request
  let path = pathInfo request 
  body <- strictRequestBody request 
  case last path of      
      "add"  -> return $ mkAddRequest (last $ init path) body
      "view" -> return $ mkViewRequest (last $ init path) 
      _      -> return mkListRequest 

handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right (resp201 PlainText "Succesful post")
handleRequest (ViewRq topic) = 
        Right (resp200 PlainText "Succesful View") 
handleRequest ListRq = Right (resp200 PlainText "Successful View All")

app :: Application
app request cb = do
  reqType <- mkRequest request
  case reqType of 
      (Left e)    -> cb $ mkErrorResponse e 
      (Right req) -> 
          case handleRequest req of 
              (Left e)     -> cb $ mkErrorResponse e 
              (Right resp) -> cb resp 

runApp :: IO ()
runApp = run 3000 app
