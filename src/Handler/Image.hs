{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Image where

import Import
import Network.HTTP.Simple
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

newtype ImagesResponse = ImagesResponse
                         { images :: [Image]
                         } deriving (Show, Generic)
instance ToJSON ImagesResponse
instance FromJSON ImagesResponse

data ImagePostData = ImagePostData
                        { label :: Maybe Text
                        , imageData :: Maybe Text
                        , imageUrl :: Maybe Text
                        , objectDetection :: Bool
                        } deriving (Show, Generic)
instance FromJSON ImagePostData


getImageR :: Handler Value
getImageR = do
    allImages <- runDB $ selectList [] []
    returnJson $ ImagesResponse { images = map entityVal allImages }

getImageLookupR :: ImageId -> Handler Value
getImageLookupR imageId = do
    image <- runDB $ get404 imageId
    returnJson image

postImageR :: Handler Value
postImageR = do
    postData <- (requireCheckJsonBody :: Handler ImagePostData)
    case parseData postData of
        Nothing -> invalidArgs ["image"]
        Just image -> do
            if objectDetection postData 
            then do
                case imageImageUrl image of
                    Nothing -> do
                        insertedImage <- runDB $ insertEntity image
                        returnJson insertedImage
                    Just imageUrl -> do
                        analysisRequest <- httpLBS $ buildImaggaRequest $ BC.pack $ T.unpack imageUrl
                        print $ getResponseBody analysisRequest
                        insertedImage <- runDB $ insertEntity image
                        returnJson insertedImage
            else do
                insertedImage <- runDB $ insertEntity image
                returnJson insertedImage


parseData :: ImagePostData -> Maybe Image
parseData postData
    | isValid       = Just $ Image url image myLabel Nothing
    | otherwise     = Nothing
    where
        isValid = hasData || hasUrl
        hasData = isJust $ image
        hasUrl = isJust $ url
        url = imageUrl postData
        image = imageData postData
        myLabel = fromMaybe "Default Label" $ label postData


imaggaToken :: BC.ByteString
imaggaToken = "REPLACE_ME"

imaggaHost :: BC.ByteString
imaggaHost = "https://api.imagga.com"

imaggaPath :: BC.ByteString
imaggaPath = "/v2/tags"

buildImaggaRequest :: BC.ByteString -> Request
buildImaggaRequest imageUrl = setQueryString queryParams
                              $ buildRequest imaggaToken imaggaHost "GET" imaggaPath
    where 
        queryParams = [("image_url" :: BC.ByteString, Just imageUrl)]

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestBearerAuth token
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ defaultRequest
