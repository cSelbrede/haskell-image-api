{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ImageSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do
    describe "valid request" $ do
        it "gives a 200" $ do
            get ImageR

            statusIs 200

        it "returns image json" $ do
            get ImageR

            let message = [] :: [Image]
                body = object [ "images" .= message ]

            images <- requireJSONResponse
            assertEq "empty images array" images body

        it "accepts posted image urls" $ do
            let imageUrl = "https://example.com/test.png" :: Text
                body = object ["imageUrl" .= imageUrl
                              , "objectDetection" .= False]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl ImageR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200

            images <- runDB $ selectList [] []
            Entity _id image <-
                case images of
                    [ent] -> pure ent
                    _ -> error "needed 1 entity"
            assertEq "Should have " image (Image (Just imageUrl) Nothing "Default Label" Nothing)

        it "accepts posted image data" $ do
            let imageData = "0101011101101" :: Text
                body = object [ "imageData" .= imageData
                              , "objectDetection" .= False]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl ImageR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200

            images <- runDB $ selectList [] []
            Entity _id image <-
                case images of
                    [ent] -> pure ent
                    _ -> error "needed 1 entity"
            assertEq "Should have " image (Image Nothing (Just imageData) "Default Label" Nothing)

        it "gives 400 when missing data from client" $ do
            let body = object []
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl ImageR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400
