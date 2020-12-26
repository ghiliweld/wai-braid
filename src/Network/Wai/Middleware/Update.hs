{-# LANGUAGE OverloadedStrings #-} -- this overloads String literals default to ByteString

module Network.Wai.Middleware.Update
    (
        Update,
        Topic,
        requestToUpdate,
        updateToBuilder
    ) where

import qualified Data.ByteString        as B        (ByteString, append, intercalate)
import Network.Wai                                  (Request(..))
import qualified Data.ByteString.Lazy   as L        (ByteString, toStrict)
import Data.ByteString.Builder                      (Builder, byteString, string7)
import Data.Text                                    (Text)
import qualified Data.CaseInsensitive   as CI
import qualified Data.ByteString.Char8  as BC
import Control.Concurrent.Chan                      (Chan, newChan, dupChan, readChan)
import qualified Network.HTTP.Types     as H

type Topic = [Text]

data Update 
    -- | Updates are streamed from the server to subcribing client.
    -- On a PUT request, the headers and request body are put into an Update and streamed to subscribing clients.
    = Update {
    -- | The updateTopic is formed, from the request path
    updateTopic :: [Text],
    -- | The updateHeader are taken straight from the request headers
    updateHeaders :: H.RequestHeaders,
    -- | The updatePatches correspond to the request body
    updatePatches :: L.ByteString
}

-- | Forms an Update from a WAI Request
requestToUpdate :: Request -> L.ByteString -> Update
requestToUpdate req body = Update {
    updateTopic = pathInfo req,
    updateHeaders = requestHeaders req,
    updatePatches = body
}


separator :: B.ByteString
separator = BC.pack ": "

-- | Turns an Update (headers and patches) into a Builder to be streamed
-- Will return Nothing if the Topic we pass doesn't not match the updateTopic in the Update
-- Or returns Just builder, where builder has type Builder
updateToBuilder :: Topic -> Update -> Maybe Builder
updateToBuilder topic (Update t h p) 
    | t /= topic = Nothing
    | otherwise = Just builder
    where
        {-| 
            Forming:
                HEADERS \n
                \n
                BODY
            Then converting the ByteString to a Builder.
            Here's an example:
                Version: "ej4lhb9z78"                                 | HEADERS
                Parents: "oakwn5b8qh", "uc9zwhw7mf"                   |
                Content-Type: application/json                        |
                Merge-Type: sync9                                     |
                Content-Length: 64                                    |
                                                                      |
                [{"text": "Hi, everyone!",                            | | BODY
                "author": {"link": "/user/tommy"}}]                   | |
        -}
        updateToBuilder' :: H.RequestHeaders -> L.ByteString -> Builder
        updateToBuilder' hs b =
            byteString $ headers `B.append` "\n\n" `B.append` L.toStrict b
            where
                {-| 
                    Essentially, we are turning [(Header1, Value1), ..., (HeaderN, ValueN)]
                    to  "
                            Header1: Value1 \n
                            .
                            .
                            .
                            HeaderN: ValueN \n
                        "
                -}
                headers = B.intercalate "\n" $ map (\(h, v) -> CI.original h `B.append` separator `B.append` v) hs
        builder = updateToBuilder' h p