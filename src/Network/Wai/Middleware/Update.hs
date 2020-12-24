{-# LANGUAGE OverloadedStrings #-} -- this overloads String literals default to ByteString
{-# LANGUAGE LambdaCase #-}
module Network.Wai.Middleware.Update
    ( 
        Chan,
        Update,
        Topic,
        requestToUpdate,
        updateToBuilder
    ) where

import qualified Data.ByteString   as B   (ByteString, empty, find, append, concat, intercalate)
import Network.Wai (Response(..), Request(..), StreamingBody, getRequestBodyChunk)
import qualified Data.ByteString.Lazy  as L
import Data.ByteString.Builder (Builder, byteString, string7)
import Data.Text            (Text)
import qualified Data.CaseInsensitive  as CI
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.Foldable (for_)
import Control.Concurrent.Chan (Chan, newChan, dupChan, readChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import qualified Network.HTTP.Types     as H
import qualified Data.Conduit.List as CL

type Topic = [Text]

data Update = Update {
    updateTopic :: [Text],                  -- from request path
    updateHeaders :: H.RequestHeaders,      -- from request headers
    updatePatches :: L.ByteString          -- from request body
}

requestToUpdate :: Request -> L.ByteString -> Update
requestToUpdate req body = Update {
    updateTopic = pathInfo req,
    updateHeaders = requestHeaders req,
    updatePatches = body
}


separator :: B.ByteString
separator = BC.pack ": "

updateToBuilder :: Topic -> Update -> Maybe Builder
updateToBuilder topic update = 
    if updateTopic update == topic
        then Just builder
        else Nothing
    where
        updateToBuilder' :: Update -> Builder
        updateToBuilder' update =
            byteString $ headers `B.append` "\n" `B.append` L.toStrict body
            where
                headers = B.intercalate "\n" $ map (\(h, v) -> CI.original h `B.append` separator `B.append` v) (updateHeaders update)
                body = updatePatches update
        builder = updateToBuilder' update