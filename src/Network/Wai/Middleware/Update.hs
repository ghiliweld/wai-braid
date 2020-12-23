{-# LANGUAGE OverloadedStrings #-} -- this overloads String literals default to ByteString
module Network.Wai.Middleware.Update
    ( 
        Chan,
        Update,
        requestToUpdate,
    ) where

import qualified Data.ByteString   as B   (ByteString, empty, find, append, concat)
import Data.ByteString.Builder
import Data.Text            (Text)
import qualified Data.CaseInsensitive  as CI
import qualified Data.ByteString.Char8 as BC
import Data.Function (fix)
import Data.Foldable (for_)
import Control.Concurrent.Chan (Chan, newChan, dupChan, readChan)
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types     as H
import Network.Wai.Internal (Request(..), getRequestBodyChunk)

data Update = Update {
    updateTopic :: [Text],                  -- from request path
    updateHeaders :: H.RequestHeaders,      -- from request headers
    updatePatches :: IO B.ByteString          -- from request body
}

requestToUpdate :: Request -> Update
requestToUpdate req = Update {
    updateTopic = pathInfo req,
    updateHeaders = requestHeaders req,
    updatePatches = getRequestBodyChunk req
}