{-# LANGUAGE OverloadedStrings #-} -- this overloads String literals default to ByteString

module Network.Wai.Middleware.Braid
    ( 
        -- * Middleware
        braidify,
        subscriptionMiddleware,
        versionMiddleware,
        addMergeTypeHeader, addPatchHeader,
        -- * Subscription helper
        sendUpdate,
        -- * Types
        Update, Topic,
        -- * Method helpers
        isGetRequest, isPutRequest, isPatchRequest,
        -- * 209 Status variable
        status209,
        -- * Header helpers & variables
        hSub, hVer, hPatch,
        lookupHeader,
        getSubscription, hasSubscription, getSubscriptionKeepAliveTime, addSubscriptionHeader,
        getVersion, hasVersion, addVersionHeader,
        getPatches, hasPatches

    ) where

import Network.HTTP.Types.Method   (Method, methodGet, methodPut, methodPatch)
import Network.HTTP.Types.Header   (Header, HeaderName, RequestHeaders, ResponseHeaders)
import Network.HTTP.Types.Status   (Status, mkStatus)
import Network.Wai          (Middleware, responseStream, modifyResponse, ifRequest, mapResponseHeaders, mapResponseStatus, strictRequestBody)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.EventSource (ServerEvent, eventData)
import Network.Wai.Internal (Response(..), Request(..), getRequestBodyChunk)
import Control.Concurrent.Chan (Chan, dupChan, readChan, writeChan)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString      (ByteString, breakSubstring)
import Data.Function (fix)
import Data.Maybe (isJust)
import qualified Data.CaseInsensitive  as CI
import qualified Data.ByteString.Char8 as BC

import Network.Wai.Middleware.Update (Update, Topic, requestToUpdate, updateToBuilder)

isGetRequest, isPutRequest, isPatchRequest :: Request -> Bool
isGetRequest req = requestMethod req == methodGet
isPutRequest req = requestMethod req == methodPut
isPatchRequest req = requestMethod req == methodPatch

-- new status code for subscriptions in braid
status209 :: Status
status209 = mkStatus 209 "Subscription"

lookupHeader :: HeaderName -> [Header] -> Maybe ByteString
lookupHeader _ [] = Nothing
lookupHeader v ((n, s):t)  
    |  v == n   =  Just s
    | otherwise =  lookupHeader v t

hSub :: HeaderName
hSub = "Subscribe"

getSubscription :: Request -> Maybe ByteString
getSubscription req = lookupHeader hSub $ requestHeaders req

getSubscriptionKeepAliveTime :: Request -> ByteString
getSubscriptionKeepAliveTime req =
    let Just str = lookupHeader hSub $ requestHeaders req 
    in snd $ breakSubstring "=" str

hasSubscription :: Request -> Bool
hasSubscription req = isJust $ getSubscription req

addSubscriptionHeader :: ByteString -> Response -> Response
addSubscriptionHeader s = mapResponseHeaders (\hs -> (hSub, s) : ("Cache-Control", "no-cache, no-transform") : hs)


-- TODO: still needs mechanism to keep alive, i.e. keeping the response connection open
subscriptionMiddleware :: Chan Update -> Middleware
subscriptionMiddleware src = catchUpdate src . modifyHeadersToSub . modifyStatusTo209
    where
        modifyHeadersToSub :: Middleware
        modifyHeadersToSub app req respond = 
            case getSubscription req of
                Just v -> app req $ respond . addSubscriptionHeader v
                Nothing -> app req respond
        modifyStatusTo209 :: Middleware
        modifyStatusTo209 = ifRequest hasSubscription $ modifyResponse $ mapResponseStatus (const status209)
        -- NOTE: we're consuming the full request body, maybe there's a better way of doing this? idk
        catchUpdate :: Chan Update -> Middleware
        catchUpdate src = ifRequest isPutRequest 
            $ \app req res -> do
                src' <- liftIO $ dupChan src
                strictRequestBody req >>= \b ->  
                    writeChan src' $ requestToUpdate req b 
                >> app req res

hVer :: HeaderName   
hVer = "Version"

getVersion :: Request -> Maybe ByteString
getVersion req = lookupHeader hVer $ requestHeaders req

hasVersion :: Request -> Bool
hasVersion req = isJust $ getVersion req

addVersionHeader :: ByteString -> Response -> Response
addVersionHeader s = mapResponseHeaders (\hs -> (hVer, s) : hs)

versionMiddleware :: Middleware
versionMiddleware app req respond = 
    case (getVersion req, isGetRequest req) of
        (Just v, True) -> app req $ respond . addVersionHeader v
        _              -> app req respond

addMergeTypeHeader :: Middleware
addMergeTypeHeader = ifRequest isGetRequest $ addHeaders [("Merge-Type", "sync9")]

addPatchHeader :: Middleware
addPatchHeader = ifRequest isPutRequest $ addHeaders [("Patches", "OK")]

hPatch :: HeaderName
hPatch = "Patches"

getPatches :: Request -> Maybe ByteString
getPatches req = lookupHeader hPatch $ requestHeaders req

hasPatches :: Request -> Bool
hasPatches req = isJust $ getPatches req

{-|
    braidify
    ---------
    braidify acts as a wai middleware, it :
    1. adds ('Range-Request-Allow-Methods', 'PATCH, PUT'), ('Range-Request-Allow-Units', 'json'), ("Patches", "OK") headers to response
    2. adds Patches OK header to response if the request is a PUT request
    3. adds Merge-Type: sync9 header to response by default is request is a GET request
    4. adds Version header to the response if request has a Version header and is a GET request
    5. applies the subscription middleware, which:
        - modifies the status to 209 Subscription (if request has a Subscription header)
        - adds Subscribe header to response (if request has a Subscription header)
        - catches updates and writes them to a Update channel to be propagated to subscribers (if request is a PUT request)

    all of this is in line with the braid protocol spec, outlined here:
        https://raw.githubusercontent.com/braid-work/braid-spec/master/draft-toomim-httpbis-braid-http-03.txt

    Subscriptions
    -------------
    when we get a subscription request we set up a watcher for updates at a certain channel,
    we then wait for updates to that channel to send them to the client

    we define 2 helper functions for managing subscriptions:
    - catchUpdate
    - sendUpdate

    --REQUEST SIDE--

    catchUpdate
    ------------
    this will be setup on each PUT request to catch these updates, make a patch from it and funnel them to the appropriate channel.
    Implemented as middleware and included in subscriptionMiddleware.

    --RESPONSE SIDE---

    on any GET route that can be subcribed to, use sendUpdate if the request is a subscription request
    
    sendUpdate
    ------------
    takes in headers, a channel and a topic, and watches for new updates to that channel, writing those updates to the client.

-}

sendUpdate :: ResponseHeaders -> Chan Update -> Topic -> Response
sendUpdate headers src topic = responseStream
    status209
    headers
    $ \write flush -> do
        flush
        src' <- liftIO $ dupChan src
        fix $ \loop -> do
            update <- readChan src'
            case updateToBuilder topic update of
                Just b -> write b >> flush >> loop
                Nothing -> loop

braidify :: Chan Update -> Middleware
braidify src =
    subscriptionMiddleware src
    . versionMiddleware
    . addMergeTypeHeader
    . addPatchHeader
    . addHeaders [("Range-Request-Allow-Methods", "PATCH, PUT"), ("Range-Request-Allow-Units", "json")]