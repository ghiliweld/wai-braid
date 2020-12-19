module Network.Wai.Middleware.Braid
    ( 
        -- * Middleware
        braidify,
        subscriptionMiddleware,
        modifyStatusTo209,
        -- * Status
        status209,
        -- * Header
        hSub,
        hVer,
        lookupHeader,
        -- * helpers
        hasSubscription,
        getVersion,
        hasVersion
    ) where

import Network.HTTP.Types.Header   (Header, HeaderName, RequestHeaders, ResponseHeaders)
import Network.HTTP.Types.Status   (Status, mkStatus)
import Network.Wai          (Middleware, modifyResponse, ifRequest, mapResponseHeaders, mapResponseStatus)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Internal (Response(..), Request(..))
import Data.ByteString      (ByteString, find)
import qualified Data.CaseInsensitive  as CI
import qualified Data.ByteString.Char8 as BC

{-|
    braidify
    ---------
    braidify acts as a wai middleware, it :
    1. sets header to ('Range-Request-Allow-Methods', 'PATCH, PUT'), ('Range-Request-Allow-Units', 'json'), ("Patches", "OK")
    2. parse header for version, parents, and subscription
    3. add sendVersion, patches(JSON), and startSubscription helpers to request and response
-}

-- new status code for subscriptions in braid
status209 :: Status
status209 = mkStatus 209 (BC.pack "Subscribed")

modifyStatusTo209 :: Middleware
modifyStatusTo209 = modifyResponse $ mapResponseStatus (const status209)

lookupHeader :: HeaderName -> [Header] -> Maybe ByteString
lookupHeader _ [] = Nothing
lookupHeader v ((n, s):t)  
    |  v == n   =  Just s
    | otherwise =  lookupHeader v t

hSub :: HeaderName
hSub = CI.mk $ BC.pack "Subscribe"

getSubscription :: Request -> Maybe ByteString
getSubscription req = lookupHeader hSub $ requestHeaders req

hasSubscription :: Request -> Bool
hasSubscription req = 
    case getSubscription req of
        Just s -> True
        Nothing -> False

-- still have to add ('cache-control', 'no-cache, no-transform') and ('content-type', 'application/json') to headers
addSubscriptionHeader :: ByteString -> Response -> Response
addSubscriptionHeader s = mapResponseHeaders (\hs -> (hSub, s) : hs)

subscriptionMiddleware :: Middleware
subscriptionMiddleware = subscriptionMiddleware' . modifyStatusTo209
    where
        subscriptionMiddleware' :: Middleware
        subscriptionMiddleware' app req respond = 
            case getSubscription req of
                Just v -> app req $ respond . addSubscriptionHeader v
                Nothing -> app req respond

hVer :: HeaderName   
hVer = CI.mk $ BC.pack "Version"

getVersion :: Request -> Maybe ByteString
getVersion req = lookupHeader hVer $ requestHeaders req

hasVersion :: Request -> Bool
hasVersion req =
    case getVersion req of
        Just s -> True
        Nothing -> False

addVersionHeader :: ByteString -> Response -> Response
addVersionHeader s = mapResponseHeaders (\hs -> (hVer, s) : hs)

versionMiddleware :: Middleware
versionMiddleware app req respond = 
    case getVersion req of
        Just v -> app req $ respond . addVersionHeader v
        Nothing -> app req respond

braidify :: Middleware
braidify =
    versionMiddleware
    . subscriptionMiddleware 
    . addHeaders 
        (map (\(x, y) -> (BC.pack x, BC.pack y)) 
        [("Range-Request-Allow-Methods", "PATCH, PUT"), ("Range-Request-Allow-Units", "json"), ("Patches", "OK")])