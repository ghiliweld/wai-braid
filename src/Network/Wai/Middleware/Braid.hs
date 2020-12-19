module Network.Wai.Middleware.Braid
    ( 
 --       braidify
    ) where

import Network.HTTP.Types   (Header)
import Network.Wai          (Middleware, modifyResponse, mapResponseHeaders)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Internal (Response(..))
import Data.ByteString      (ByteString)
import qualified Data.ByteString.Char8 as BC

{-|
    braidify
    ---------
    braidify acts as a wai middleware, it :
    1. sets header to ('Range-Request-Allow-Methods', 'PATCH, PUT'), ('Range-Request-Allow-Units', 'json'), ("Patches", "OK")
    2. parse header for version, parents, and subscription
    3. add sendVersion, patches(JSON), and startSubscription helpers to request and response

    1. can be done w/ addHeaders middleware (github.com/yesodweb/wai/wai-extra)

    code draft
    ----------
    braidify :: Middleware
    braidify = 
    helperMiddleware 
    $ parseHeaders 
    $ addHeaders headers
    where helperMiddleware, parseHeaders, addHeaders are other middlewares chained together w/ $

-}

braidify :: Middleware
braidify = addHeaders $ map (\(x, y) -> (BC.pack x, BC.pack y)) [("Range-Request-Allow-Methods", "PATCH, PUT"), ("Range-Request-Allow-Units", "json"), ("Patches", "OK")]