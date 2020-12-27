# wai-braid
braid protocol server in haskell, implemented as wai middleware

[my writeup](https://github.com/ghiliweld/writings/blob/master/braid.md) on the [braid protocol](https://braid.news/)

### Usage

```hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.Braid (braidify, sendUpdate, hasSubscription, Update)
import Network.Wai
import Control.Concurrent.Chan (Chan, newChan)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status (status200)
    
application src req respond = respond $ 
    if hasSubscription req 
    then sendUpdate [("Content-Type", "text/plain")] src ["topic"]
    else responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main :: IO ()
main = newChan >>= \src -> run 3000 $ braidify src $ application src
```