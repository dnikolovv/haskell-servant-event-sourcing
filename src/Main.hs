module Main where

import CounterApi
import CounterEventStore
import Eventful.Store.Memory
import Network.Wai.Handler.Warp
import Servant

port :: Int
port = 8081

app :: CounterEventReader -> CounterEventWriter -> Application
app reader writer = serve counterApiProxy $ server reader writer

main :: IO ()
main = do
       tvar <- eventMapTVar
       let reader = tvarEventStoreReader tvar
           writer = tvarEventStoreWriter tvar
       putStr $ "Listening on port " ++ show port ++ "..."
       run port $ app reader writer