{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CounterApi (
  server,
  counterApiProxy) where

import Control.Monad.IO.Class
import Counter
import CounterEventStore
import Servant

type CounterApi = "counter" :> "latest"    :> Get '[JSON] Counter
             :<|> "counter" :> "increment" :> ReqBody '[JSON] Int :> Post '[JSON] Counter
             :<|> "counter" :> "decrement" :> ReqBody '[JSON] Int :> Post '[JSON] Counter
             :<|> "counter" :> "reset"     :> Post '[JSON] Counter
             :<|> "counter" :> "events"    :> Get '[JSON] [(CounterEvent, Counter)]

server :: CounterEventReader -> CounterEventWriter -> Server CounterApi
server reader writer = getLatestCounter
                  :<|> increment
                  :<|> decrement
                  :<|> reset
                  :<|> getCounterEvents
  where
    getLatestCounter :: Handler Counter
    getLatestCounter = liftIO $ latestCounter reader

    increment :: Int -> Handler Counter
    increment amount = storeEvent (CounterIncremented amount)

    decrement :: Int -> Handler Counter
    decrement amount = storeEvent (CounterDecremented amount)

    reset :: Handler Counter
    reset = storeEvent CounterReset

    getCounterEvents :: Handler [(CounterEvent, Counter)]
    getCounterEvents = liftIO $ getAllCounterEvents reader

    storeEvent :: CounterEvent -> Handler Counter
    storeEvent event = do
                       writeEventResult <- liftIO $ writeEvent event writer reader
                       case writeEventResult of
                         Left _ -> throwError err500
                         Right counter -> return counter

counterApiProxy :: Proxy CounterApi
counterApiProxy = Proxy