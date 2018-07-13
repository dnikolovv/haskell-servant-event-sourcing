{-# LANGUAGE DeriveGeneric #-}

module CounterEventStore (
  CounterEvent (..),
  EventStoreError (..),
  CounterEventReader,
  CounterEventWriter,
  writeEvent,
  latestCounter,
  getAllCounterEvents) where

import Control.Concurrent.STM
import Counter
import Data.Aeson.Types
import Eventful
import GHC.Generics

data CounterEvent = CounterIncremented Int
                  | CounterDecremented Int
                  | CounterReset
                  deriving (Show, Eq, Generic)

instance ToJSON CounterEvent

data EventStoreError = EventStoreError {
  message :: String
} deriving (Show, Eq, Generic)

instance ToJSON EventStoreError

type CounterEventReader = VersionedEventStoreReader STM CounterEvent
type CounterEventWriter = VersionedEventStoreWriter STM CounterEvent
type CounterProjection = Projection Counter CounterEvent

handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter x) (CounterIncremented amount) = Counter $ x + amount
handleCounterEvent (Counter x) (CounterDecremented amount) = Counter $ x - amount
handleCounterEvent _ CounterReset = Counter 0

writeEvent :: CounterEvent -> CounterEventWriter -> CounterEventReader -> IO (Either EventStoreError Counter)
writeEvent event writer reader = do
                                 storeResult <- atomically $ storeEvents writer nil AnyPosition [event]
                                 case storeResult of
                                   Left _ -> return $ Left $ EventStoreError $ "An error occurred while saving the event '" ++ show event ++ "'."
                                   Right _ -> do
                                              counter <- latestCounter reader
                                              return $ Right $ counter

counterProjection :: CounterProjection
counterProjection = Projection {
  projectionSeed = Counter 0,
  projectionEventHandler = handleCounterEvent
}

getAllCounterEvents :: CounterEventReader -> IO [(CounterEvent, Counter)]
getAllCounterEvents reader = do
                             events <- atomically $ getEvents reader (allEvents nil)
                             let counterEvents = map fromVersionedEvent events
                             -- We take the tail, ignoring the initial seed
                             let counters = tail $ allProjections counterProjection counterEvents
                             return $ zip counterEvents counters
  where
    fromVersionedEvent :: VersionedStreamEvent CounterEvent -> CounterEvent
    fromVersionedEvent (StreamEvent _ _ event) = event

latestCounter :: CounterEventReader -> IO Counter
latestCounter reader = do
                       latestStreamProjection <- atomically $ getLatestStreamProjection reader (versionedStreamProjection nil counterProjection)
                       let latestState = streamProjectionState latestStreamProjection
                       return latestState