module Main where

import CounterApi
import CounterEventStore
import Eventful.Store.Memory
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Servant

port :: Int
port = 8081

getEventStoreReaderWriter :: IO (CounterEventReader, CounterEventWriter)
getEventStoreReaderWriter = do
                            tvar <- eventMapTVar
                            let reader = tvarEventStoreReader tvar
                                writer = tvarEventStoreWriter tvar
                            return (reader, writer)

app :: CounterEventReader -> CounterEventWriter -> Application
app reader writer = serve counterApiProxy $ server reader writer

configureLoggerSettings :: ApacheLogger -> Settings
configureLoggerSettings logger = setPort port $ setLogger logger defaultSettings

runApp :: CounterEventReader -> CounterEventWriter -> IO ()
runApp reader writer = withStdoutLogger $ runWithSettings . configureLoggerSettings
    where 
        runWithSettings :: Settings -> IO ()
        runWithSettings settings = runSettings settings $ app reader writer

main :: IO ()
main = do
       (reader, writer) <- getEventStoreReaderWriter
       putStrLn $ "Listening on port " ++ show port ++ "..."
       runApp reader writer