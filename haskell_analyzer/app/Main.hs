{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import Data.Aeson
  ( eitherDecode
  , FromJSON(..)
  , Value(..)
  , withObject
  , withArray
  , (.:)
  )
import qualified Data.ByteString.Char8 as B -- change to char8
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import System.Exit (exitFailure)

data MetricEntry = MetricEntry
  { cpuUsage :: Float
  , memUsage :: Float
  , diskRead :: Double
  , diskWrite :: Double
  } deriving (Show, Generic)

instance FromJSON MetricEntry where
  parseJSON = withObject "MetricEntry" $ \v -> do
    cpu <- v .: "cpu_usage"
    mem<- v .: "mem_usage"
    diskR <- v .: "disk_read"
    diskW <- v .: "disk_write"
    return $ MetricEntry cpu mem diskR diskW

detectAnomalies :: [MetricEntry] -> [MetricEntry]
detectAnomalies = filter (\m -> cpuUsage m > 90 || memUsage m > 90)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    connect sock (SockAddrInet 9000 0x7F000001)
    let retryCount = 3
    forever $ do
        sendAll sock (B.pack "get_metrics")
        metricData <- recv sock 8192
        putStrLn $ "Received data: " ++ show metricData
        let parseResult = eitherDecode (BL.fromStrict metricData) :: Either String [MetricEntry]
        case parseResult of
            Right entries -> do
                let anomalies = detectAnomalies entries
                putStrLn $ "Detected anomalies: " ++ show (length anomalies)
                mapM_ print anomalies
            Left err -> putStrLn $ "Parse error: " ++ err
        threadDelay 500000
