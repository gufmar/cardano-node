{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.SubmitApi
  ( submitApi
  , submitApiSubmitTransaction
  ) where

import           Control.Exception (IOException, handle)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON (..), Value, (.=), (.:))
import           Data.Bool
import           Data.Eq
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Data.Text (Text)
import           GHC.Generics
import           GHC.Num
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Stock.Aeson (rewriteObject)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath, IO)
import           Text.Show

import qualified Data.Aeson as J
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Network.Curl as C
import qualified System.Directory as IO
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Base as H
import qualified Test.Process as H
import qualified Testnet.Cardano as H
import qualified Testnet.Conf as H
import qualified Testnet.List as L

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use head" -}

submitApi :: H.SubmitApiConfig -> H.Integration Int
submitApi H.SubmitApiConfig {..} = do
  hStdout <- H.openFile stdoutFile IO.WriteMode
  hStderr <- H.openFile stderrFile IO.WriteMode

  [port] <- H.noteShowIO $ IO.allocateRandomPorts 1

  void $ H.createProcess =<<
    ( H.procSubmitApi
      [ "--config", configFile
      , "--socket-path", IO.sprocketArgumentName sprocket
      , "--port", show @Int port
      , "--testnet-magic", show @Int testnetMagic
      ] <&>
      ( \cp -> cp
        { IO.std_in = IO.CreatePipe
        , IO.std_out = IO.UseHandle hStdout
        , IO.std_err = IO.UseHandle hStderr
        , IO.cwd = Just tempBaseAbsPath
        }
      )
    )

  return port

newtype ObjectWithCBorHexField = ObjectWithCBorHexField
  { cborHex :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ObjectWithCBorHexField where
  parseJSON = J.withObject "ObjectWithCBorHexField" $ \obj -> ObjectWithCBorHexField
    <$> obj .: "cborHex"

submitApiSubmitTransaction :: Int -> FilePath -> H.Integration Bool
submitApiSubmitTransaction port txFile = do
  txJson <- H.readJsonFile txFile & H.leftFailM

  objectWithCBorHexField <- J.fromJSON @ObjectWithCBorHexField txJson & H.jsonErrorFail

  bs <- Base16.decode (T.encodeUtf8 (cborHex objectWithCBorHexField)) & H.leftFail

  -- From https://mail.haskell.org/pipermail/beginners/2011-May/006967.html
  result <- liftIO . handleIOException (const $ return Nothing) $ C.withCurlDo $ do
    bodyRef <- newIORef []
    h <- C.initialize
    mapM_ (C.setopt h)
      [ C.CurlURL ("http://localhost:" <> show port <> "/api/submit/tx")
      , C.CurlNoBody False
      , C.CurlFollowLocation False
      , C.CurlMaxRedirs 0
      , C.CurlAutoReferer False
      , C.CurlUserAgent "Mozilla/5.0"
      , C.CurlNoSignal True
      , C.CurlVerbose True
      , C.CurlPostFields [BS8.unpack bs]
      , C.CurlHttpHeaders ["Content-Type: application/cbor"]
      , C.CurlWriteFunction $ bodyFunction bodyRef
      ]
    code <- C.perform h
    if code /= C.CurlOK
        then return Nothing
        else Just . BS.fromChunks . L.reverse <$> readIORef bodyRef

  return (isJust result)

bodyFunction :: IORef [BSS.ByteString] -> C.WriteFunction
bodyFunction r = C.gatherOutput_ $ \s -> do
                   bs <- BSS.packCStringLen s
                   modifyIORef r (bs:)

handleIOException :: (IOException -> IO a) -> IO a -> IO a
handleIOException handler = handle (\(e :: IOException) -> handler e)
