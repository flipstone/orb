{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestDispatchM
  ( TestDispatchEnv (..)
  , TestDispatchM
  , runTestDispatchM
  ) where

import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Class qualified as MIO
import Control.Monad.Reader qualified as Reader
import Network.Wai qualified as Wai

import Orb qualified

data TestDispatchEnv = TestDispatchEnv
  { testDispatchRequest :: Wai.Request
  , testDispatchRespond :: Wai.Response -> IO Wai.ResponseReceived
  }

newtype TestDispatchM a
  = TestDispatchM (Reader.ReaderT TestDispatchEnv IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    , Safe.MonadThrow
    , Safe.MonadCatch
    )

instance Orb.HasRequest TestDispatchM where
  request = TestDispatchM (Reader.asks testDispatchRequest)

instance Orb.HasRespond TestDispatchM where
  respond = TestDispatchM (Reader.asks testDispatchRespond)

instance Orb.HasLogger TestDispatchM where
  log = MIO.liftIO . putStrLn . Safe.displayException

runTestDispatchM :: TestDispatchM Wai.ResponseReceived -> Wai.Application
runTestDispatchM (TestDispatchM reader) request respond =
  let
    env =
      TestDispatchEnv
        { testDispatchRequest = request
        , testDispatchRespond = respond
        }
  in
    Reader.runReaderT reader env
