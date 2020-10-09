{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Arachne.GraphT
-- Copyright   :  (c) Christian Gram Kalhauge, 2020
-- License     :  BSD3
--
-- Maintainer  :  christian@kalhauge.dk
--
-- A small server-side FRP push-pull graph.
module Arachne.GraphT
 ( GraphT (..)
 -- , oneShot
 , drawGraphT
 , nodeG
 ) where

-- unliftio
import Control.Monad.IO.Unlift
import UnliftIO.Async

-- exceptions
import Control.Monad.Catch

-- mtl
import Control.Monad.State
import Control.Monad.Writer

-- base
import Control.Monad.IO.Class
import Debug.Trace
import Text.Show
import Data.Functor.Classes

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- path
import Path

-- free
import Control.Monad.Free
import qualified Control.Monad.Free.Church as F
import Control.Monad.Free.TH

-- | A GraphF generates a graph which can be run or inspected.
data GraphF m f
  = forall a. NodeG (m a) (a -> f)
  | forall a b. ParG (GraphT m a) (GraphT m b) ((a, b) -> f)

deriving instance (Show1 m => Show1 (GraphF m))
deriving instance (Functor (GraphF m))

newtype GraphT m a = GraphT
  { unGraphT :: Free (GraphF m) a
  } deriving (Functor)

instance MonadFree (GraphF m) (GraphT m) where
  wrap = GraphT . wrap . fmap unGraphT
  {-# INLINE wrap #-}

makeFree_ ''GraphF

instance Monad (GraphT m) where
  return = GraphT . return
  (GraphT ma) >>= amb = GraphT (ma >>= unGraphT . amb)

instance Applicative (GraphT m) where
  pure = pure
  a <*> b = (\(fn,x) -> fn x) <$> parG a b

-- Create a node in the graph
nodeG :: m a -> GraphT m a

-- | Runs a graphs a GraphT
drawGraphT :: Monad m => GraphT m a -> m (a, [String])
drawGraphT gt =
  runWriterT (evalStateT (unwrapGraphT gt) (0, []))
 where
  unwrapGraphT (GraphT f) = do
    traceShowM f
    iterM handler f

  handler ::
    Monad m => GraphF m (StateT (Int, [Int]) (WriterT [String] m) a)
    -> StateT (Int, [Int]) (WriterT [String] m) a
  handler = \case
    NodeG ma mx -> do
      traceM "node"
      (idx, lasts) <- get
      tell [ show last ++ " -> " ++ show idx ++ ";" | last <- lasts ]
      put (idx +1, [idx])
      x <- lift (lift ma)
      traceM "exit node"
      mx x

    ParG ma mb mx -> do
      traceM "par"
      lasts <- gets snd
      (ix, xs) <- get
      a <- unwrapGraphT ma
      traceM "exit a"
      b <- unwrapGraphT mb
      traceM "exit b"
      (ix2, ys) <- get
      put (ix2, xs ++ ys)
      mx (a, b)






-- instance MonadResource (GraphT m) where
--   getResource = fileG
--
-- instance MonadThrow m => MonadThrow (GraphT m) where
--   throwM = pullG . throwM

-- -- | Create a pull event. This will be run every time the graph
-- -- is evaluated.
-- pullG :: m a -> GraphT m a
--
-- -- | Create a file event. In continues mode this will push updates
-- -- every time the file has changed.
-- fileG :: Path Rel File -> GraphT m (Maybe BL.ByteString)
--
-- -- | Checkpoint a computation. The graph will only continue pulling and pushing
-- -- beyond this point if the value have changed.
-- checkpointG :: GraphT m a -> GraphT m a

-- | Run two computations in parallel.
parG :: GraphT m a -> GraphT m b -> GraphT m (a, b)

-- data GraphConfig = GraphConfig
--   { dataDir  :: Path Rel Dir
--   , cacheDir :: Path Rel Dir
--   } deriving (Show)
--
-- -- | Run the graph a single time.
-- oneShot :: (MonadCatch m, MonadUnliftIO m) => GraphConfig -> GraphT m a -> m a
-- oneShot cfg (GraphT f) = flip F.iterM f \case
--   PullG ma c -> do
--     liftIO (putStrLn $ "PullG")
--     ma >>= c
--
--   FileG pth c -> do
--     liftIO (putStrLn $ "Read " ++ show pth)
--     try (liftIO $ BL.readFile p) >>= \case
--       Right bs -> do
--         liftIO (putStrLn $ "Successful")
--         a <- c $ Just bs
--         liftIO (putStrLn $ "Done")
--         return a
--       Left (msg :: IOError) ->
--         c $ Nothing
--    where
--     p = fromRelFile (dataDir cfg Path.</> pth)
--
--   CheckpointG x c -> do
--     liftIO (putStrLn $ "Checkpoint")
--     oneShot cfg x >>= c
--
--   ParG a b c -> do
--     liftIO (putStrLn $ "ParG")
--     concurrently (oneShot cfg a) (oneShot cfg b) >>= c
--
-- class MonadResource m where
--   -- | Get a resource.
--   getResource :: Path Rel File -> m (Maybe BL.ByteString)

