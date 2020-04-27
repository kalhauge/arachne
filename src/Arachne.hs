-- |
-- Module      :  Wib
-- Copyright   :  (c) Christian Gram Kalhauge 2020
-- License     :  BSD3
--
-- Maintainer  :  christian@kalhauge.dk
--
-- Arachne is a small semi-static site generator.
module Arachne
  (
  -- * Site Generation
  -- $sitegen
  SiteGen

  -- ** Routes
  -- $routes
  , Route
  , routeRelativeUrl
  , routeBaseUrl

  ) where

-- mtl 
import Control.Monad.Writer 

-- text
import Data.Text as Text

-- path
import Path

-- $sitegen
-- The Site gen

-- | Site Gen
newtype SiteGen s a = 
  SiteGen ( WriterT [(Route, s -> Text)] IO a )


-- $routes
-- The goal 

-- | The basic way of refering to another page on the site. 
-- It is automatically generated by the 'SiteGen'.
data Route = Route 
  { routeRelativeUrl :: Path Rel File
  -- ^ Get the relative url of a 'Route'
  , routeBaseUrl :: Path Rel Dir
  -- ^ Get the base url of a 'Route' 
  }


