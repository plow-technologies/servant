{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.IsSecure where

import Data.Typeable

data IsSecure = Secure | NotSecure
  deriving Typeable
