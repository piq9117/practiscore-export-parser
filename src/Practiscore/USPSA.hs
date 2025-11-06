{-# LANGUAGE DerivingStrategies #-}

module Practiscore.USPSA (CompId (..)) where

newtype CompId = CompId
  { unCompId :: Word8
  }
  deriving newtype (Show, Eq)
