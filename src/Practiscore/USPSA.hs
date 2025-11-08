{-# LANGUAGE DerivingStrategies #-}

module Practiscore.USPSA (CompId (..), UspsaMemberId (..)) where

newtype CompId = CompId
  { unCompId :: Word8
  }
  deriving newtype (Show, Eq)

newtype UspsaMemberId = UspsaMemberId
  { unUspsaMemberId :: Text
  }
  deriving newtype (Show, Eq)

instance ToText UspsaMemberId where
  toText (UspsaMemberId {unUspsaMemberId}) = unUspsaMemberId
