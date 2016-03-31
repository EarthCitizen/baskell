{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Element (Element(..)) where

data Element = Print String | Block [Element] | Main Element deriving (Eq, Ord, Show)
