{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Element (Element(Print, Block)) where

data Element = Print String | Block [Element] deriving (Eq, Ord, Show)
