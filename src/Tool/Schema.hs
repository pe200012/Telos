{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Tool.Schema
  ( ToolSpec
  , ToolFunctionSpec
  , defaultToolSpec
  , defaultToolFunctionSpec
  , HasToolType(..)
  , HasToolFunction(..)
  , HasToolName(..)
  , HasToolDescription(..)
  , HasToolParameters(..)
  ) where

import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Data.Aeson      ( (.=), ToJSON(toJSON), Value(Null), object )

import           Relude

data ToolFunctionSpec
  = ToolFunctionSpec { _toolName :: Text, _toolDescription :: Text, _toolParameters :: Value }
  deriving ( Eq, Show, Generic )

data ToolSpec = ToolSpec { _toolType :: Text, _toolFunction :: ToolFunctionSpec }
  deriving ( Eq, Show, Generic )

instance ToJSON ToolFunctionSpec where
  toJSON spec
    = object
      [ "name" .= _toolName spec
      , "description" .= _toolDescription spec
      , "parameters" .= _toolParameters spec
      ]

instance ToJSON ToolSpec where
  toJSON spec = object [ "type" .= _toolType spec, "function" .= _toolFunction spec ]

makeFieldsNoPrefix ''ToolFunctionSpec

makeFieldsNoPrefix ''ToolSpec

defaultToolSpec :: ToolSpec
defaultToolSpec = ToolSpec { _toolType = "function", _toolFunction = defaultToolFunctionSpec }

defaultToolFunctionSpec :: ToolFunctionSpec
defaultToolFunctionSpec
  = ToolFunctionSpec { _toolName = "", _toolDescription = "", _toolParameters = Null }
