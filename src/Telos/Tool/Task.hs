{-# LANGUAGE OverloadedStrings #-}

module Telos.Tool.Task ( taskBuiltinTool, taskTool ) where

import           Control.Lens     ( (?~) )

import           Data.Aeson       ( (.=), Value, object )

import           Relude

import           Telos.Core.Types ( Tool, makeTool, toolDescription )

-- | Task tool for delegating subtasks to subagents
taskTool :: Tool
taskTool = makeTool "task" inputSchema & toolDescription ?~ description
  where
    description :: Text
    description
      = "Delegate a subtask to a new agent. The agent runs with \
                  \isolated conversation history but shares tools. Use this \
                  \for complex subtasks that benefit from focused execution."

    inputSchema :: Value
    inputSchema
      = object
        [ "type" .= ("object" :: Text)
        , "properties"
          .= object
            [ "prompt"
              .= object
                [ "type" .= ("string" :: Text)
                , "description"
                  .= ("The task description for the subagent. \
                                  \Be specific about what you want done."
                        :: Text)
                ]
            , "max_iterations"
              .= object
                [ "type" .= ("integer" :: Text)
                , "description"
                  .= ("Maximum iterations for the subagent \
                                  \(default: 10)"
                        :: Text)
                ]
            , "description"
              .= object
                [ "type" .= ("string" :: Text)
                , "description"
                  .= ("Brief description of the task for \
                                  \logging purposes"
                        :: Text)
                ]
            ]
        , "required" .= ([ "prompt" ] :: [ Text ])
        ]

-- | Placeholder for the builtin tool registration
-- The actual executor will be added when we refactor BuiltinTool
-- to support agent-aware executors
taskBuiltinTool :: Tool
taskBuiltinTool = taskTool
