module Telos.MCP.Interpreter ( runMCPWithManager ) where

import           Data.Aeson              ( Value )
import           Data.Text               ( Text )

import           Polysemy                ( Embed, Member, Sem, embed, interpret )

import           Telos.Core.Error        ( MCPError(..) )
import           Telos.Core.Types        ( Tool(..) )
import           Telos.Effect.MCP
import qualified Telos.MCP.Client        as Client
import           Telos.MCP.ServerManager
import qualified Telos.MCP.Types         as Types

runMCPWithManager :: Member (Embed IO) r => ServerManager -> Sem (MCP ': r) a -> Sem r a
runMCPWithManager mgr = interpret $ \case
  ListTools -> embed $ do
    result <- aggregateTools mgr
    case result of
      Left _ -> pure []
      Right toolsWithSource -> pure $ map twsTool toolsWithSource

  CallTool name arguments -> embed $ do
    mConn <- findToolServer mgr name
    case mConn of
      Nothing   -> pure $ Left $ MCPToolNotFound name
      Just conn -> do
        result <- Client.callTool conn name (Just arguments)
        pure $ fmap convertToolResult result

  ListResources -> embed $ do
    conns <- getAllConnections mgr
    results <- mapM fetchResources conns
    pure $ concat $ rights results

  ReadResource uri -> embed $ do
    conns <- getAllConnections mgr
    tryReadFromServers conns uri

convertToolResult :: Types.CallToolResult -> ToolResult
convertToolResult ctr
  = ToolResult { trContent = map convertContentPart (Types.ctrContent ctr)
               , trIsError = maybe False id (Types.ctrIsError ctr)
               }

convertContentPart :: Types.ContentPart -> ContentItem
convertContentPart = \case
  Types.TextPart t -> TextContent t
  Types.ImagePart d m -> ImageContent { icMimeType = m, icBase64Data = d }
  Types.ResourcePart u _ (Just t) -> EmbeddedResource { erUri = u, erText = t }
  Types.ResourcePart u _ Nothing -> EmbeddedResource { erUri = u, erText = "" }

fetchResources :: Client.MCPConnection -> IO (Either MCPError [ Resource ])
fetchResources conn = do
  result <- Client.listResources conn
  pure $ fmap (map convertResource . Types.lrrResources) result

convertResource :: Types.ResourceInfo -> Resource
convertResource ri
  = Resource { resUri         = Types.riUri ri
             , resName        = Types.riName ri
             , resMimeType    = Types.riMimeType ri
             , resDescription = Types.riDescription ri
             }

tryReadFromServers :: [ Client.MCPConnection ] -> Text -> IO (Either MCPError ResourceContent)
tryReadFromServers [] uri = pure $ Left $ MCPResourceNotFound uri
tryReadFromServers (conn : rest) uri = do
  result <- Client.readResource conn uri
  case result of
    Left _    -> tryReadFromServers rest uri
    Right rrr -> case Types.rrrContents rrr of
      []      -> tryReadFromServers rest uri
      (c : _) -> pure
        $ Right
          ResourceContent { rcUri      = Types.rcUri c
                          , rcMimeType = Types.rcMimeType c
                          , rcText     = Types.rcText c
                          , rcBlob     = Types.rcBlob c
                          }

rights :: [ Either a b ] -> [ b ]
rights = foldr (\e acc -> either (const acc) (: acc) e) []
