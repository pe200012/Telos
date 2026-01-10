module Telos.MCP.Interpreter ( runMCPWithManager ) where

import           Lens.Micro              ( (.~), (^.), non )

import           Polysemy                ( Embed, Member, Sem, embed, interpret )

import           Telos.Core.Error        ( MCPError(..) )
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
      Right toolsWithSource -> pure $ map (^. twsTool) toolsWithSource

  CallTool tName arguments -> embed $ do
    mConn <- findToolServer mgr tName
    case mConn of
      Nothing   -> pure $ Left $ MCPToolNotFound tName
      Just conn -> do
        result <- Client.callTool conn tName (Just arguments)
        pure $ fmap convertToolResult result

  ListResources -> embed $ do
    conns <- getAllConnections mgr
    results <- mapM fetchResources conns
    pure $ concat $ rights' results

  ReadResource uri -> embed $ do
    conns <- getAllConnections mgr
    tryReadFromServers conns uri

convertToolResult :: Types.CallToolResult -> ToolResult
convertToolResult ctr
  = makeToolResult (map convertContentPart (ctr ^. Types.ctrContent))
  & trIsError .~ (ctr ^. Types.ctrIsError . non False)

convertContentPart :: Types.ContentPart -> ContentItem
convertContentPart = \case
  Types.TextPart t -> TextContent t
  Types.ImagePart d m -> ImageContent m d
  Types.ResourcePart u _ (Just t) -> EmbeddedResource u t
  Types.ResourcePart u _ Nothing -> EmbeddedResource u ""

fetchResources :: Client.MCPConnection -> IO (Either MCPError [ Resource ])
fetchResources conn = do
  result <- Client.listResources conn
  pure $ fmap (map convertResource . (^. Types.lrrResources)) result

convertResource :: Types.ResourceInfo -> Resource
convertResource ri
  = makeResource (ri ^. Types.riUri) (ri ^. Types.riName)
  & resMimeType .~ (ri ^. Types.riMimeType)
  & resDescription .~ (ri ^. Types.riDescription)

tryReadFromServers :: [ Client.MCPConnection ] -> Text -> IO (Either MCPError ResourceContent)
tryReadFromServers [] uri = pure $ Left $ MCPResourceNotFound uri
tryReadFromServers (conn : rest) uri = do
  result <- Client.readResource conn uri
  case result of
    Left _    -> tryReadFromServers rest uri
    Right rrr -> case rrr ^. Types.rrrContents of
      []      -> tryReadFromServers rest uri
      (c : _) -> pure
        $ Right
        $ makeResourceContent (c ^. Types.resUri)
        & rcMimeType .~ (c ^. Types.resMimeType)
        & rcText .~ (c ^. Types.resText)
        & rcBlob .~ (c ^. Types.resBlob)

-- | Local version to avoid conflict with Relude.rights
rights' :: [ Either a b ] -> [ b ]
rights' = foldr (\e acc -> either (const acc) (: acc) e) []
