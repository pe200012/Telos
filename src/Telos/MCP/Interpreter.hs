module Telos.MCP.Interpreter ( runMCPWithManager ) where

import           Lens.Micro              ( (.~), (^.), non )

import           Polysemy                ( Embed, Member, Sem, embed, interpret )

import           Relude

import           Telos.Core.Error        ( MCPError(..) )
import           Telos.Effect.MCP
import           Telos.MCP.Client        ( MCPConnection )
import qualified Telos.MCP.Client        as Client
import           Telos.MCP.ServerManager ( ServerManager
                                         , aggregateTools
                                         , findToolServer
                                         , getOrConnectServer
                                         )
import qualified Telos.MCP.Types         as Types

-- | Run MCP effect with ServerManager (lazy connection)
runMCPWithManager :: Member (Embed IO) r => ServerManager -> Sem (MCP ': r) a -> Sem r a
runMCPWithManager mgr = interpret $ \case
  ListTools -> embed $ do
    result <- aggregateTools mgr
    case result of
      Left _ -> pure []
      Right toolsWithSource -> pure $ map fst toolsWithSource

  CallTool tName arguments -> embed $ do
    mServerConn <- findToolServer mgr tName
    case mServerConn of
      Nothing          -> pure $ Left $ MCPToolNotFound tName
      Just ( _, conn ) -> do
        result <- Client.callTool conn tName (Just arguments)
        pure $ fmap convertToolResult result

  ListResources -> embed $ do
    result <- aggregateTools mgr
    case result of
      Left _ -> pure []
      Right toolsWithSource -> do
        let serverNames = ordNub $ map snd toolsWithSource
        conns <- forM serverNames $ \name -> do
          connResult <- getOrConnectServer mgr name
          pure $ either (const Nothing) Just connResult
        results <- mapM fetchResources (catMaybes conns)
        pure $ concat $ rights' results

  ReadResource uri -> embed $ do
    result <- aggregateTools mgr
    case result of
      Left _ -> pure $ Left $ MCPResourceNotFound uri
      Right toolsWithSource -> do
        let serverNames = ordNub $ map snd toolsWithSource
        conns <- forM serverNames $ \name -> do
          connResult <- getOrConnectServer mgr name
          pure $ either (const Nothing) Just connResult
        tryReadFromServers (catMaybes conns) uri

-- Helper functions

convertToolResult :: Types.CallToolResult -> ToolResult
convertToolResult ctr
  = makeToolResult (map convertContent (ctr ^. Types.ctrContent))
  & trIsError .~ (ctr ^. Types.ctrIsError . non False)

convertContent :: Types.ContentPart -> ContentItem
convertContent (Types.TextPart txt)             = TextContent txt
convertContent (Types.ImagePart dat mime)       = ImageContent dat mime
convertContent (Types.ResourcePart uri _ mText) = TextContent $ mText ^. non uri

fetchResources :: MCPConnection -> IO (Either MCPError [ Resource ])
fetchResources conn = do
  result <- Client.listResources conn
  case result of
    Left err  -> pure $ Left err
    Right lrr -> pure $ Right $ map convertResource (lrr ^. Types.lrrResources)

convertResource :: Types.ResourceInfo -> Resource
convertResource ri
  = makeResource (ri ^. Types.riUri) (ri ^. Types.riName)
  & resDescription .~ (ri ^. Types.riDescription)
  & resMimeType .~ (ri ^. Types.riMimeType)

tryReadFromServers :: [ MCPConnection ] -> Text -> IO (Either MCPError Types.ResourceContents)
tryReadFromServers [] uri = pure $ Left $ MCPResourceNotFound uri
tryReadFromServers (conn : rest) uri = do
  result <- Client.readResource conn uri
  case result of
    Left _    -> tryReadFromServers rest uri
    Right rrr -> case rrr ^. Types.rrrContents of
      [] -> tryReadFromServers rest uri
      (content : _) -> pure $ Right content

-- | Local version to avoid conflict with Relude.rights
rights' :: [ Either a b ] -> [ b ]
rights' = foldr (\e acc -> either (const acc) (: acc) e) []
