# OpenCode Persistent Conversation Analysis (2026-01-11)

## 项目概述
- **仓库**: https://github.com/anomalyco/opencode
- **技术栈**: TypeScript + Bun (monorepo with packages/)
- **存储方式**: 文件系统 JSON (非 SQLite)

## 核心存储架构

### 1. Storage 模块 (`packages/opencode/src/storage/storage.ts`)

文件系统存储，使用 key 数组路径：

```typescript
namespace Storage {
  // 基础操作
  read<T>(key: string[])           // 读取 JSON
  write<T>(key: string[], content) // 写入 JSON
  update<T>(key: string[], fn)     // 原子更新（带锁）
  remove(key: string[])            // 删除
  list(prefix: string[])           // 列出目录
}
```

**存储目录结构**:
```
Global.Path.data/storage/
├── session/{projectID}/{sessionID}.json
├── message/{sessionID}/{messageID}.json
├── part/{messageID}/{partID}.json
├── project/{projectID}.json
├── session_diff/{sessionID}.json
└── migration  # 迁移版本号
```

**特性**:
- 使用 `Lock.read/write` 进行文件锁
- 支持迁移 (MIGRATIONS 数组)
- JSON 格式，美化输出

### 2. Session 模块 (`packages/opencode/src/session/index.ts`)

```typescript
namespace Session {
  // Session 元数据
  interface Info {
    id: string              // Identifier.descending("session")
    projectID: string
    directory: string
    parentID?: string       // 支持子会话
    title: string
    version: string
    time: {
      created: number
      updated: number
      compacting?: number
      archived?: number
    }
    summary?: { additions, deletions, files, diffs }
    share?: { url }
    permission?: PermissionNext.Ruleset
    revert?: { messageID, partID?, snapshot?, diff? }
  }
  
  // 生命周期
  create(options?)          // 创建新会话
  createNext(input)         // 内部创建
  get(sessionID)            // 获取会话
  update(id, editor)        // 更新会话
  remove(sessionID)         // 删除（含消息和部件）
  list()                    // 列出所有会话
  touch(sessionID)          // 更新 updated 时间
  fork({ sessionID, messageID? })  // 复制会话到指定消息
  
  // 消息操作
  messages({ sessionID, limit? })  // 获取消息列表
  updateMessage(msg)        // 更新/创建消息
  removeMessage({ sessionID, messageID })
  updatePart(part)          // 更新/创建部件
  
  // 事件
  Event.Created, Event.Updated, Event.Deleted, Event.Diff, Event.Error
}
```

### 3. Message 结构 (`packages/opencode/src/session/message-v2.ts`)

```typescript
namespace MessageV2 {
  // 消息基础信息
  interface Info {
    id: string
    sessionID: string
    role: "user" | "assistant"
    parentID?: string  // assistant 消息引用 user 消息
    // ...其他元数据
  }
  
  // 部件类型 (Part)
  type Part = 
    | TextPart           // 文本内容
    | ReasoningPart      // 推理过程
    | FilePart           // 文件附件
    | SnapshotPart       // 快照
    | PatchPart          // 补丁
    | AgentPart          // Agent 调用
    | CompactionPart     // 压缩标记
    | SubtaskPart        // 子任务
    | RetryPart          // 重试信息
    | StepStartPart      // 步骤开始
    | StepFinishPart     // 步骤结束 (含 tokens, cost)
    | ToolPart           // 工具调用
  
  // 工具状态
  type ToolState = 
    | ToolStatePending   // 等待执行
    | ToolStateRunning   // 执行中
    | ToolStateCompleted // 已完成
  
  // 带部件的消息
  interface WithParts {
    info: Info
    parts: Part[]
  }
}
```

### 4. 前端持久化 (`packages/app/src/utils/persist.ts`)

使用 `@solid-primitives/storage` 的 `makePersisted`:

```typescript
const Persist = {
  global(key)                      // 全局配置
  workspace(dir, key)              // 工作区配置
  session(dir, session, key)       // 会话配置
  scoped(dir, session?, key)       // 自动选择
}
```

**存储文件**:
- `opencode.global.dat` - 全局
- `opencode.workspace.{hash}.dat` - 工作区

## 关键设计决策

1. **文件系统而非数据库**: 简单、可移植、易调试
2. **分层存储**: session → message → part 三级结构
3. **ID 生成**: `Identifier.descending/ascending` 保证排序
4. **事件驱动**: Bus 发布 Created/Updated/Deleted 事件
5. **原子更新**: 使用文件锁保证一致性
6. **迁移支持**: MIGRATIONS 数组支持 schema 演进
7. **子会话**: parentID 支持会话分叉

## 为 Telos 的实现建议

### 方案 A: 简单文件存储 (推荐)
```haskell
-- 类似 opencode 的目录结构
data StoragePath
  = SessionPath ProjectId SessionId
  | MessagePath SessionId MessageId
  | ConfigPath

-- 核心操作
readJson :: FromJSON a => StoragePath -> IO (Maybe a)
writeJson :: ToJSON a => StoragePath -> a -> IO ()
updateJson :: (FromJSON a, ToJSON a) => StoragePath -> (a -> a) -> IO a
listDir :: StoragePath -> IO [FilePath]
```

### 方案 B: SQLite
```haskell
-- 适合更复杂的查询需求
data Session = Session
  { sessionId :: Text
  , projectId :: Text
  , title :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }

data StoredMessage = StoredMessage
  { msgId :: Text
  , sessionId :: Text
  , role :: Text
  , content :: Text  -- JSON 编码的 Message
  , createdAt :: UTCTime
  }
```

### 需要添加到 Telos 的类型

```haskell
-- 会话信息
data SessionInfo = SessionInfo
  { _siId :: Text
  , _siTitle :: Text
  , _siCreatedAt :: UTCTime
  , _siUpdatedAt :: UTCTime
  }

-- 可序列化的消息 (不含 TVar)
data StoredMessage = StoredMessage
  { _smId :: Text
  , _smSessionId :: Text
  , _smContent :: Message  -- 现有类型
  , _smCreatedAt :: UTCTime
  }
```
