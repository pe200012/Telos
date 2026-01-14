# Polysemy Effect System Patterns

本文档总结了 Polysemy 效果系统的核心模式和最佳实践。

## 1. 定义自定义效果 (使用 GADT)

### 基础模式
```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- 效果定义：GADT 形式，kind 为 Effect = (Type -> Type) -> Type -> Type
data MyEffect m a where
  Action1 :: String -> MyEffect m ()
  Action2 :: Int -> MyEffect m Bool
  Action3 :: m a -> (a -> m b) -> MyEffect m b  -- 高阶效果

-- 使用 makeSem 生成智能构造器
makeSem ''MyEffect

-- 生成的函数签名：
-- action1 :: Member MyEffect r => String -> Sem r ()
-- action2 :: Member MyEffect r => Int -> Sem r Bool
-- action3 :: Member MyEffect r => Sem r a -> (a -> Sem r b) -> Sem r b
```

### 真实示例 (来自 wire-server)
```haskell
-- 文件系统效果
data DNSLookup m a where
  LookupSRV :: Domain -> DNSLookup m SrvResponse
  LookupA :: Domain -> DNSLookup m (Either DNS.DNSError [IP.IPv4])

makeSem ''DNSLookup
```

**证据**: [wireapp/wire-server DNSEffect.hs#L24-L30](https://github.com/wireapp/wire-server/blob/develop/libs/dns-util/src/Wire/Network/DNS/Effect.hs#L24-L30)

## 2. 解释器模式

### 一阶效果解释器 (使用 interpret)
```haskell
-- 基础模式：使用 lambda case
teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg
```

**证据**: [polysemy README.md#L94-L97](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/README.md#L94-L97)

### 高阶效果解释器 (使用 interpretH)
```haskell
-- 需要处理包含子计算的效果
interpretTestE :: InterpreterFor TestE r
interpretTestE = interpretH $ \case
  TestE ma f -> do
    a <- runTSimple ma
    bindTSimple f a
```

**证据**: [polysemy TacticsSpec.hs#L5-L11](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/test/TacticsSpec.hs#L5-L11)

### Stateful 解释器
```haskell
-- 使用 stateful 组合器实现状态化解释
runState :: s -> Sem (State s ': r) a -> Sem r (s, a)
runState = stateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
```

**证据**: [polysemy State.hs#L87-L91](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/src/Polysemy/State.hs#L87-L91)

### Reinterpret 模式
```haskell
-- 将一个效果重新解释为其他效果
runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  = runOutputMonoid pure
  . runInputList i
  . reinterpret2 \case  -- reinterpret2 用于两个新效果
      ReadTTY      -> maybe "" id <$> input
      WriteTTY msg -> output msg
```

**证据**: [polysemy README.md#L99-L109](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/README.md#L99-L109)

## 3. 与 IO 集成

### Embed IO 模式
```haskell
-- 使用 Member (Embed IO) 约束直接嵌入 IO 操作
runStateIORef :: forall s r a. Member (Embed IO) r 
              => IORef s -> Sem (State s ': r) a -> Sem r a
runStateIORef ref = interpret $ \case
  Get   -> embed $ readIORef ref
  Put s -> embed $ writeIORef ref s
```

**证据**: [polysemy State.hs#L148-L157](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/src/Polysemy/State.hs#L148-L157)

### embedToFinal 模式
```haskell
-- 用于最终运行 IO 效果
main :: IO (Either CustomException ())
main
  = runFinal
  . embedToFinal @IO
  . resourceToIOFinal
  . errorToIOFinal @CustomException
  . teletypeToIO
  $ program
```

**证据**: [polysemy README.md#L159-L166](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/README.md#L159-L166)

### Final IO 完整运行模式
```haskell
runExample :: IO ()
runExample = do
  result <- runFinal
          $ embedToFinal @IO
          $ runState (Example 0 0) 
          $ example
  print result
```

**证据**: [wiwinwlh polysemy.hs#L34-L38](https://github.com/sdiehl/wiwinwlh/blob/master/src/03-monad-transformers/polysemy.hs#L34-L38)

## 4. Streaming/Conduit 集成

### Streaming 库集成 (polysemy-zoo)
```haskell
-- 将 Streaming 流转换为 Input 效果
runInputViaStream
    :: S.Stream (Of i) (Sem r) ()
    -> InterpreterFor (Input (Maybe i)) r
runInputViaStream stream
  = evalState (Just stream)
  . reinterpret ( \Input ->
      get >>= \case
        Nothing -> pure Nothing
        Just s ->
          raise (S.inspect s) >>= \case
            Left () -> pure Nothing
            Right (i :> s') -> do
              put $ Just s'
              pure $ Just i
  )
```

**证据**: [polysemy-zoo Streaming.hs#L27-L41](https://github.com/polysemy-research/polysemy-zoo/blob/81a4944456854f051c53133cae708727072ee7d8/src/Polysemy/Input/Streaming.hs#L27-L41)

### Conduit 集成模式
```haskell
-- 在 Polysemy 效果中使用 Conduit
-- 方法1: 通过 Embed IO 直接运行
runConduitInEffect :: Member (Embed IO) r => ConduitT () a IO () -> Sem r [a]
runConduitInEffect conduit = embed $ Conduit.runConduit $ conduit .| Conduit.consume

-- 方法2: 创建自定义 Conduit 效果
data ConduitEffect i o m a where
  RunConduit :: ConduitT i o (Sem r) () -> ConduitEffect i o m [o]

interpretConduit :: Member (Embed IO) r 
                 => InterpreterFor (ConduitEffect i o) r
interpretConduit = interpret $ \case
  RunConduit c -> embed $ Conduit.runConduit $ c .| Conduit.consume
```

## 5. 效果组合最佳实践

### 类型别名模式
```haskell
-- 使用 InterpreterFor 类型别名
type InterpreterFor e r = ∀ a. Sem (e ': r) a -> Sem r a

-- 真实使用示例
interpretPasswordStore :: Member (Embed IO) r 
                       => ClientState 
                       -> InterpreterFor PasswordStore r
```

**证据**: [polysemy Internal.hs#L657-L665](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/src/Polysemy/Internal.hs#L657-L665)

### 效果栈组合
```haskell
-- 多个效果的组合运行
runApp :: Sem AppEffects a -> IO (Either SparError a)
runApp = runFinal
       . embedToFinal @IO
       . nowToIO
       . randomToIO
       . runInputConst logger
       . runInputConst opts
       . loggerToTinyLog logger
       . runError
```

**证据**: [wire-server CanonicalInterpreter.hs#L122-L133](https://github.com/wireapp/wire-server/blob/develop/services/spar/src/Spar/CanonicalInterpreter.hs#L122-L133)

### Raise 模式
```haskell
-- 提升效果到栈顶
raise :: Sem r a -> Sem (e ': r) a

-- 在解释器中使用 raise 处理嵌套效果
reinterpret (\Input -> do
    s <- get
    raise (S.inspect s) >>= \case  -- raise 允许访问外层效果
      Left () -> pure Nothing
      Right (i :> s') -> do
        put $ Just s'
        pure $ Just i
  )
```

## 6. 必需的语言扩展

```yaml
# package.yaml 配置
ghc-options: -O2 -flate-specialise -fspecialise-aggressively
default-extensions:
  - DataKinds
  - FlexibleContexts
  - GADTs
  - LambdaCase
  - PolyKinds
  - RankNTypes
  - ScopedTypeVariables
  - TypeApplications
  - TypeOperators
  - TypeFamilies
  - TemplateHaskell  # 用于 makeSem
```

**证据**: [polysemy README.md#L205-L217](https://github.com/polysemy-research/polysemy/blob/cf2efec15ae57a46a81694d361da55da8d7b1d73/README.md#L205-L217)

## 7. 常见模式总结

| 场景 | 使用的组合器 | 示例 |
|------|-------------|------|
| 一阶效果 | `interpret` | State, Output |
| 高阶效果 | `interpretH` | Error.Catch, Resource.bracket |
| 转换为其他效果 | `reinterpret` | Teletype -> Input/Output |
| 状态化解释 | `stateful` | runState |
| IO 嵌入 | `embed` | 读写文件、网络 |
| 最终运行 | `runFinal . embedToFinal` | main 函数 |
| 流处理 | `evalState + reinterpret` | Streaming/Conduit 集成 |

## 参考资源
- [Polysemy 官方仓库](https://github.com/polysemy-research/polysemy)
- [Polysemy Zoo](https://github.com/polysemy-research/polysemy-zoo) - 扩展效果库
- [What I Wish I Knew When Learning Haskell](https://github.com/sdiehl/wiwinwlh) - Polysemy 示例
