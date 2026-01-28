{-# LANGUAGE GADTs #-}

module Markdown.RenderAnsi ( RenderState, newRenderState, renderLineAnsi, renderMarkdown ) where

import           Data.Attoparsec.Text ( Parser
                                      , choice
                                      , endOfInput
                                      , parseOnly
                                      , takeText
                                      , takeWhile
                                      , takeWhile1
                                      )
import           Data.Char            ( isAlphaNum, isSpace )
import qualified Data.Text            as Text

import           Relude               hiding ( takeWhile )

import           System.Console.ANSI  ( Color(..)
                                      , ColorIntensity(..)
                                      , ConsoleIntensity(..)
                                      , ConsoleLayer(..)
                                      , SGR(..)
                                      , setSGRCode
                                      )

data LineToken
  = LineBlank
  | LineHeading Int Text
  | LineList Int Text
  | LineQuote Text
  | LineFence Text
  | LineText Text
  deriving ( Eq, Show )

data RenderState = RenderState { _inCodeBlock :: Bool, _fenceMarker :: Text, _prevBlank :: Bool }

newRenderState :: RenderState
newRenderState = RenderState False "" False

renderMarkdown :: Text -> [ Text ]
renderMarkdown input
  = let
      -- Text.lines does not keep the trailing empty line when input ends with "\n".
      -- This avoids rendering a spurious blank line at the end of streamed responses.
      linesText = Text.lines input
    in 
      snd (foldl' step ( newRenderState, [] ) linesText)
  where
    step ( st, acc ) lineText
      = let
          ( st', out ) = renderLineAnsi st lineText
        in 
          ( st', acc <> out )

renderLineAnsi :: RenderState -> Text -> ( RenderState, [ Text ] )
renderLineAnsi renderState lineText
  | _inCodeBlock renderState
    = if isFenceEnd (_fenceMarker renderState) lineText
      then ( renderState { _inCodeBlock = False, _prevBlank = False }, [] )
      else ( renderState { _prevBlank = False }, [ applyLineStyle codeStyle lineText ] )
  | otherwise = case parseLineToken lineText of
    LineBlank
      | _prevBlank renderState -> ( renderState, [] )
      | otherwise -> ( renderState { _prevBlank = True }, [ "" ] )
    LineFence marker
      -> ( renderState { _inCodeBlock = True, _fenceMarker = marker, _prevBlank = False }, [] )
    LineHeading level content -> ( renderState { _prevBlank = False }
                                 , [ applyLineStyle (headingStyle level) (renderInline content) ]
                                 )
    LineList indent content -> let
        prefix = Text.replicate indent " " <> "- "
      in 
        ( renderState { _prevBlank = False }, [ prefix <> renderInline content ] )
    LineQuote content -> ( renderState { _prevBlank = False }
                         , [ applyLineStyle quoteStyle ("| " <> renderInline content) ]
                         )
    LineText content -> ( renderState { _prevBlank = False }, [ renderInline content ] )

parseLineToken :: Text -> LineToken
parseLineToken lineText
  | Text.all isSpace lineText = LineBlank
  | otherwise = fromRight (LineText lineText) (parseOnly lineParser lineText)

lineParser :: Parser LineToken
lineParser
  = choice [ headingParser, listParser, quoteParser, fenceParser, LineText <$> takeText ]
  <* endOfInput

headingParser :: Parser LineToken
headingParser = do
  hashes <- takeWhile1 (== '#')
  let level = Text.length hashes
  guard (level <= 6)
  _ <- takeWhile1 isSpace
  LineHeading level <$> takeText

listParser :: Parser LineToken
listParser = do
  indent <- Text.length <$> takeWhile (== ' ')
  _ <- choice [ "- ", "* ", "+ " ]
  LineList indent <$> takeText

quoteParser :: Parser LineToken
quoteParser = do
  _ <- "> "
  LineQuote <$> takeText

fenceParser :: Parser LineToken
fenceParser = do
  -- Allow optional indentation and info string (e.g. ```python).
  -- We only track the fence marker itself; the info string is ignored for ANSI output.
  _ <- takeWhile (== ' ')
  marker <- choice [ "```", "~~~" ]
  _ <- takeText
  pure (LineFence marker)

isFenceEnd :: Text -> Text -> Bool
isFenceEnd marker lineText
  = let
      stripped = Text.stripStart lineText
    in 
      case Text.stripPrefix marker stripped of
        Nothing   -> False
        Just rest -> Text.all isSpace rest

data InlineStyle = StyleBold | StyleItalic | StyleCode
  deriving ( Eq, Show )

renderInline :: Text -> Text
renderInline input
  = let
      ( acc, stack, _ ) = go [] Nothing input []
      result = Text.concat (reverse acc)
    in 
      if null stack
        then result
        else result <> reset
  where
    go stack prev remaining acc = case Text.uncons remaining of
      Nothing           -> ( acc, stack, prev )
      Just ( ch, rest )
        | isInCode stack -> if ch == '`'
          then let
              stack' = toggle StyleCode stack
              acc'   = transition stack'
            in 
              go stack' Nothing rest (acc' : acc)
          else go stack (Just ch) rest (Text.singleton ch : acc)
        | ch == '\\' -> case Text.uncons rest of
          Nothing -> go stack (Just ch) "" (Text.singleton ch : acc)
          Just ( next, rest' ) -> go stack (Just next) rest' (Text.singleton next : acc)
        | Text.isPrefixOf "**" remaining -> let
            stack' = toggle StyleBold stack
            acc'   = transition stack'
          in 
            go stack' Nothing (Text.drop 2 remaining) (acc' : acc)
        | Text.isPrefixOf "__" remaining -> let
            stack' = toggle StyleBold stack
            acc'   = transition stack'
          in 
            go stack' Nothing (Text.drop 2 remaining) (acc' : acc)
        | ch == '*' -> if shouldToggleSingle prev (peek rest) '*'
          then let
              stack' = toggle StyleItalic stack
              acc'   = transition stack'
            in 
              go stack' Nothing rest (acc' : acc)
          else go stack (Just ch) rest (Text.singleton ch : acc)
        | ch == '_' -> if shouldToggleSingle prev (peek rest) '_'
          then let
              stack' = toggle StyleItalic stack
              acc'   = transition stack'
            in 
              go stack' Nothing rest (acc' : acc)
          else go stack (Just ch) rest (Text.singleton ch : acc)
        | ch == '`' -> let
            stack' = toggle StyleCode stack
            acc'   = transition stack'
          in 
            go stack' Nothing rest (acc' : acc)
        | otherwise -> go stack (Just ch) rest (Text.singleton ch : acc)

    peek = fmap fst . Text.uncons

    isInCode st = StyleCode `elem` st

    toggle style st = case st of
      top : rest
        | top == style -> rest
      _          -> style : st

    transition st = reset <> applyStack st

    applyStack st = sgrText (concatMap styleToSGR (reverse st))

    styleToSGR StyleBold   = [ SetConsoleIntensity BoldIntensity ]
    styleToSGR StyleItalic = [ SetItalicized True ]
    styleToSGR StyleCode   = [ SetColor Foreground Vivid Yellow ]

    shouldToggleSingle prev next marker
      | marker == '_' && isAlphaNumChar prev && isAlphaNumChar next = False
      | otherwise = True

    isAlphaNumChar = maybe False isAlphaNum

reset :: Text
reset = sgrText [ Reset ]

applyLineStyle :: [ SGR ] -> Text -> Text
applyLineStyle styles content
  = let
      prefix = sgrText styles
    in 
      prefix <> content <> reset

headingStyle :: Int -> [ SGR ]
headingStyle _ = [ SetConsoleIntensity BoldIntensity ]

quoteStyle :: [ SGR ]
quoteStyle = [ SetColor Foreground Dull Cyan ]

codeStyle :: [ SGR ]
codeStyle = [ SetColor Foreground Dull Yellow ]

sgrText :: [ SGR ] -> Text
sgrText = Text.pack . setSGRCode
