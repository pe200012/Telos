module Markdown.Stream ( StreamState, newStreamState, pushDelta, finalizeStream ) where

import           Data.Attoparsec.Text ( IResult(..)
                                      , Parser
                                      , Result
                                      , endOfLine
                                      , feed
                                      , isEndOfLine
                                      , parse
                                      , takeTill
                                      )
import qualified Data.Text            as Text

import           Markdown.RenderAnsi  ( RenderState, newRenderState, renderLineAnsi )

import           Relude

-- | StreamState uses attoparsec's incremental parsing (Partial/Done/Fail).
-- We keep _accumulatedText alongside the parser for fallback: if parsing fails
-- (malformed markdown from LLM), we output the accumulated text as-is.
data StreamState
  = StreamState
  { _lineResult :: Result Text, _accumulatedText :: Text, _renderState :: RenderState }

newStreamState :: StreamState
newStreamState
  = StreamState
  { _lineResult = parse lineParser "", _accumulatedText = "", _renderState = newRenderState }

lineParser :: Parser Text
lineParser = do
  line <- takeTill isEndOfLine
  endOfLine
  pure line

pushDelta :: StreamState -> Text -> ( StreamState, [ Text ] )
pushDelta streamState delta
  | Text.null delta = ( streamState, [] )
  | otherwise
    = let
        newAccumulated = _accumulatedText streamState <> delta
        res = feed (_lineResult streamState) delta
      in 
        drainResult res newAccumulated (_renderState streamState)

finalizeStream :: StreamState -> ( StreamState, [ Text ] )
finalizeStream streamState
  = let
      resEof = feed (_lineResult streamState) ""
    in 
      drainResultEof resEof (_accumulatedText streamState) (_renderState streamState)

-- | Drain lines from Result. On Done, output line and continue; on Partial, wait;
-- on Fail, output accumulated text as fallback (malformed markdown).
drainResult :: Result Text -> Text -> RenderState -> ( StreamState, [ Text ] )
drainResult result accumulated renderState = case result of
  Done rest line -> let
      ( rs', rendered ) = renderLineAnsi renderState line
      -- Continue parsing rest; remaining text goes into _accumulatedText
      ( moreLines, rs'', res', remaining ) = drainLinesFromRest rest rs'
    in 
      ( StreamState { _lineResult = res', _accumulatedText = remaining, _renderState = rs'' }
      , rendered <> moreLines
      )
  Partial _
    -> ( StreamState
         { _lineResult = result, _accumulatedText = accumulated, _renderState = renderState }
       , []
       )
  Fail {}        ->
    -- Malformed markdown: output accumulated text as-is, reset parser
    let
        ( rs', rendered ) = renderLineAnsi renderState accumulated
      in 
        ( StreamState
          { _lineResult = parse lineParser "", _accumulatedText = "", _renderState = rs' }
        , rendered
        )

-- | Drain all complete lines from rest (after a Done).
-- Returns (output lines, render state, result, remaining text for accumulation).
drainLinesFromRest :: Text -> RenderState -> ( [ Text ], RenderState, Result Text, Text )
drainLinesFromRest rest renderState
  = let
      res = parse lineParser rest
    in 
      case res of
        Done rest' line -> let
            ( rs', rendered ) = renderLineAnsi renderState line
            ( moreLines, rs'', res', remaining ) = drainLinesFromRest rest' rs'
          in 
            ( rendered <> moreLines, rs'', res', remaining )
        Partial _       -> ( [], renderState, res, rest )
        Fail {}         -> ( [], renderState, parse lineParser "", "" )

-- | Drain result at EOF. Force partial continuations and handle remaining text.
drainResultEof :: Result Text -> Text -> RenderState -> ( StreamState, [ Text ] )
drainResultEof result accumulated renderState = case result of
  Done rest line -> let
      ( rs', rendered ) = renderLineAnsi renderState line
      resEof = feed (parse lineParser rest) ""
      ( moreLines, rs'', res' ) = drainLinesAtEof resEof rest rs'
    in 
      ( StreamState { _lineResult = res', _accumulatedText = "", _renderState = rs'' }
      , rendered <> moreLines
      )
  Partial k      -> drainResultEof (k "") accumulated renderState
  Fail {}        ->
    -- Malformed: if accumulated is not empty, render it; otherwise nothing
    if Text.null accumulated
      then ( StreamState { _lineResult      = parse lineParser ""
                         , _accumulatedText = ""
                         , _renderState     = renderState
                         }
           , []
           )
      else let
          ( rs', rendered ) = renderLineAnsi renderState accumulated
        in 
          ( StreamState
            { _lineResult = parse lineParser "", _accumulatedText = "", _renderState = rs' }
          , rendered
          )

-- | Drain lines at EOF from rest.
drainLinesAtEof :: Result Text -> Text -> RenderState -> ( [ Text ], RenderState, Result Text )
drainLinesAtEof result rest renderState = case result of
  Done rest' line -> let
      ( rs', rendered ) = renderLineAnsi renderState line
      resEof = feed (parse lineParser rest') ""
      ( moreLines, rs'', res' ) = drainLinesAtEof resEof rest' rs'
    in 
      ( rendered <> moreLines, rs'', res' )
  Partial k       -> drainLinesAtEof (k "") rest renderState
  Fail {}         ->
    -- At EOF, if rest is not empty, render it as final line
    if Text.null rest
      then ( [], renderState, parse lineParser "" )
      else let
          ( rs', rendered ) = renderLineAnsi renderState rest
        in 
          ( rendered, rs', parse lineParser "" )
