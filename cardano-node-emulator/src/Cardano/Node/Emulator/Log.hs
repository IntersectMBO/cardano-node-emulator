{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Emulator.Log
  ( -- $log
    LogMsg (..),
    LogLevel (..),
    LogMessage (..),
    logLevel,
    logMessageContent,
    logMessage,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * Modifying logs
    mapLog,
    mapMLog,

    -- * Running logs
    handleLogIgnore,
    handleLogTrace,
    handleLogWriter,
    renderLogMessages,

    -- * Observing
    LogObserve (..),
    ObservationHandle,
    Observation (..),
    observeBefore,
    observeAfter,

    -- * Combinators
    surround,
    surroundDebug,
    surroundInfo,
    surroundWarn,

    -- ** Handlers
    handleObserveLog,
    handleObserve,
  )
where

import Control.Lens (AReview, Prism', makeLenses, prism', review)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Text (Text)
import Debug.Trace qualified as Trace
import GHC.Generics (Generic)
import Polysemy (Effect, InterpreterFor, Member, Sem, insertAt, interpret, makeSem, raise, reinterpret)
import Polysemy.State (State, get, put, runState)
import Polysemy.Writer (Writer, tell)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, hang, layoutPretty, (<+>))
import Prettyprinter.Render.String qualified as Render
import Prettyprinter.Render.Text qualified as Render

-- | The severity level of a log message
--  See https://en.wikipedia.org/wiki/Syslog#Severity_level
data LogLevel
  = Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty LogLevel where
  pretty = \case
    Debug -> "[DEBUG]"
    Info -> "[INFO]"
    Notice -> "[NOTICE]"
    Warning -> "[WARNING]"
    Error -> "[ERROR]"
    Critical -> "[CRITICAL]"
    Alert -> "[ALERT]"
    Emergency -> "[EMERGENCY]"

data LogMessage a = LogMessage {_logLevel :: LogLevel, _logMessageContent :: a}
  deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON, FromJSON)

makeLenses ''LogMessage

logMessage :: LogLevel -> Prism' (LogMessage a) a
logMessage lvl =
  prism'
    (LogMessage lvl)
    ( \case
        LogMessage lvl' a | lvl' == lvl -> Just a
        _ -> Nothing
    )

instance (Pretty a) => Pretty (LogMessage a) where
  pretty LogMessage {_logLevel, _logMessageContent} =
    pretty _logLevel <+> hang 0 (pretty _logMessageContent)

data LogMsg a :: Effect where
  LogMsg :: LogMessage a -> LogMsg a m ()

makeSem ''LogMsg

logDebug :: forall a effs. (Member (LogMsg a) effs) => a -> Sem effs ()
logDebug = logMsg . LogMessage Debug

logWarn :: forall a effs. (Member (LogMsg a) effs) => a -> Sem effs ()
logWarn = logMsg . LogMessage Warning

logInfo :: forall a effs. (Member (LogMsg a) effs) => a -> Sem effs ()
logInfo = logMsg . LogMessage Info

logError :: forall a effs. (Member (LogMsg a) effs) => a -> Sem effs ()
logError = logMsg . LogMessage Error

-- | Re-interpret a logging effect by effectfully mapping the log messages.
mapMLog ::
  forall a b effs.
  (Member (LogMsg b) effs) =>
  (a -> Sem effs b) ->
  InterpreterFor (LogMsg a) effs
mapMLog f = interpret $ \(LogMsg msg) -> traverse f msg >>= logMsg

-- | Re-interpret a logging effect by purely mapping the log messages.
mapLog ::
  forall a b effs.
  (Member (LogMsg b) effs) =>
  (a -> b) ->
  InterpreterFor (LogMsg a) effs
mapLog = mapMLog . (return .)

-- | Pretty-print the log messages
renderLogMessages ::
  forall a effs.
  (Member (LogMsg Text) effs, Pretty a) =>
  InterpreterFor (LogMsg a) effs
renderLogMessages =
  mapLog (Render.renderStrict . layoutPretty defaultLayoutOptions . pretty)

-- | Re-interpret a 'Log' effect with a 'Writer'
handleLogWriter ::
  forall a w effs.
  (Member (Writer w) effs) =>
  AReview w (LogMessage a) ->
  InterpreterFor (LogMsg a) effs
handleLogWriter p = interpret $ \(LogMsg msg) -> tell (review p msg)

-- | Ignore all log messages.
handleLogIgnore ::
  forall a effs.
  InterpreterFor (LogMsg a) effs
handleLogIgnore = interpret $ \(LogMsg _) -> pure ()

-- | Write the log to stdout using 'Debug.Trace.trace'
handleLogTrace ::
  forall a effs.
  (Pretty a) =>
  InterpreterFor (LogMsg a) effs
handleLogTrace = interpret $ \(LogMsg msg) ->
  Trace.trace
    (Render.renderString . layoutPretty defaultLayoutOptions . pretty $ msg)
    (pure ())

-- | An abstract type used to tie the beginning and end of observations
--  together.
newtype ObservationHandle = ObservationHandle Integer

data LogObserve a :: Effect where
  ObserveBefore :: a -> LogObserve a m ObservationHandle
  ObserveAfter :: Maybe a -> ObservationHandle -> LogObserve a m ()

makeSem ''LogObserve

-- | Write a log message before and after an action. Consider using
--  'observeBefore' and 'observeAfter' directly if you need more control
--  over the values that are observed at the call site.
surround ::
  forall v a effs.
  (Member (LogObserve v) effs) =>
  v ->
  Sem effs a ->
  Sem effs a
surround v action = do
  i <- observeBefore v
  result <- action
  observeAfter Nothing i
  pure result

-- | @surroundInfo = surround Info@
surroundInfo :: (Member (LogObserve (LogMessage v)) effs) => v -> Sem effs a -> Sem effs a
surroundInfo = surround . LogMessage Info

-- | @surroundDebug = surround Debug@
surroundDebug :: (Member (LogObserve (LogMessage v)) effs) => v -> Sem effs a -> Sem effs a
surroundDebug = surround . LogMessage Debug

-- | @surroundWarn = surround Warn@
surroundWarn :: (Member (LogObserve (LogMessage v)) effs) => v -> Sem effs a -> Sem effs a
surroundWarn = surround . LogMessage Warning

-- | How did the observed action end
data ExitMode
  = -- | The action was run to completion
    Regular
  | -- | Execution of the observed action was cut short. This can happen if you use 'LogObserve' in combination with 'Error', 'NonDet', 'Prompt' or similar effects.
    Irregular
  deriving (Eq, Ord, Show)

-- | An observation with measurements before and after running an action.
data Observation v s = Observation
  { -- | Call-site information about the start of the observation
    obsLabelStart :: v,
    -- | Measurement taken before running the action
    obsStart :: s,
    -- | Call-site information about the end of the observation
    obsLabelEnd :: Maybe v,
    -- | 'ExitMode' of the action.
    obsExit :: ExitMode
  }

--  | An 'Observation' that doesn't have an 'obsEnd' value yet.
data PartialObservation v s = PartialObservation
  { obsMsg :: v,
    obsValue :: s,
    obsDepth :: Integer
  }

-- | State of partial observations
data ObsState v s = ObsState
  { obsMaxDepth :: Integer,
    obsPartials :: [PartialObservation v s]
  }

initialState :: ObsState v s
initialState = ObsState 0 []

-- | Handle the 'LogObserve' effect by recording observations
--  @s@ before and after the observed action, and turning
--  them into 'LogMessage (Observation s)' values.
handleObserve ::
  forall v s effs.
  -- | How to get the current 's'
  (v -> Sem effs s) ->
  -- | What to do with the observation
  (Observation v s -> Sem effs ()) ->
  InterpreterFor (LogObserve v) effs
handleObserve getCurrent handleObs =
  handleFinalState
    . runState initialState
    . handler
    . insertAt @1 @'[State (ObsState v s)]
  where
    -- empty the stack of partial observations at the very end.
    handleFinalState :: forall a. Sem effs (ObsState v s, a) -> Sem effs a
    handleFinalState action = do
      (finalState, result) <- action
      _ <- handleObserveAfter Nothing finalState 0
      pure result

    -- when an action with the given depth is finished, take the final
    -- measurement and clear the stack of partial observations.
    handleObserveAfter :: Maybe v -> ObsState v s -> Integer -> Sem effs (ObsState v s)
    handleObserveAfter v' ObsState {obsPartials} i = do
      let (finishedPartials, remainingPartials) = span ((<=) i . obsDepth) obsPartials
      for_ finishedPartials $ \PartialObservation {obsMsg, obsValue, obsDepth} -> do
        -- we assume that a 'PartialObservation' was completed
        -- regularly if it is handled at its own depth level.
        -- If the @obsDepth@ is greater than @i@ then one or more
        -- 'LObserveAfter' calls were skipped, which we note with
        -- 'Irregular'.
        let exitMode = if obsDepth == i then Regular else Irregular
            message =
              Observation
                { obsLabelStart = obsMsg,
                  obsStart = obsValue,
                  obsExit = exitMode,
                  obsLabelEnd = case exitMode of Regular -> v'; Irregular -> Nothing
                }
        handleObs message
      pure ObsState {obsMaxDepth = i - 1, obsPartials = remainingPartials}

    handleObserveBefore :: v -> ObsState v s -> Sem effs (ObsState v s, ObservationHandle)
    handleObserveBefore v ObsState {obsPartials, obsMaxDepth} = do
      current <- getCurrent v
      let newMaxDepth = obsMaxDepth + 1
          msg =
            PartialObservation
              { obsMsg = v,
                obsValue = current,
                obsDepth = newMaxDepth
              }
          newState = ObsState {obsMaxDepth = newMaxDepth, obsPartials = msg : obsPartials}
      pure (newState, ObservationHandle newMaxDepth)

    handler :: InterpreterFor (LogObserve v) (State (ObsState v s) : effs)
    handler = interpret $ \case
      ObserveBefore vl -> do
        currentState <- get @(ObsState v s)
        (newState, handle) <- raise (handleObserveBefore vl currentState)
        put newState
        pure handle
      ObserveAfter v' (ObservationHandle i) -> do
        currentState <- get @(ObsState v s)
        newState <- raise (handleObserveAfter v' currentState i)
        put newState

-- | Interpret the 'LogObserve' effect by logging a "start" message
--  before the action and an "end" message after the action.
handleObserveLog ::
  forall effs.
  (Member (LogMsg Text) effs) =>
  InterpreterFor (LogObserve (LogMessage Text)) effs
handleObserveLog =
  handleObserve (\_ -> pure ()) handleAfter
    . reinterpret handleBefore
  where
    handleBefore ::
      forall c rInitial.
      LogObserve (LogMessage Text) (Sem rInitial) c ->
      Sem (LogObserve (LogMessage Text) ': effs) c
    handleBefore = \case
      ObserveBefore msg -> do
        let msg' = fmap (<> " start") msg
        logMsg msg'
        observeBefore msg
      ObserveAfter v' i -> observeAfter v' i
    handleAfter Observation {obsLabelStart, obsExit} =
      logMsg $
        obsLabelStart
          <&> ( <>
                  case obsExit of
                    Regular -> " end"
                    Irregular -> " end (irregular)"
              )
