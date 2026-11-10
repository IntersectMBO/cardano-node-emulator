{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Monitoring.Util
  ( handleLogMsgTrace,
    handleLogMsgTraceMap,
    handleObserveTrace,
    runLogEffects,
    convertLog,
    toSeverity,
  )
where

import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Data.Counter
import Cardano.BM.Data.LogItem
import Cardano.BM.Data.Severity
import Cardano.BM.Data.SubTrace
import Cardano.BM.Data.Trace
import Cardano.BM.Observer.Monadic
import Cardano.BM.Trace
import Cardano.Node.Emulator.Log qualified as L
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (for_)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Polysemy (Embed, InterpreterFor, Member, Sem, insertAt, interpret)
import Polysemy.Embed (embed)
import Polysemy.Final (embedToFinal, runFinal)

toSeverity :: L.LogLevel -> Severity
toSeverity = \case
  L.Debug -> Debug
  L.Info -> Info
  L.Notice -> Notice
  L.Warning -> Warning
  L.Error -> Error
  L.Critical -> Critical
  L.Alert -> Alert
  L.Emergency -> Emergency

convertLog :: (a -> b) -> Trace m b -> Trace m a
convertLog = contramap . second . fmap

-- | Handle the 'LogMsg' effect by logging messages to a 'Trace'
handleLogMsgTrace ::
  forall a m effs.
  ( MonadIO m,
    Member (Embed m) effs
  ) =>
  Trace m a ->
  InterpreterFor (L.LogMsg a) effs
handleLogMsgTrace trace = interpret $
  \(L.LogMsg (L.LogMessage lvl content)) ->
    let defaultPrivacy = Public -- TODO: Configurable / add to 'LogMessage'?
     in embed $ traceNamedItem trace defaultPrivacy (toSeverity lvl) content

-- | Handle the 'LogMsg' effect by logging messages to a mapped 'Trace'
handleLogMsgTraceMap ::
  forall b a m effs.
  ( MonadIO m,
    Member (Embed m) effs
  ) =>
  (b -> a) ->
  Trace m a ->
  InterpreterFor (L.LogMsg b) effs
handleLogMsgTraceMap mapping = handleLogMsgTrace . convertLog mapping

runLogEffects ::
  forall m l a.
  (MonadIO m) =>
  Trace m l ->
  Sem '[L.LogMsg l, Embed m] a ->
  m a
runLogEffects trace =
  runFinal
    . embedToFinal
    . handleLogMsgTrace trace
    . insertAt @2

-- | Handle the 'LogObserve' effect using the 'Cardano.BM.Observer.Monadic'
--  observer functions
handleObserveTrace ::
  forall effs m a.
  ( Member (Embed m) effs,
    MonadIO m,
    MonadCatch m
  ) =>
  CM.Configuration ->
  Trace m a ->
  InterpreterFor (L.LogObserve (L.LogMessage Text)) effs
handleObserveTrace config t =
  -- We need to call 'observeOpen' and 'observeClose' with the appropriate
  -- arguments.
  --
  -- 'observeBefore' makes the call to 'observeOpen' and 'observeAfter'
  -- makes the call to 'observeClose.'
  let observeBefore :: L.LogMessage Text -> Sem effs (Maybe (SubTrace, CounterState))
      observeBefore L.LogMessage {L._logLevel, L._logMessageContent} = do
        -- find the correct subtrace using the logging config and the content
        -- of the message.
        subtrace <-
          fromMaybe Neutral <$> embed (liftIO $ CM.findSubTrace config _logMessageContent)

        -- 'observeOpen' produces the state of the counters at the beginning of
        -- the action. We return 'counterState' and 'subtrace' so that
        -- they are available in 'observeAfter'.
        mCountersid <- embed $ observeOpen subtrace (toSeverity _logLevel) t
        case mCountersid of
          Left _ -> pure Nothing
          Right counterState -> pure (Just (subtrace, counterState))

      observeAfter :: L.Observation (L.LogMessage Text) (Maybe (SubTrace, CounterState)) -> Sem effs ()
      observeAfter L.Observation {L.obsStart} =
        for_ obsStart $ \(subtrace, counterState) ->
          void $ embed $ observeClose subtrace Info t counterState []
   in L.handleObserve observeBefore observeAfter
