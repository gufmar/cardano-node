{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Configuration
  ( configureTracers
  , withNamespaceConfig
  , filterSeverityFromConfig
  , filterPrivacyFromConfig
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Katip (Severity)

import           Cardano.Logging.Trace (filterTraceByPrivacy,
                     filterTraceBySeverity)
import           Cardano.Logging.Types

-- | Call this function at initialisation, and later for reconfiguration
configureTracers :: Monad m => TraceConfig -> [Trace m a] -> m ()
configureTracers config tracers = do
    mapM_ (configureTrace Reset) tracers
    mapM_ (configureTrace (Config config)) tracers
    mapM_ (configureTrace Optimize) tracers
  where
    configureTrace :: Monad m => TraceControl -> Trace m a -> m ()
    configureTrace c tr = T.traceWith tr (emptyLoggingContext, Left c)

-- | Take a selector function, and a function from trace to trace with
--   this selector to make a trace transformer with a config value
withNamespaceConfig :: (MonadIO m, Eq b) =>
     (TraceConfig -> Namespace -> b)
  -> (Maybe b -> Trace m a -> Trace m a)
  -> Trace m a
  -> m (Trace m a)
withNamespaceConfig extract needsConfigFunc tr = do
    ref  <- liftIO (newIORef (Left Map.empty))
    pure $ T.arrow $ T.emit $ mkTrace ref
  where
    mkTrace ref (lc, Right a) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Right val -> T.traceWith (needsConfigFunc (Just val) tr) (lc, Right a)
        Left map -> case Map.lookup (lcContext lc) map of
                      Just val -> T.traceWith
                                    (needsConfigFunc (Just val) tr)
                                    (lc, Right a)
                      Nothing  -> error $ "Unconfigured trace with context "
                                        ++ show (lcContext lc)
    mkTrace ref (lc, Left Reset) = do
      liftIO $ writeIORef ref (Left Map.empty)
      T.traceWith (needsConfigFunc Nothing tr) (lc, Left Reset)
    mkTrace ref (lc, Left (Config c)) = do
      let ! val = extract c (lcContext lc)
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left map ->
          case Map.lookup (lcContext lc) map of
            Nothing -> do
              liftIO $ writeIORef ref $ Left (Map.insert (lcContext lc) val map)
              T.traceWith (needsConfigFunc (Just val) tr) (lc, Left (Config c))
            Just v  -> do
              if v == val
                then T.traceWith (needsConfigFunc (Just val) tr) (lc, Left (Config c))
                else error $ "Inconsistent trace configuration with context "
                                  ++ show (lcContext lc)
        Right val -> error $ "Trace not reset before reconfiguration "
                            ++ show (lcContext lc)
    mkTrace ref (lc, Left Optimize) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left cmap ->
          case Map.size cmap of
            0 ->  -- This will never be called!?
                  pure ()
            1 -> do
                  case Map.elems cmap of
                    [val] -> do
                      liftIO $ writeIORef ref $ Right val
                      T.traceWith (needsConfigFunc (Just val) tr) (lc, Left Optimize)
                    _   -> error "Cardano.Logging.Configuration>>withConfig: Impossible"
            _ -> T.traceWith (needsConfigFunc Nothing tr) (lc, Left Optimize)
        Right val -> error $ "Trace not reset before reconfiguration "
                            ++ show (lcContext lc)

-- | Filter a trace by severity and take the filter value from the config
filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig = withNamespaceConfig getSeverity filterTraceBySeverity

-- | Filter a trace by severity and take the filter value from the config
filterPrivacyFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterPrivacyFromConfig = withNamespaceConfig getPrivacy filterTraceByPrivacy

--------------------------------------------------------
-- Internal

-- | If no severity can be found in the config, it is set to Warning
getSeverity :: TraceConfig -> Namespace -> SeverityF
getSeverity config context =
    fromMaybe WarningF (getOption severitySelector config context)
  where
    severitySelector :: ConfigOption -> Maybe SeverityF
    severitySelector (CoSeverity s) = Just s
    severitySelector _              = Nothing

-- | If no privacy can be found in the config, it is set to Public
getPrivacy :: TraceConfig -> Namespace -> Privacy
getPrivacy config context =
  fromMaybe Public (getOption privacySelector config context)
  where
    privacySelector :: ConfigOption -> Maybe Privacy
    privacySelector (CoPrivacy s) = Just s
    privacySelector _             = Nothing

-- | Searches in the config to find an option
getOption :: (ConfigOption -> Maybe a) -> TraceConfig -> Namespace -> Maybe a
getOption sel config [] =
  case Map.lookup [] (tcOptions config) of
    Nothing -> Nothing
    Just options -> case mapMaybe sel options of
                      []        -> Nothing
                      (opt : _) -> Just opt
getOption sel config context =
  case Map.lookup context (tcOptions config) of
    Nothing -> getOption sel config (tail context)
    Just options -> case mapMaybe sel options of
                      []        -> getOption sel config (init context)
                      (opt : _) -> Just opt