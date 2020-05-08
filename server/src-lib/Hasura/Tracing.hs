{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Tracing
  ( MonadTrace(..)
  , TraceT
  , runTraceT
  , TraceContext(..)
  , HasReporter(..)
  , TracingMetadata
  , SuspendedRequest(..)
  , traceHttpRequest
  ) where

import           Hasura.Prelude

import           Control.Monad.Trans.Control
import           Data.String

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Network.HTTP.Client         as HTTP
import qualified System.Random               as Rand

-- | Any additional human-readable key-value pairs relevant
-- to the execution of a block of code.
type TracingMetadata = [(String, String)]

-- | A type class for monads which support some
-- way to report execution traces.
class Monad m => HasReporter m where
  -- | Get the current tracer
  report :: TraceContext
         -- ^ the current trace context
         -> String
         -- ^ human-readable name for this block of code
         -> m (a, TracingMetadata)
         -- ^ the action whose execution we want to report, returning 
         -- any metadata emitted
         -> m a

  default report :: TraceContext -> String -> m (a, TracingMetadata) -> m a
  report _ name = fmap fst

instance HasReporter m => HasReporter (ReaderT r m) where
  report ctx name = mapReaderT $ report ctx name

instance HasReporter m => HasReporter (ExceptT e m) where
  report ctx name (ExceptT ema) = ExceptT $ report ctx name $ ema >>= \case
    Left err -> pure (Left err, mempty)
    Right (a, meta) -> pure (Right a, meta)

-- | A trace context records the current active trace,
-- the active span within that trace, and the span's parent,
-- unless the current span is the root.
data TraceContext = TraceContext
  { tcCurrentTrace  :: Word64
  , tcCurrentSpan   :: Word64
  , tcCurrentParent :: Maybe Word64
  }

-- | The 'TraceT' monad transformer adds the ability to keep track of
-- the current trace context.
newtype TraceT m a = TraceT { unTraceT :: ReaderT TraceContext (WriterT TracingMetadata m) a }
  deriving (Functor, Applicative, Monad, MonadIO)
  
instance MonadTrans TraceT where
  lift = TraceT . lift . lift

deriving instance MonadBase b m => MonadBase b (TraceT m)
deriving instance MonadBaseControl b m => MonadBaseControl b (TraceT m)

instance MonadError e m => MonadError e (TraceT m) where
  throwError = lift . throwError
  catchError (TraceT m) f = TraceT (catchError m (unTraceT . f))
  
instance MonadReader r m => MonadReader r (TraceT m) where
  ask = TraceT $ lift ask
  local f m = TraceT $ mapReaderT (local f) (unTraceT m)

-- | Run an action in the 'TraceT' monad transformer.
-- 'runTraceT' delimits a new trace with its root span, and the arguments
-- specify a name and metadata for that span.
runTraceT :: (HasReporter m, MonadIO m) => String -> TraceT m a -> m a
runTraceT name tma = do
  ctx <- TraceContext 
           <$> liftIO Rand.randomIO 
           <*> liftIO Rand.randomIO
           <*> pure Nothing
  report ctx name $ runWriterT $ runReaderT (unTraceT tma) ctx
  
-- | Monads which support tracing. 'TraceT' is the standard example.
class Monad m => MonadTrace m where
  -- | Trace the execution of a block of code, attaching a human-readable name.
  trace :: String -> m a -> m a
  
  -- | Ask for the current tracing context, so that we can provide it to any
  -- downstream services, e.g. in HTTP headers.
  currentContext :: m TraceContext
  
  -- | Log some metadata to be attached to the current span
  attachMetadata :: TracingMetadata -> m ()
   
-- | If the underlying monad can report trace data, then 'TraceT' will 
-- collect it and hand it off to that reporter.
instance (HasReporter m, MonadIO m) => MonadTrace (TraceT m) where
  trace name ma = TraceT . ReaderT $ \ctx -> do
    spanId <- liftIO (Rand.randomIO :: IO Word64)
    let subCtx = ctx { tcCurrentSpan = spanId 
                     , tcCurrentParent = Just (tcCurrentSpan ctx)
                     }
    lift . report ctx name . runWriterT $ runReaderT (unTraceT ma) subCtx
    
  currentContext = TraceT ask
  
  attachMetadata = TraceT . tell

instance MonadTrace m => MonadTrace (ReaderT r m) where
  trace = mapReaderT . trace
  currentContext = lift currentContext
  attachMetadata = lift . attachMetadata

instance MonadTrace m => MonadTrace (ExceptT e m) where
  trace = mapExceptT . trace
  currentContext = lift currentContext
  attachMetadata = lift . attachMetadata

-- | A HTTP request, which can be modified before execution.
data SuspendedRequest m a = SuspendedRequest HTTP.Request (HTTP.Request -> m a)

traceHttpRequest 
  :: MonadTrace m 
  => String 
  -- ^ human-readable name for this block of code 
  -> m (SuspendedRequest m a)
  -- ^ an action which yields the request about to be executed and suspends
  -- before actually executing it
  -> m a  
traceHttpRequest name f = trace name do
  SuspendedRequest req next <- f
  case HTTP.requestBody req of
    HTTP.RequestBodyBS bs ->
      attachMetadata [("request_body_bytes", show (BS.length bs))]
    HTTP.RequestBodyLBS bs ->
      attachMetadata [("request_body_bytes", show (BL.length bs))]
    HTTP.RequestBodyBuilder len _ -> 
      attachMetadata [("request_body_bytes", show len)]
    HTTP.RequestBodyStream len _ ->
      attachMetadata [("request_body_bytes", show len)]
    _ -> pure ()
  TraceContext{..} <- currentContext
  let tracingHeaders = 
        [ ("X-Hasura-TraceId", fromString (show tcCurrentTrace))
        , ("X-Hasura-SpanId", fromString (show tcCurrentSpan))
        ]
      req' = req { HTTP.requestHeaders = 
                     tracingHeaders <> HTTP.requestHeaders req
                 }
  next req'
  
  