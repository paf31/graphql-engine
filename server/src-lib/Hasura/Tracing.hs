module Hasura.Tracing
  ( MonadTrace
  , nullTracer
  , askTracer
  , tracerMiddleware
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Char8 as BS8
import qualified Network.Wai           as Wai

-- | A tracer traces the execution of blocks of code,
-- providing a label and additional metadata.
newtype Tracer = Tracer
  { runTracer :: forall a
               . String
              -- ^ human-readable name for this block of code
              -> [(String, String)]
              -- ^ additional human-readable key-value pairs
              -> IO a
              -> IO a
  }

-- | A tracer which does nothing.
nullTracer :: Tracer
nullTracer = Tracer \_ _ io -> io

-- | A type class for monads which support some
-- way to trace execution.
class Monad m => MonadTrace m where
  -- | Get the current tracer
  askTracer :: m Tracer
  
  default askTracer :: m Tracer
  askTracer = return nullTracer

-- | Add tracing to a WAI application using the specified 'Tracer'.
tracerMiddleware :: Tracer -> Wai.Middleware
tracerMiddleware tracer app req k = do
  app req \res -> do
    let name = BS8.unpack (Wai.rawPathInfo req)
    runTracer tracer name [] $ k res
