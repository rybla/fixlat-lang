module Effect.File where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

newtype FilePath
  = FilePath 
      { base :: String
      , end :: String
      }

iRef :: Ref Int
iRef = unsafePerformEffect $ Ref.new 0

writeFile :: forall m. MonadEffect m => String -> FilePath -> m Unit
writeFile content (FilePath fp) = do
  i <- liftEffect $ Ref.modify (1 + _) iRef
  let _ = _writeFile content (fp.base <> show (i - 1) <> "." <> fp.end)
  pure unit

foreign import _writeFile :: String -> String -> Void
