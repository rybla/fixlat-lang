module Bug where

import Prelude
import Pretty
import Partial.Unsafe (unsafeCrashWith)

throwBug :: forall a b. Pretty a => String -> a -> b
throwBug label msg =
  unsafeCrashWith <<< render
    $ "====[ begin: bug ]================================"
    \~ pretty label
    \~ ""
    \~ msg
    \~ "====[ end: bug ]================================"
