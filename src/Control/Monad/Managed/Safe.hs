{-# LANGUAGE RankNTypes #-}

{-| This module is a safer subset of "Control.Monad.Managed" that only lets you
    unwrap the `Managed` type using `runManaged`.  This enforces that you never
    leak acquired resources from a `Managed` computation.

    In general, you should strive to propagate the `Managed` type as much as
    possible and use `runManaged` when you are done with acquired resources.
    However, there are legitimate circumstances where you want to return a value
    other than acquired resource from the bracketed computation, which requires
    using `Control.Monad.Managed.with`.

    This module is not the default because you can also use the `Managed` type
    for callback-based code that is completely unrelated to resources.
-}

module Control.Monad.Managed.Safe (
    -- * Managed
    Managed,
    managed,
    runManaged
    ) where

import Control.Monad.Managed (Managed, managed, runManaged)
