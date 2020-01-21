{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

{-| An example Haskell program to copy data from one handle to another might
    look like this:

> main =
>     withFile "inFile.txt" ReadMode $ \inHandle ->
>         withFile "outFile.txt" WriteMode $ \outHandle ->
>             copy inHandle outHandle
>
> -- A hypothetical function that copies data from one handle to another
> copy :: Handle -> Handle -> IO ()

    `System.IO.withFile` is one of many functions that acquire some resource in
    an exception-safe way.  These functions take a callback function as an
    argument and they invoke the callback on the resource when it becomes
    available, guaranteeing that the resource is properly disposed if the
    callback throws an exception.

    These functions usually have a type that ends with the following pattern:

>                    Callback
> --                -----------
> withXXX :: ... -> (a -> IO r) -> IO r

    Here are some examples of this pattern from the @base@ libraries:

> withArray      :: Storable a => [a] -> (Ptr a   -> IO r) -> IO r
> withBuffer     ::          Buffer e -> (Ptr e   -> IO r) -> IO r
> withCAString   ::            String -> (CString -> IO r) -> IO r
> withForeignPtr ::      ForeignPtr a -> (Ptr a   -> IO r) -> IO r
> withMVar       ::            Mvar a -> (a       -> IO r) -> IO r
> withPool       ::                      (Pool    -> IO r) -> IO r

    Acquiring multiple resources in this way requires nesting callbacks.
    However, you can wrap anything of the form @((a -> IO r) -> IO r)@ in the
    `Managed` monad, which translates binds to callbacks for you:

> import Control.Monad.Managed
> import System.IO
>
> inFile :: FilePath -> Managed Handle
> inFile filePath = managed (withFile filePath ReadMode)
>
> outFile :: FilePath -> Managed Handle
> outFile filePath = managed (withFile filePath WriteMode)
>
> main = runManaged $ do
>     inHandle  <- inFile "inFile.txt"
>     outHandle <- outFile "outFile.txt"
>     liftIO (copy inHandle outHandle)

    ... or you can just wrap things inline:

> main = runManaged $ do
>     inHandle  <- managed (withFile "inFile.txt" ReadMode)
>     outHandle <- managed (withFile "outFile.txt" WriteMode)
>     liftIO (copy inHandle outHandle)

    Additionally, since `Managed` is a `Monad`, you can take advantage of all
    your favorite combinators from "Control.Monad".  For example, the
    `Foreign.Marshal.Utils.withMany` function from "Foreign.Marshal.Utils"
    becomes a trivial wrapper around `mapM`:

> withMany :: (a -> (b -> IO r) -> IO r) -> [a] -> ([b] -> IO r) -> IO r
> withMany f = with . mapM (Managed . f)

    Another reason to use `Managed` is that if you wrap a `Monoid` value in
    `Managed` you get back a new `Monoid`:

> instance Monoid a => Monoid (Managed a)

    This lets you combine managed resources transparently.  You can also lift
    operations from some numeric type classes this way, too, such as the `Num`
    type class.

    NOTE: `Managed` may leak space if used in an infinite loop like this
    example:

> import Control.Monad
> import Control.Monad.Managed
>
> main = runManaged (forever (liftIO (print 1)))

    If you need to acquire a resource for a long-lived loop, you can instead
    acquire the resource first and run the loop in `IO`, using either of the
    following two equivalent idioms:

> with resource (\r -> forever (useThe r))
>
> do r <- resource
>    liftIO (forever (useThe r))
-}

module Control.Monad.Managed (
    -- * Managed
    Managed,
    MonadManaged(..),
    managed,
    managed_,
    with,
    runManaged,

    -- * Re-exports
    -- $reexports
    module Control.Monad.IO.Class
    ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail as MonadFail (MonadFail(..))
#endif
import Control.Monad.Trans.Class (lift)

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (liftA2)
#else
import Control.Applicative
import Data.Monoid (Monoid(..))
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

import qualified Control.Monad.Trans.Cont          as Cont
#if MIN_VERSION_transformers(0,4,0)
import qualified Control.Monad.Trans.Except        as Except
#endif
import qualified Control.Monad.Trans.Identity      as Identity
import qualified Control.Monad.Trans.Maybe         as Maybe
import qualified Control.Monad.Trans.Reader        as Reader
import qualified Control.Monad.Trans.RWS.Lazy      as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict    as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy    as State.Lazy
import qualified Control.Monad.Trans.State.Strict  as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict

-- | A managed resource that you acquire using `with`
newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r }

instance Functor Managed where
    fmap f mx = Managed (\return_ ->
        mx >>- \x ->
        return_ (f x) )

instance Applicative Managed where
    pure r    = Managed (\return_ ->
        return_ r )

    mf <*> mx = Managed (\return_ ->
        mf >>- \f ->
        mx >>- \x ->
        return_ (f x) )

instance Monad Managed where
    return r = Managed (\return_ ->
        return_ r )

    ma >>= f = Managed (\return_ ->
        ma  >>- \a ->
        f a >>- \b ->
        return_ b )

instance MonadIO Managed where
    liftIO m = Managed (\return_ -> do
        a <- m
        return_ a )

#if MIN_VERSION_base(4,9,0)
instance MonadFail Managed where
    fail s = Managed (\return_ -> do
        a <- MonadFail.fail s
        return_ a )
#endif

instance Semigroup a => Semigroup (Managed a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Managed a) where
    mempty = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = liftA2 mappend
#endif

instance Num a => Num (Managed a) where
    fromInteger = pure . fromInteger
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional a => Fractional (Managed a) where
    fromRational = pure . fromRational
    recip = fmap recip
    (/) = liftA2 (/)

instance Floating a => Floating (Managed a) where
    pi = pure pi
    exp   = fmap exp
    sqrt  = fmap sqrt
    log   = fmap log
    sin   = fmap sin
    tan   = fmap tan
    cos   = fmap cos
    asin  = fmap sin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    tanh  = fmap tanh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh
    (**)    = liftA2 (**)
    logBase = liftA2 logBase

{-| You can embed a `Managed` action within any `Monad` that implements
    `MonadManaged` by using the `using` function

    All instances must obey the following two laws:

> using (return x) = return x
>
> using (m >>= f) = using m >>= \x -> using (f x)
-}
class MonadIO m => MonadManaged m where
    using :: Managed a -> m a

instance MonadManaged Managed where
    using = id

instance MonadManaged m => MonadManaged (Cont.ContT r m) where
    using m = lift (using m)

#if MIN_VERSION_transformers(0,4,0)
instance MonadManaged m => MonadManaged (Except.ExceptT e m) where
    using m = lift (using m)
#endif

instance MonadManaged m => MonadManaged (Identity.IdentityT m) where
    using m = lift (using m)

instance MonadManaged m => MonadManaged (Maybe.MaybeT m) where
    using m = lift (using m)

instance MonadManaged m => MonadManaged (Reader.ReaderT r m) where
    using m = lift (using m)

instance (Monoid w, MonadManaged m) => MonadManaged (RWS.Lazy.RWST r w s m) where
    using m = lift (using m)

instance (Monoid w, MonadManaged m) => MonadManaged (RWS.Strict.RWST r w s m) where
    using m = lift (using m)

instance MonadManaged m => MonadManaged (State.Strict.StateT s m) where
    using m = lift (using m)

instance MonadManaged m => MonadManaged (State.Lazy.StateT s m) where
    using m = lift (using m)

instance (Monoid w, MonadManaged m) => MonadManaged (Writer.Strict.WriterT w m) where
    using m = lift (using m)

instance (Monoid w, MonadManaged m) => MonadManaged (Writer.Lazy.WriterT w m) where
    using m = lift (using m)

-- | Build a `Managed` value
managed :: (forall r . (a -> IO r) -> IO r) -> Managed a
managed = Managed

-- | Like 'managed' but for resource-less operations.
managed_ :: (forall r. IO r -> IO r) -> Managed ()
managed_ f = managed $ \g -> f $ g ()

{-| Acquire a `Managed` value

    This is a potentially unsafe function since it allows a resource to escape
    its scope.  For example, you might use `Managed` to safely acquire a
    file handle, like this:

> import qualified System.IO as IO
>
> example :: Managed Handle
> example = managed (IO.withFile "foo.txt" IO.ReadMode)

    ... and if you never used the `with` function then you would never run the
    risk of accessing the `Handle` after the file was closed.  However, if you
    use `with` then you can incorrectly access the handle after the handle is
    closed, like this:

> bad :: IO ()
> bad = do
>     handle <- with example return
>     IO.hPutStrLn handle "bar"  -- This will fail because the handle is closed

    ... so only use `with` if you know what you are doing and you're returning
    a value that is not a resource being managed.
-}
with :: Managed a -> (a -> IO r) -> IO r
with = (>>-)

-- | Run a `Managed` computation, enforcing that no acquired resources leak
runManaged :: Managed () -> IO ()
runManaged m = m >>- return

{- $reexports
    "Control.Monad.IO.Class" re-exports 'MonadIO'
-}
