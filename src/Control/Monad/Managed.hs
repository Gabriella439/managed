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
> inFile filePath = Managed (withFile filePath ReadMode)
>
> outFile :: FilePath -> Managed Handle
> outFile filePath = Managed (withFile filePath WriteMode)
>
> main = runManaged $ do
>     inHandle  <- inFile "inFile.txt"
>     outHandle <- outFile "outFile.txt"
>     liftIO (copy inHandle outHandle)

    ... or you can just wrap things inline:

> main = runManaged $ do
>     inHandle  <- Managed (withFile "inFile.txt" ReadMode)
>     outHandle <- Managed (withFile "outFile.txt" WriteMode)
>     liftIO (copy inHandle outHandle)

    Additionally, since `Managed` is a `Monad`, you can take advantage of all
    your favorite combinators from "Control.Monad".  For example, the
    `Foreign.Marshal.Utils.withMany` function from "Foreign.Marshal.Utils"
    becomes a trivial wrapper around `mapM`:

> withMany :: (a -> (b -> IO r) -> IOr ) -> [a] -> ([b] -> IO r) -> IO r
> withMany f = with . mapM (Managed . f)

    Another reason to use `Managed` is that if you wrap a `Monoid` value in
    `Managed` you get back a new `Monoid`:

> instance Monoid a => Monoid (Managed a)

    This lets you combine managed resources transparently.  You can also lift
    operations from some numeric type classes this way, too, such as the `Num`
    type class.
-}

module Control.Monad.Managed (
    -- * Managed
    Managed,
    managed,
    with,
    runManaged
    ) where

import Control.Applicative (Applicative(pure, (<*>)), liftA2)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Monoid (Monoid(mempty, mappend))

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

instance Monoid a => Monoid (Managed a) where
    mempty = pure mempty

    mappend = liftA2 mappend

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

-- | Build a `Managed` value
managed :: (forall r . (a -> IO r) -> IO r) -> Managed a
managed = Managed

-- | Acquire a `Managed` value
with :: Managed a -> (a -> IO r) -> IO r
with = (>>-)

-- | Run a `Managed` computation, enforcing that no acquired resources leak
runManaged :: Managed () -> IO ()
runManaged m = m >>- return
