# managed

This library contains the `Managed` monad, which is a small building block for
wrapping resources that you acquire in an exception-safe way using a callback.

The `Managed` type is really simple::

```haskell
newtype Managed a = Managed { with :: forall r . (a -> IO r) -> IO r }
```

... and it's a special case of two other monads:

```
Managed a = Codensity IO a = forall r . ContT r IO a
```

The main reason for defining a separate type is to simplify inferred types and
to provide additional type class instances.  Also, the `Managed` monad has a
less intimidating name so I feel more comfortable using it to teach beginners.

The most useful feature of `Managed` is that it automatically lifts the `Monoid`
and `Num` type classes.  All `Applicative`s can auto-lift these two type classes
and by chaining `Applicative`s you can extend types with new functionality while
still preserving their `Monoid` and `Num` operations.

This type was popularized by the `mvc` library, and I received several requests
to split this type out into a small and separate library.

## Quick Example

Install [`stack`](http://haskellstack.org/) and run:

    $ stack install managed pipes

Then compile and run the following small program which copies `"inFile.txt"` to
`"outFile.txt"`:

```haskell
import Control.Monad.Managed
import System.IO
import Pipes
import qualified Pipes.Prelude as Pipes

main = runManaged $ do
    hIn  <- managed (withFile "inFile.txt" ReadMode)
    hOut <- managed (withFile "outFile.txt" WriteMode)
    liftIO $ runEffect $ Pipes.fromHandle hIn >-> Pipes.toHandle hOut
```

```bash
$ stack ghc -- -O2 example.hs
$ cat inFile.txt
ABC
$ ./example
$ cat outFile.txt
ABC
```

Read the documentation in the `Control.Monad.Managed` module to learn more about
how to use the `Managed` type.

## Development Status

The API is mostly stable.  I might add a few utility functions later on that
wrap `withXXX` functions from `base` in the `Managed` monad, but for now I'm
waiting for people to reach a decision on split-`base` before adding these.

## How to contribute

* Use the `Managed` type in your own library

* Write tutorials explaining how to use this library

## License (BSD 3-clause)

Copyright (c) 2014 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
