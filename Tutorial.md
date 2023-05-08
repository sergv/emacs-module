### Intro

Emacs module is a shared library object (`.so` on Linux, `.dll` on Windows). Since Emacs module API
is a C-based one, writing a module in Haskell will likely involve FFI.

If at any point this tutorial doesn’t work you could refer to tests
for this package (at https://github.com/sergv/emacs-module/tree/master/test)
that will contain all the pieces of creating shared library object with Haskell in
hopefully working order since they’re run on CI.

### Preparations

For starters, module will need to initialize Haskell RTS. This cannot be done from Haskell
so C will need to be used:

```c
#include <emacs-module.h>

#include <stdlib.h>
#include "HsFFI.h"
#include "Rts.h"

#ifdef __cplusplus
extern "C" {
#endif
extern HsBool initialise(struct emacs_runtime *ert);
#ifdef __cplusplus
}
#endif

int plugin_is_GPL_compatible = 1;

HsBool init(void) {
  int argc = 0;
  char *argv[] = { NULL };
  char **pargv = argv;

  // Initialise Haskell runtime, can pass RTS options to the module via argv.
  {
      RtsConfig conf = defaultRtsConfig;
      conf.rts_opts_enabled = RtsOptsAll;
      hs_init_ghc(&argc, &pargv, conf);
  }
  return HS_BOOL_TRUE;
}

void deinit(void) {
  hs_exit();
}

int
emacs_module_init(struct emacs_runtime *ert)
{
  return !(init() && initialise(ert));
}
```

The `plugin_is_GPL_compatible` is required by Emacs, without it will refuse to load your module.
Another mandatory bit is `emacs_module_init` function which will be called upon module load.
Here it will initialize Haskell’s RTS and call `initialise` which will be exported from Haskell.

It should be noted that `hs_init` could be called somewhere else. If multiple Haskell modules are
created as literally show in this tutorial they could not be used together because each of
them will call `hs_init` upon loading by Emacs but they’ll share single Haskell runtime for which
`hs_init` should be called only once. In that case the call to `hs_init` should be factored out,
but this won’t be shown in this tutorial.

I recommend building shared library with cabal. For than following
minimalistic cabal file should work (put in into `emacs_wrapper.c`):

```cabal
cabal-version: 3.0
name:
  emacs-module-example
version:
  0.1.0.0

build-type:
  Simple

common ghc-options
  default-language:
    GHC2021

  default-extensions:
    ImportQualifiedPost
    LambdaCase

  ghc-options:
    -Weverything
    -Wno-all-missed-specialisations
    -Wno-implicit-prelude
    -Wno-missed-specialisations
    -Wno-missing-import-lists
    -Wno-missing-local-signatures
    -Wno-missing-safe-haskell-mode
    -Wno-redundant-constraints
    -Wno-safe
    -Wno-type-defaults
    -Wno-unsafe

  if impl(ghc >= 8.8)
    ghc-options:
      -Wno-missing-deriving-strategies

  if impl(ghc >= 9.2)
    ghc-options:
      -Wno-missing-kind-signatures

foreign-library emacs-module-example
  import: ghc-options
  type:
    native-shared
  lib-version-info:
    0:0:0
  c-sources:
    cbits/emacs_wrapper.c
  includes:
    emacs-module.h
  install-includes:
    emacs-module.h
  include-dirs:
    cbits
  other-modules:
    Emacs.Example

  if os(Windows)
    options:
      standalone
    mod-def-file:
      emacs-module-example.def

  ghc-options:
    -threaded

  build-depends:
    , base >= 4.16 && <5
    , emacs-module >= 0.2
  hs-source-dirs:
    src
```

The `foreign-library` section will direct cabal to create shared library. For Windows users
it’s important to provide following def file that will specify exported names. Without it the DLL
will fail to link because GHC will export every symbol and hit a limit.

```
LIBRARY emacs-module-example
EXPORTS
  plugin_is_GPL_compatible @1
  emacs_module_init @2
```

Users on other systems can safely ignore the def file and remove corresponding part from cabal file.

### Haskell part

Put following source under `src/Emacs/Example.hs`:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Example () where

import Foreign
import Foreign.C

import Data.Emacs.Module.Args
import Data.Emacs.Module.Runtime (Runtime)
import Data.Emacs.Module.Runtime qualified as Runtime
import Emacs.Module
import Emacs.Module.Monad

foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true  = CBool 1
false = CBool 0

initialise :: Ptr Runtime -> IO CBool
initialise runtime = do
  runtime' <- Runtime.validateRuntime runtime
  case runtime' of
    Nothing        -> pure false
    Just runtime'' ->
      Runtime.withEnvironment runtime'' $ \env -> do
        res <- reportAllErrorsToEmacs env (pure False) $ runEmacsM env initialise'
        pure $ if res then true else false

initialise'
  :: MonadEmacs m v
  => m s Bool
initialise' = do
  bindFunction "haskell-emacs-module-example" =<<
    makeFunction example "Add arguments and call a function on the sum."
  pure True

example
  :: MonadEmacs m v
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
example (R f (R x (R y Stop))) = do
  z <- makeInt =<< (+) <$> extractInt x <*> extractInt y
  funcall f [z]
```

This will export the `initialise` symbol that we mentioned in `emacs_wrapper.c`. It will
follow Emacs module initialisation protocol of checking the size of the runtime struct that
Emacs passed in. The `emacs-module` package requires at least Emacs 28. For earlier Emacs versions
the runtime struct will be smaller that expected and thus the check will rule out loading under
older Emacs versions.

After the check it will call the `initialise'` function which gives names to Haskell implementations
in Emacs so that they can be called from elisp. The `makeFunction` creates an unammed function
object that will call specified Haskell function. The `bindFunction` takes function object
and associates it with a name in the elisp environment.

And finally, the `example` function that could be called from elisp. It needs to be of type
`EmacsFunction A B C m v s` that has quite a few parameters. Their meanings are:
- `A` - number of required arguments that the function will receive,
  specified as Peano numeral on typelevel. If elisp provides less
  arguments that required an error will be thrown without reaching Haskell.
- `B` - number of optional arguments that the function will receive,
  specified as Peano numeral on typelevel. If elisp provides less
  arguments that required the remaining optional arguments will be
  `nil`. Corresponds to `&optional` annotation in elisp.
- `C` - type-level boolean that denotes whether function accepts `&rest` arguments, similar to elisp.
- `m` - underlying monad, can be either kept abstract by using MTL-style `MonadEmacs` or can
  be concrete, in which case specify EmacsM.
- `v` - type of Emacs values the monad handles
- `s` - type-level threading marker similar to the one in the `ST` monad. Makes it impossible
  to share values between different runs of the Emacs interaction monad. This is because all
  the values created during particular run will be garbage-collected when Haskell returns control
  back to Emacs. This can be overcome by using `makeGlobalRef` function, please see package docs.

The `example` function receives 3 required arguments which are
pattern-matched as `(R f (R x (R y Stop)))`. It expects `x` and `y` to be integers which
it extracts via `extractInt` that takes an Emacs value and returns a Haskell `Int` value.
Sum of the `x` and `y` in converted back to Emacs value and fed into the `f` argument which
is expected to be a function.

The sample project can be build by `cabal build`. It will print where the shared library
was placed. On my system it’s `dist-newstyle/build/x86_64-linux/ghc-9.6.1/emacs-module-example-0.1.0.0/f/emacs-module-example/build/emacs-module-example/libemacs-module-example.so`. Now we can
direct Emacs to load example module and call the Haskell function as `(haskell-emacs-module-example (lambda (x) (* x x)) 10 20)`:

```
$ emacs --no-init -Q --batch -l dist-newstyle/build/x86_64-linux/ghc-9.6.1/emacs-module-example-0.1.0.0/f/emacs-module-example/build/emacs-module-example/libemacs-module-example.so --eval '(message "Result = %s" (haskell-emacs-module-example (lambda (x) (* x x)) 10 20))'
Result = 900
```
