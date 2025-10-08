# 0.3

- Add `extractByteString` function to the `EmacsMonad` class

# 0.2.1.1

- Fix build with GHC 9.10.1

# 0.2.1

- Expose `make_unibyte_string` as `makeBinaryString`

# 0.2

- Major rework of the package’s core

- Bump minimum required Emacs version to 28 due to exposing `process_input` from API. It allows to check whether user wants to abort and control should be returned back to Emacs ASAP

- Bump minimum required `base` version to 4.14 - minimum supported GHC is 8.10

- Symbols can be either statically known or dynamically known. This is mostly an optimization that doesn’t affect symbol use but client code may require updating. Statically known symbol are just pointers to statically allocated (by GHC) bytes and the pointers are simply passed to Emacs to create symbols.

- Some commonly used symbols are now cached on first use so they won’t be re-interned on subsequent uses. New symbols can be defined by users.

- `makeFunction` now cleans up after itself and no longer has memory leak if called a lot of times (which shouldn’t have typically happened anyway but still nice to have just in case)

- `makeFunctionExtra` and `EmacsFunctionExtra` are gone. They offered to pass extra pointer into subroutine exposed to Emacs but it was never needed since arbitrary closure can be exposed. Now extra pointer is used to clean up result of `makeFunction`.

- Removed `extractUnboxedVectorWith` - now regular `extractVector` produces generic vectors

- Removed `extractVector` - `extractVectorWith` and similar for other vector/array types are preferable since they’re guaranteed to not create intermediate vectors/arrays

- Introduce dedicated `Doc` type for function documentation that can be constructed from unboxed string literals to just pass the pointer around

- `produceRef` and `EmacsReturn` are gone - vanilla `pure` and `EmacsRef` are enough

- Symbol names got `IsString` instance and can be defined as string constants

- Removed `UserPtrFinaliser` and `UserPtrFinaliserType`. Use `Foreign.ForeignPtr.FinalizerPtr` instead

- `GlobalRef` got removed, instead `RawValue` is indexed by whether it’s going to be GC’ed after returning control back to Emacs

- `funcall` and `funcallPrimitive` now accept any emacs value function rather than symbols only

- `extractString` removed, `extractText` is now part of `MonadEmacs` typeclass

# 0.1.1.1

- Fix build with GHC 9.0+
- Switch license from BSD 3 to Apache 2.0
- Bump minimum required GHC to 8.10
- Bump minimum `prettyprinter` to 1.7
