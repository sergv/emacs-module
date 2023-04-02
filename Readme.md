# A Haskell package for writing Emacs modules

## Why would anyone want to write Emacs modules in Haskell?
Emacs Lisp is not a young language and can go quite a long way, but
it has a couple of issues that are not going to be solved any time soon:

- It’s dynamically typed which makes refactoring large extensions a pain
- It’s intepreted and is quite slow. It might be argued that editors don’t
  need much computing power, but from time to time computation-intensive
  tasks do occur. For example, fuzzy matching provided by the cool
  [flx.el](https://github.com/lewang/flx) package and used by great
  [ivy.el](https://github.com/abo-abo/swiper) package to quickly find things.
- Somewhat related to the previous point, there’s virtually no support
  for parallelising computations. There’re [adavances](https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Threads.html) on adding threads
  to Emacs lisp, but this only provides [concurrency, but no parallelism](https://stackoverflow.com/questions/1050222/what-is-the-difference-between-concurrency-and-parallelism).

  Haskell is well known for solving points 1 and 3 outlined above.
  For me it also solves point 2 by providing enough performance and adding
  parallelism on top of it.

  If you think this might be a good idea and would like to see what
  this package can do for you, you can look at part of
  [my emacs config](https://github.com/sergv/emacs-native/tree/master/lib/Emacs)
  that uses this package to implement things like

  + Rewrite of `flx.el` that leverages parallelism
  + Fast search across filesystem
  + Concurrrent grep reimplementation (somewhat dubious since things like `ripgrep` exist)

## FAQ
### How do I start writing my own extensions?
Some day there will be a proper tutorial for using this package.
For the time being the best place to start is
[this package’s tests](https://github.com/sergv/emacs-module/blob/master/test/src/Emacs/TestsInit.hs).

### What about Windows?
It works, Cabal can build a dll for you.

### How it’s related to [haskell-emacs](https://github.com/knupfer/haskell-emacs)?
The `haskell-emacs` aims to address the same problem - writing Emacs
extensions in Haskell, but uses different approach. It seems to use
some kind of marshalling scheme to make Emacs data available in
Haskell with a caveat that not all Emacs types can be converted (e.g.
buffers cannot be typically serialised). Presumably, an extension
built with this project will look like an executable that reads sexps
from stdin and produces output on stdout. Or, possibly, as a daemon
process that communicates with Emacs over network.

This project is a bit different. It wraps Emacs C API for writing new
extensions that can manipulate Emacs values directly, without
marhsalling. In this approach, an extension will look like a shared
library/dll that can be loaded by standard emacs with `(load "/tmp/libmy-ext.so")`.

## Supported GHC versions

Tested with GHC `8.10`, `9.0`, `9.2`, `9.4`.
