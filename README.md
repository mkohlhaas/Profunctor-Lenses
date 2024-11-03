# Profunctor Lenses

Pure profunctor lenses: a mechanism for updating, viewing, and setting values within nested data structures.

Learn more about profunctor lenses with:

- [Practical Profunctor Optics & Lenses in PureScript](https://thomashoneyman.com/articles/practical-profunctor-lenses-optics/), a practical introduction to profunctor optics in PureScript
- [Lenses for the Mere Mortal](https://leanpub.com/lenses), a book about lenses in PureScript

## Installation

Install `profunctor-lenses` with [Spago](https://github.com/purescript/spago):

```sh
spago install profunctor-lenses
```

## Quick start

```purescript
> structure = Tuple (Tuple (Tuple "hi!" 3) 2) 1

> import Data.Lens
> _leftmost = _1 <<< _1 <<< _1

> view _leftmost structure
"hi!"

> set _leftmost "Bye!" structure
(Tuple (Tuple (Tuple "Bye!" 3) 2) 1)

> over _leftmost String.toUpper structure
(Tuple (Tuple (Tuple "HI!" 3) 2) 1)
```

You can try out the [examples](./examples) in the REPL by running:

```
spago -x examples.dhall repl
```

## Documentation

`profunctor-lenses` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-profunctor-lenses).
1. Usage examples can be found in [the test suite](./test) and [`examples`](./examples) directory.
1. [Practical Profunctor Optics & Lenses in PureScript](https://thomashoneyman.com/articles/practical-profunctor-lenses-optics/), a practical introduction to profunctor optics in PureScript
1. [Lenses for the Mere Mortal](https://leanpub.com/lenses), a book about lenses in PureScript

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-profunctor-lenses/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.
