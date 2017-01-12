# xhb-requests

Most generated functions in [xhb](https://hackage.haskell.org/package/xhb) have types similar to those below:

```
createWindow :: Connection -> CreateWindow -> IO ()
grabKeyboard :: Connection -> GrabKeyboard -> IO (Receipt GrabStatus)
```

Those requests which expect responses can wait for them as follows:

```
getReply :: Receipt a -> IO (Either SomeError a)
```

Some functions are a bit different (request parameters don’t live inside a dedicated type, but are passed directly to the function):

```
configureWindow :: Connection -> WINDOW -> ValueParam Word16 -> IO ()
queryTextExtents :: Connection -> FONTABLE -> [CHAR2B] -> IO (Receipt QueryTextExtentsReply)
```

Nevertheless, thanks to disciplined code generation, xhb has a clean and uniform code base. The xhb-requests package wraps all of these generated request functions in two classes:

```
class Request a where
    requestIO :: a -> Connection -> IO ()

class RequestWithReply a b | a -> b, b -> a where
    requestWithReplyIO :: a -> Connection -> IO (IO (Either SomeError b))
```

The instances of this class are generated from the xhb source which is itself generated from XML protocol documents.
This may sound ugly, but it worked quite well.

This package is the foundation for [xhb-monad](https://nickspinale.github.io/xhb-monad) and friends.

[This article](http://nickspinale.com/articles/xhb-monad) describes this set of packages in detail.

## Documentation

Haddock can be found [here](https://nickspinale.github.io/xhb-requests).

## Building

[Nix](https://nixos.org/nix/) is used to build this package.

- `./build-utils` contains the program that generates `xhb-requests.cabal` and `Graphics.XHB.Requests.Internal.Instances.*` from the xhb source.
- The nix expression in `./xhb-requests-src.nix` shows how it is used.
- That expression describes a derivation containing the entire source of the package `xhb-requests`.
- `./xhb-requests.nix` describes the package itself.
- `./default.nix` describes a `nixpkgs` that includes `haskellPackages.xhb-requests`, `haskellPackages.xhb-requests-src`, and `haskellPackages.xhb-requests-build-utils`.

## Developing

`./mklinks` creates symlinks in `.` to the generated source in `/nix/store` to allow you to develop normally (e.g. with `ghci`).
