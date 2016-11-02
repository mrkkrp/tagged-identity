# Tagged Identity

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/tagged-identity.svg?style=flat)](https://hackage.haskell.org/package/tagged-identity)
[![Stackage Nightly](http://stackage.org/package/tagged-identity/badge/nightly)](http://stackage.org/nightly/package/tagged-identity)
[![Stackage LTS](http://stackage.org/package/tagged-identity/badge/lts)](http://stackage.org/lts/package/tagged-identity)
[![Build Status](https://travis-ci.org/mrkkrp/tagged-identity.svg?branch=master)](https://travis-ci.org/mrkkrp/tagged-identity)

The library provides a monad transformer that works just like `IdentityT`,
but can be tagged at type level. This allows to work with monad stacks as
usual, but you can make two identical monad stacks have different types. The
main application of this is, of course, ability to have different instances
for otherwise the same stacks without having to do opaque `newtype` wrapping
which is not handy with monad stacks.

## License

Copyright © 2016 Mark Karpov

Distributed under BSD 3 clause license.
