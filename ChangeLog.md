HEAD
====

- Add multi-line input support
- Add finaliser option to control REPL exit on <Ctrl-D>

0.3.0.0
=======

- Upgrades to lower bound to Haskeline 0.8.0.0.
- No longer requires MonadException.
- MonadCatch, MonadThrow, MonadMask instances.

0.2.2.0
=======

- `ReplOpts` configuration type and `evalReplOpts` function.
- Only use `fail` for GHC<8.0

0.2.1.0
=======

- Add `exceptions` dependency.
- Add a `MonadFail` instance to `HaskelineT`.

0.2.0.0
=======

- `evalRepl` has changed signature.

0.1.0.0
=======

- Initial release.
