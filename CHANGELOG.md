## 0.0.1.2 (April 26th, 2017)

- Removed the need for `MonadIO` when running `PersistT`, since `MonadBaseControl IO` is already enough.

## 0.0.1.1 (April 24th, 2017)

- Added instances for `MonadThrow`, `MonadCatch`, and `MonadMask` to `PersistT`.

## 0.0.1.0 (April 20th, 2017)

- Initial release
