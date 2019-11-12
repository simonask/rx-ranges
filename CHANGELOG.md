## Version 1.0.1

Bug fixes and feature enhancements. Thanks for the valuable feedback from /r/cpp!

## Features

- Added `enumerate(...)` as an alias for `zip(seq(), ...)`.
- Added `reverse()` sink, which reverses the order of the input.
- Renamed `first_n()` to `take()` (keeping `first_n()` as an alias).
- Renamed `fill()` to `repeat()` (keeping `fill()` as an alias).
- Renamed `fill_n()` to `repeat_n()` (keeping `fill_n()` as an alias).

### Bugfixes

- Fixed a bug where `zip(...)` would not explicitly convert its inputs to ranges, meaning standard
  containers could not be used directly.
- Fixed a bug where `zip(...)` would produce tuples containing expired references.

## Other

- Added MIT license (See [./LICENSE](LICENSE)).
- Updated README, including more examples.