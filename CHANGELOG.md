## Version 2.0.0

Many optimizations, bug fixes, and new features. With this version, the popular "calendar" showcase
for range libraries is supported (see [./test/calendar.cpp](calendar.cpp) for more).

### Features

- Added `in_groups_of(size_t)`, `in_groups_of_exactly<size_t>()`, `in_groups_of_exactly(size_t)`,
  which produce consecutive subranges of a particular size. `in_groups_of(size_t)` includes all
  elements, potentially producing a smaller group at the end of the input.
- Added `group_adjacent_by(F)`, which produces groups of adjacent elements for which a given
  function returns the same value.
- `min()` and `max()` can now operate with a custom comparison function.
- Added `for_each` combinator, for situations where it produces more legible code.


### Bugfixes

- Overload resolution for `as_input_range()` was very confusing. This has been cleaned up.
- Combinators now support non-default-constructible value types, unless the output explicitly
  requires it.

### Optimizations

- When compilers support it, `__builtin_expect` is utilized in inner loops to aid code layout.
- Idempotency is now only ensured when the input isn't already idempotent.
- Fine-tuning of many internal loops to avoid unnecessary branches.
- Where possible, the "empty base class" optimization is used. This mostly applies to combinators    that take a standard comparison predicate (`std::less`, `std::equal_to`, etc.).

### Other

- Added a benchmark suite using Google Benchmark.
- Cleanup of internal naming conventions.
- `T::is_finite` is no longer required for input ranges (defaults to `false` if missing).

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