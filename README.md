# Simpler ranges for C++17

Ranges in C++20 are looking unwieldy.

This is a ranges-like library for C++17 that provides zero-overhead list comprehensions with a
pipe-like syntax.

Standard containers can be filtered, transformed, passed through various algorithms, optimizing to
loops that would not be more efficient if written by hand.

The library makes heavy use of modern C++17 features, so a compliant C++17 compiler is required.

## Features

- Arbitrary composability.
- No temporary heap allocations (`sort() | to_vector()` only allocates the resulting vector).
- Open-ended generators (non-terminating sequences).
- No dependencies beyond the standard library.
- Header-only.
- Compatible with standard containers (anything that supports `std::begin()` and `std::end()`).
- Compatible with standard algorithms (implicit conversion to iterator-like objects).
- Integration with foreign codebases (override hooks for `std::optional`, `std::remove_cvref_t`,
  assertions, etc.).
- Simple to extend with custom range adapters.
- Zero-overhead.

## Limitations (non-goals)

- Bidirectional ranges. Ranges can only be consumed in the forward direction, so the equivalen of
  `std::reverse()` is not provided. Put the result in a temporary container and use standard
  algorithms.

## Algorithms

- `generate()`: Generate infinite elements from lambda.
- `seq()`: Generate an infinite sequence of elements of any arithmetic types.
- `fill()`: Generate infinite copies of a value.
- `fill_n()`: Generate `n` copies of a value (like `std::fill()`).
- `transform()`: Transform elements from an input range with lambda (like `std::transform()`).
- `filter()`: Produce only elements from an input range where predicate returns true.
- `first_n()`: Produce the first `n` elements of an input range.
- `first()`: Produce the first element as an `std::optional`.
- `skip_n()`: Produce all elements from an input range after skipping `n` elements.
- `until()`: Produce elements from an input range until a predicate returns false.
- `zip()`: Produce tuples of values from multiple input ranges.
- `count()`: Count number of elements in an input range.
- `foldl()`: Fold-left with initial value and lambda (like `std::accumulate()`).
- `sum()`: Sum elements of types that support `operator+`.
- `max()`: Get the maximum element, according to `std::max()`, or `std::nullopt` if the input is
  empty.
- `min()`: Get the minimum element, according to `std::min()`, or `std::nullopt` if the input is
  empty.
- `any_of()`: True if any element matches predicate.
- `all_of()`: True if all elements matches predicate.
- `none_of()`: True if no element matches predicate.
- `append()`: Append the result of an input range to an arbitrary container supporting
  `emplace_back()`, `push_back()`, or `emplace()`.
- `sort()`: Sort the elements of an input range with `std::sort()`. Does not allocate temporary
  storage, unless used as input for further algorithms.
- `uniq()`: Reduce the output by consecutive equality with `std::unique()`. Does not allocate
  temporary storage, unless used as input for further algorithms.

## Examples

See [test/test_ranges.cpp](the tests).