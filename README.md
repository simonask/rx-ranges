# Simpler ranges for C++17

Ranges in C++20 are looking unwieldy.

This is a ranges-like library for C++17 that provides zero-overhead list comprehensions with a
pipe-like syntax.

Standard containers can be filtered, transformed, passed through various algorithms, optimizing to
loops that would not be more efficient if written by hand.

The goal is to provide the tools to write more readable loops, where the intent of the programmer
is clearly communicated to the reader. Mentally simulating loops is a common but error-prone
part of reading other people's code, and indeed your own code from 3 months ago.

The library makes heavy use of modern C++17 features, so a compliant C++17 compiler is required.

## Features

- Arbitrary composability.
- Constexpr-friendly.
- No unnecessary temporary heap allocations (`foo | sort() | to_vector()` only allocates into the
  resulting container).
- Heap allocation minimization: `reserve()` is used on resulting containers, when possible.
- Open-ended generators (non-terminating, infinite ranges).
- Re-entrancy: A non-rvalue range can be used multiple times in a function.
- Compatible with standard containers (anything that supports `std::begin()` and `std::end()`).
- Compatible with standard algorithms (implicit conversion to iterator-like objects).
- Simple extensibility with custom range adapters. Just implement the `InputRange` faux-concept.
- Non-intrusive `operator|`. The ranges `foo | bar | baz` can be expressed as `baz(bar(foo))`, if
  using `operator|` would introduce ambiguous overloads.
- No dependencies beyond the standard library.
- Integration with foreign codebases (override hooks for `std::optional`, `std::remove_cvref_t`,
  assertions, etc.). Can easily be used as a submodule.
- Compiler support for all major compilers (GCC, Clang, MSVC).
- Zero-overhead, compared to manually written loops in optimized builds.
- Header-only, and single-header.

## Limitations (non-goals)

Other than usability concerns, these are the main differences from C++20 ranges.

- Bidirectional ranges. Ranges can only be consumed linearly in the forward direction.
- Random-access ranges. Ranges can only be consumed linearly in the forward direction.
- Internally using iterators. The internal iteration objects are modeled with an "enumerator"
  concept instead (objects that provide `next()`, `get()`, `at_end()`, etc.), which simplifies
  custom extensions. Implicit, zero-overhead conversion to iterators is provided for
  compatibility with standard algorithms and the range-based for loop syntax.
- Direct access to the data of underlying contiguous ranges (`data()` etc.).

## Compact generated code

Different compilers employ different heuristics, impacting things like inlining decisions,
auto-vectorization, register spilling, etc., and these tend to be highly sensitive to minor
code changes. For this reason, it is very hard to provide any guarantees about the generated
code.

[This example on Godbolt](https://godbolt.org/z/skF3-v) is a good case study. We can make the
following observations:

- For a constant argument, GCC does constant-folding in both the loop-based and range-based
  versions.
- Clang is able to constant-fold the range-based version, but not the loop-based version.
- MSVC does not do any constant-folding, but *does* auto-vectorize both versions, generating very
  similar code. MSVC also chooses to spill many registers to the stack, which may or may not impact
  performance in vectorized code.
- When the compilers are not able to constant-fold, the generated code is extremely similar for
  the loop-based version and the ranges-based version.

## Algorithms

### Generators

Algorithms that generate ranges of elements. These are typically used in the beginning of a chain.

- `cycle()`: Create an infinite range repeating the input elements in a loop.
- `empty_range()`: A range that is always empty.
- `fill_n()`: Generate `n` copies of a value (like `std::fill()`).
- `fill()`: Generate infinite copies of a value.
- `generate()`: Generate infinite elements by calling a user-provided function.
- `iterator_range()`: Form a range from a pair of standard iterators.
- `padded()`: Yield an infinite list of constant values once the input range is exhausted.
- `seq()`: Generate an infinite sequence of elements of any arithmetic types.

### Combinators

Algorithms that produce ranges from the output of other ranges.

- `chain()`: Return values from multiple ranges, one after another.
- `filter()`: Produce only elements from an input range where predicate returns true.
- `first_n()`: Alias for `take()`.
- `group_adjacent_by()`: Produce subranges for which a user-provided function returns the same value
  for a sequence of elements.
- `in_groups_of()`: Produce subranges containing at least one and at most `n` elements.
- `in_groups_of_exactly()`: Produce subranges containing exactly `n` elements, discarding elements
  at the end if number of input elements is not divisible by `n`.
- `skip_n()`: Produce all elements from an input range after skipping `n` elements.
- `take()`: Produce the first `n` elements of an input range.
- `tee()`: Copy values of a range into a container during iteration, forwarding the value unmodified
  to the next combinator.
- `transform()`: Transform elements from an input range with lambda (like `std::transform()`).
- `until()`: Produce elements from an input range until a predicate returns false.
- `zip_longest()`: Produce tuples of values from multiple input ranges, until all of the ranges
  reach their end.
- `zip()`: Produce tuples of values from multiple input ranges, until one of the ranges reaches its
  end.

### Aggregators

Algorithms that produce or modify a single value from a range of elements.

- `all_of()`: True if all elements matches predicate.
- `any_of()`: True if any element matches predicate.
- `append()`: Append the result of an input range to an arbitrary container supporting
  `emplace_back()`, `push_back()`, or `emplace()`.
- `count()`: Count number of elements in an input range.
- `first()`: Produce the first element as an `std::optional`.
- `foldl()`: Fold-left with initial value and lambda (like `std::accumulate()`).
- `max()`: Get the maximum element, according to `std::max()`, or `std::nullopt` if the input is
  empty.
- `min()`: Get the minimum element, according to `std::min()`, or `std::nullopt` if the input is
  empty.
- `none_of()`: True if no element matches predicate.
- `sum()`: Sum elements of types that support `operator+`.

### Sinks

Algorithms that operate on a full range of elements, and produce the output in a standard container.
When multiple sinks are chained, they will all operate on the same destination container, avoiding
temporary allocations. However, if a sink is chained with an aggregator or combinator, a temporary
`std::vector` will be allocated to hold the result.

- `reverse()`: Reverse the order of elements in the input.
- `uniq()`: Reduce the output by consecutive equality with `std::unique()`.
- `sort()`: Sort the elements of an input range with `std::sort()`.
- `to_vector()`: Produce an `std::vector` with all elements of the input.
- `to_list()`: Produce an `std::list` with all elements of the input.
- `to_map()`: Produce an `std::map` from elements of the input. Note that the input type must be
  "tuple-like", where the first component of the tuple becomes the key, and the second component
  becomes the value.
- `to_opt()`: Produce a single `std::optional` containing the last element of the input, or
  `std::nullopt` if the input was empty.
- `to_set()`: Produce an `std::set` from elements of the input.


## Examples

Generate a sequence of 15 odd integers:

~~~c++
for (auto x: seq() | filter([](int x) { return x % 2 == 1; }) | first_n(15)) {
    // do something with x
}
~~~

Generate a map of 15 integers to their string representation.

~~~c++
// Note: Composing two infinite ranges.
auto ints = seq() | filter([](int x) { return x % 2 == 1; };
auto strings = ints | transform([](int x) { return std::to_string(x); });
auto map = zip(ints, strings) | first_n(15) | to_map();
~~~

Sort a list of integers converted to strings. Note that sorting happens in the
resulting output vector. No additional storage is

~~~c++
auto ints = std::vector{{4, 1, 6, 2, 7, 4}};
// Note: The following line will not perform any computation yet.
auto strings = ints | transform([](int x) { return std::to_string(x); });
// Note: The following line will materialize into a vector and call reserve()
// based on the size of the input, resulting in a single allocation.
auto sorted = strings | sort() | to_vector();
// => {"1"s, "2"s, "4"s, "4"s, "6"s, "7"s}
~~~

See [the tests](test/test_ranges.cpp) for more.
