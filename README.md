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
- `flatten()`: Return a range flattening one level of nesting in a range of ranges.
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
- `for_each()`: Call function for each element in a range, returning `void`.
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

- `null_sink()`: A sink that simply discards all elements of a range.
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


### Custom combinators

If the algorithms included in rx::ranges do not cover your needs, defining your
own is simple. Here is an example of an adapter that converts its input to strings:

~~~c++
struct convert_to_string {
    template <class Input>
    struct Range {
        // In many cases, the output type will be derived from the input range. Not in this case,
        // though, because we always emit a string.
        using output_type = std::string;

        // Some ranges produce an infinite number of elements, but we produce the same number of
        // elements as our input.
        //
        // This definition is optional, and will be treated as "false" if absent.
        static constexpr bool is_finite = rx::is_finite_v<Input>;

        // Some ranges may call user-provided lambdas to produce a value, in which case they
        // cannot be guaranteed to be idempotent (meaning that calling `get()` and `next()` has
        // side-effects). In this case, we are idempotent if the input is idempotent.
        //
        // This definition is optional, and will be treated as "false" if absent.
        static constexpr bool is_idempotent = rx::is_idempotent_v<Input>;

        Input input;
        constexpr explicit Range(Input input) : input(std::move(input)) {}

        // Get the value at the current position. This is where we perform the conversion of
        // the input value to string.
        //
        // This method can be constexpr, but we are producing strings.
        [[nodiscard]] constexpr output_type get() const noexcept {
            return std::to_string(input.get());
        }

        // Advance to the next position.
        constexpr void next() noexcept {
            input.next();
        }

        // Check whether we can produce more elements, or we are at the end of the input.
        [[nodiscard]] constexpr bool at_end() const noexcept {
            return input.at_end();
        }

        // If we know something about the number of elements that we will produce, a hint
        // can be supplied to anyone consuming this range. This hint is solely used as an
        // optimization, e.g. to avoid excessive heap allocations.
        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            return input.size_hint();
        }

        // If we can advance by more than one position at a time, this method can be
        // implemented as an optimization.
        //
        // This method is optional. If absent will be implemented as calling `next()` in a loop
        // `n` times, or until `at_end()` returns true. Since callers cannot know for sure
        // how many remaining elements we can produce, it may be called with `n` larger
        // than that number, which should be treated as advancing to the end.
        //
        // The return value should be how much the range was actually advanced, and therefore always
        // less than or equal to `n`.
        //
        // In this example, we simply pass the call to the input range.
        constexpr size_t advance_by(size_t n) const noexcept {
            return rx::advance_by(input, n);
        }
    };

    // This operator is what takes care of chaining with other adapters.
    //
    // Implementing this is sufficient to support the `operator|` syntax.
    template <class Input>
    [[nodiscard]] constexpr auto operator()(Input&& input) const {
        // This line ensures that the adapter can be used to wrap standard containers as well as
        // chains of other adapters.
        //
        // If your range requires idempotency of the input, `as_idempotent_input_range()` is
        // provided as an alternative to `as_input_range()`. An example of an adapter that
        // requires idempotency is `filter()`, because it calls `get()` on its input both
        // when advancing to the next position, and when its own `get()` is called.
        using Inner = decltype(rx::as_input_range(std::forward<Input>(input)));
        return Range<Inner>(rx::as_input_range(std::forward<Input>(input)));
    }
};

// Using our new adapter in practice:
std::vector<std::string> convert_ints_to_sorted_strings(std::vector<int> input) {
    return input | convert_to_string() | rx::sort() | rx::to_vector();
}
~~~


### Custom sinks

Some algorithms want to operate on a "materialized" range, i.e. a collection of values from
some input, where all the values are available. However, we do not want to allocate temporary
storage for those when the result of a chain of adapters is going to be materialized into
some collection anyway. To facilitate this, the "sink" concept ensures that operations such as
`sort()` do not need any temporary storage beyond the resulting container. If multiple sinks
are chained, they will all operate on the same resulting container, in order.

Note that sinks are different from adapters because the operate on a whole range. They do not
have a notion of "position" and "advancing". However, if sinks are chained with other adapters,
temporary storage *will* be allocated to hold the input for the adapters. An example of this is
`sort() | filter()`.

Here is an example of a sink that normalizes a range representing an
N-dimensional vector, where N is the number of dimensions:

~~~c++
struct normalize {
    template <class Input>
    struct Range {
        using output_type = rx::get_output_type_of_t<Input>;
        Input input;
        constexpr explicit Range(Input input) : input(std::move(input)) {}

        // Write the result to `out`, which is assumed to be a standard container.
        // Note that "sinking" can only be done on an rvalue reference, to ensure that the
        // use case of storing a chain including sinks in a local variable that gets reused.
        template <class Out>
        constexpr void sink(Out& out) && noexcept {
            // First, produce the elements from the input. If the size of the input is
            // known, this also takes care of calling `reserve()` on the output, if it is
            // available.
            rx::sink(std::move(input), out);

            // Then, compute the length of the input vector:
            auto square = [](auto x) { return x * x; };
            auto length = std::sqrt(out | transform(square) | sum());

            // And finally, divide each element in the output by the length:
            for (auto& x : out) {
                x /= length;
            }
        }
    };

    template <class Input>
    [[nodiscard]] constexpr auto operator()(Input&& input) const {
        // Note: Here we are not using `as_input_range()`, because that would allocate
        // temporary storage for each sink in a chain.
        using Inner = rx::remove_cvref_t<Input>;
        return Range<Inner>(std::forward<Input>(input));
    }
};

// Using our new sink in practice:
std::vector<double> normalize_vector(std::vector<double> input) {
    return input | normalize() | to_vector();
}
~~~

### Custom aggregators

An aggregator is simply any function that takes a range as an input and outputs a single value. This
example computes the average of the input elements.

~~~c++
struct average {
    template <class Input>
    constexpr auto operator()(Input&& input) const {
        using element_type = rx::get_output_type_of_t<Input>;
        auto [count, summed] = rx::zip(rx::seq(1, 0), input)
            | rx::foldl(std::tuple(size_t(0), element_type{0}),
                [](auto&& accum, auto&& element) {
                    return std::tuple(std::get<0>(accum) + std::get<0>(element),
                                      std::get<1>(accum) + std::get<1>(element));
                });
        return summed / element_type(count);
    }
};

// Using our new aggregator in practice:
double compute_average(std::vector<double> values) {
    return values | average();
}
~~~