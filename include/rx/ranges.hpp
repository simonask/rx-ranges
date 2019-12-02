#ifndef RX_RANGES_HPP_INCLUDED
#define RX_RANGES_HPP_INCLUDED

#include <algorithm>
#include <array>
#include <list>
#include <map>
#include <set>
#include <type_traits>
#include <vector>

// This override is provided to avoid name clashes in foreign codebases where `rx` already has a
// different meaning.
#if !defined(RX_NAMESPACE_OVERRIDE)
#define RX_NAMESPACE rx
#else
#define RX_NAMESPACE RX_NAMESPACE_OVERRIDE
#endif

// Some foreign libraries may have their own optional type, which may be optimized for their own
// value types. For example, some libraries treat NaN values as "none", or provide "transparent"
// optional when the value type has an "illegal" representation.
#if !defined(RX_OPTIONAL_OVERRIDE)
#include <optional>
#define RX_OPTIONAL std::optional
#else
#define RX_OPTIONAL RX_OPTIONAL_OVERRIDE
#endif

// Some foreign libraries may provide a more user-friendly static assertion facility. Most compilers
// don't give any context about the types that failed a `static_assert()` check in templated code.
#if !defined(RX_TYPE_ASSERT_OVERRIDE)
#define RX_TYPE_ASSERT(cond) static_assert(std::conjunction<cond>::value)
#else
#define RX_TYPE_ASSERT RX_TYPE_ASSERT_OVERRIDE
#endif

// Some foreign libraries may provide their own assertion facilities that integrate better with
// their environment.
#if !defined(RX_ASSERT_OVERRIDE)
#include <cassert>
#define RX_ASSERT(cond) assert(cond)
#else
#define RX_ASSERT RX_ASSERT_OVERRIDE
#endif

// Unfortunately, this is not compatible with C++20 `[[likely]]` / `[[unlikely]]`, because it has a
// different syntax structure.
#if defined(__GNUC__) || defined(__clang__)
#define RX_LIKELY(cond) bool(__builtin_expect(!!(cond), 1))
#define RX_UNLIKELY(cond) bool(__builtin_expect(!!(cond), 0))
#else
#define RX_LIKELY(cond) bool(cond)
#define RX_UNLIKELY(cond) bool(cond)
#endif


/*!
    @brief rx::ranges library

    Core concepts
    =============

    - Input range: An object that can produce a series of values. Input ranges can be chained
                   together and combined to form aggregate input ranges.

    - Sink:        An object that can receive values from input ranges in order to perform some
                   action on the whole range of results. A chain of sinks will operate on the same
                   resulting container without allocating any intermediate storage. Sinks can also
                   be (implicitly) converted to input ranges, in which case temporary storage *will*
                   be allocated for the intermediate result.

*/
namespace RX_NAMESPACE {

template <class T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

/*!
    @brief InputRange concept

    An input range is expected to conform to the following interface:

    @code
    // Required:
    using output_type = ...; // The output element type, usually typename Inner::output_type.

    output_type get() const noexcept; // Get the value at the current position.

    void next() noexcept; // Advance to the next position.

    bool at_end() const noexcept; // True if there are no more elements.

    size_t size_hint() const noexcept; // A hint about how many times `next()` can be called.
                                       // It is purely an optimization hint, not the accurate number
                                       // of times. Return `std::numeric_limits<size_t>::max()` if
                                       // unknown.

    // Optional:

    static constexpr bool is_finite = ...; // True if the range produces a bounded number of
                                           // elements. Defaults to false.
    static constexpr bool is_idempotent = ...; // True if `get()` can be called multiple times
                                               // without advancing internal state. Defaults to
                                               // false.
    T&& get() && noexcept; // For non-idempotent ranges, an rvalue reference may be returned for
                           // efficiency. Note that this also requires the const `get()` version to
                           // be marked as `const&` instead of just `const`.
    size_t advance_by(size_t) noexcept; // Advance by n positions, or until the range is at its end.
    @endcode

    Calling `get()` or `next()` while `at_end() == true` is a breach of contract, and is allowed to
    be undefined behavior.

    @tparam T Implementation of the input range.
    @tparam Inner Another InputRange used as the input for this range.
*/
template <class T, class Enable = void>
struct is_input_range : std::false_type {};
template <class T>
struct is_input_range<
    T,
    std::void_t<
        typename T::output_type,
        decltype(std::declval<T&>().get()),
        decltype(std::declval<const T&>().at_end()),
        decltype(std::declval<T&>().next())>> : std::true_type {};
template <class T>
constexpr bool is_input_range_v = is_input_range<remove_cvref_t<T>>::value;

/*!
    @brief Sink concept

    Sinks transfer the result of a range expression to a container. If a sink is used as an
    InputRange, the result will be copied to a temporary vector, which will then become the input.

    A sink is expected to conform to the following interface:

    @code
    using output_type = ...; // The output element type, usually `typename Inner::output_type`, if
                             // the sink is wrapping another range or sink.
    template <class Out>
    void sink(Out& out) && noexcept; // Call sink(std::move(inner), out), and perform the desired
                                     // operation on the result.
    @endcode

    The `sink()` member function should be "destructive", and does not need to be repeatable
    multiple times for the same instance. If the sink wraps another range, it should call
    `sink(std::move(inner), out)`.

    @tparam T The sink type.
    @tparam Out The output container type.
*/
template <class T, class Enable = void>
struct is_sink : std::false_type {};
// Test whether T is a sink by checking that it has `T::output_type` and an implementation of
// `sink()` that accepts an `std::vector<T::output_type>&` as its argument.
template <class T>
struct is_sink<
    T,
    std::void_t<decltype(std::declval<T&&>().sink(
        std::declval<std::vector<typename remove_cvref_t<T>::output_type>&>()))>>
    : std::true_type {};
template <class T>
constexpr bool is_sink_v = is_sink<remove_cvref_t<T>>::value;

template <class T>
constexpr bool is_input_or_sink_v = is_input_range_v<T> || is_sink_v<T>;


template <class T, class Enable = void>
struct has_reserve : std::false_type {};
template <class T>
struct has_reserve<T, std::void_t<decltype(std::declval<T&>().reserve(std::declval<size_t>()))>>
    : std::true_type {};
template <class T>
constexpr bool has_reserve_v = has_reserve<remove_cvref_t<T>>::value;

template <class T, class Enable = void>
struct has_emplace_back : std::false_type {};
template <class T>
struct has_emplace_back<
    T,
    std::void_t<decltype(std::declval<T&>().emplace_back(
        std::declval<typename T::value_type&&>()))>> : std::true_type {};
template <class T>
constexpr bool has_emplace_back_v = has_emplace_back<remove_cvref_t<T>>::value;

template <class T, class Enable = void>
struct has_push_back : std::false_type {};
template <class T>
struct has_push_back<
    T,
    std::void_t<decltype(std::declval<T&>().push_back(std::declval<typename T::value_type&&>()))>>
    : std::true_type {};
template <class T>
constexpr bool has_push_back_v = has_push_back<remove_cvref_t<T>>::value;

template <class T, class Enable = void>
struct has_emplace : std::false_type {};
template <class T>
struct has_emplace<
    T,
    std::void_t<decltype(std::declval<T&>().emplace(std::declval<typename T::value_type&&>()))>>
    : std::true_type {};
template <class T>
constexpr bool has_emplace_v = has_emplace<remove_cvref_t<T>>::value;

// True if `std::tuple_size<T>::value` is defined.
template <class T, class Enable = void>
struct is_tuple_like : std::false_type {};
template <class T>
struct is_tuple_like<T, std::void_t<decltype(std::tuple_size<T>::value)>> : std::true_type {};
template <class T>
constexpr bool is_tuple_like_v = is_tuple_like<remove_cvref_t<T>>::value;

// Trait for input ranges to indicate whether they output a finite number of elements.
template <class T, class Enable = void>
struct is_finite : std::false_type {};
template <class T>
struct is_finite<T, std::void_t<decltype(T::is_finite)>> : std::bool_constant<T::is_finite> {};
template <class T>
constexpr bool is_finite_v = is_finite<remove_cvref_t<T>>::value;

// Trait for input ranges to indicate whether their `get()` function can be called multiple times
// without advancing internal state.
template <class T, class Enable = void>
struct is_idempotent : std::false_type {};
template <class T>
struct is_idempotent<T, std::void_t<decltype(T::is_idempotent)>>
    : std::bool_constant<T::is_idempotent> {};
template <class T>
constexpr bool is_idempotent_v = is_idempotent<remove_cvref_t<T>>::value;

// Fake RandomAccessIterator concept. Any iterator that supports advancing by an arbitrary integer
// amount is considered "random access".
template <class T, class Enable = void>
struct is_random_access_iterator : std::false_type {};
template <class T>
struct is_random_access_iterator<T, std::void_t<decltype(std::declval<T&>() += 0)>>
    : std::true_type {};
template <class T>
constexpr bool is_random_access_iterator_v = is_random_access_iterator<T>::value;

/// Convenience notation for chaining ranges.
///
/// Note that this enables `operator|` for any pair where RHS is callable with LHS as its only
/// argument. Therefore, it may be best to selectively import this operator into your scope with
/// `using`. If it simply cannot be imported without clashing with other definitions of `operator|`,
/// you can use the alternative chaining syntax using `operator()`: `rhs(lhs)`.
template <
    class LHS,
    class RHS,
    class = std::void_t<decltype(std::declval<RHS>()(std::declval<LHS>()))>>
constexpr auto operator|(LHS&& lhs, RHS&& rhs) noexcept {
    return std::forward<RHS>(rhs)(std::forward<LHS>(lhs));
}

/*!
    @brief Convert input range or sink to standard iterators.
*/
struct input_range_iterator_end {};
template <class R>
struct input_range_iterator {
    R range;
    template <class Arg>
    constexpr explicit input_range_iterator(Arg&& range) noexcept
        : range(std::forward<Arg>(range)) {}

    constexpr bool operator==(input_range_iterator_end) const noexcept {
        return range.at_end();
    }
    constexpr bool operator!=(input_range_iterator_end) const noexcept {
        return !range.at_end();
    }

    constexpr auto& operator++() noexcept {
        RX_ASSERT(!range.at_end());
        range.next();
        return *this;
    }

    [[nodiscard]] constexpr decltype(auto) operator*() const noexcept {
        return range.get();
    }
    template <
        class T = typename R::output_type,
        class = std::enable_if_t<std::is_lvalue_reference_v<T>>>
    [[nodiscard]] constexpr auto operator-> () const noexcept {
        return &range.get();
    }
};
template <class R>
input_range_iterator(R &&)->input_range_iterator<remove_cvref_t<R>>;

template <class R, class = std::enable_if_t<is_input_or_sink_v<R>>>
[[nodiscard]] constexpr auto begin(R&& range) noexcept {
    return input_range_iterator(as_input_range(std::forward<R>(range)));
}
template <class R, class = std::enable_if_t<is_input_or_sink_v<R>>>
[[nodiscard]] constexpr auto end(const R&) noexcept {
    // Note: The first argument may be moved-from, but that's OK, we just need its type.
    return input_range_iterator_end{};
}

template <class T, class Enable = void>
struct has_advance_by : std::false_type {};
template <class T>
struct has_advance_by<T, std::void_t<decltype(std::declval<T&>().advance_by(size_t(0)))>>
    : std::true_type {};
template <class T>
constexpr bool has_advance_by_v = has_advance_by<T>::value;

/// Advance a range by n steps, or until it reaches its end.
///
/// This calls R::advance_by() if available. Otherwise, it calls R::next() at most n times.
template <class R>
size_t advance_by(R& range, size_t n) {
    if constexpr (has_advance_by_v<R>) {
        return range.advance_by(n);
    } else {
        size_t i;
        for (i = 0; i < n && !range.at_end(); ++i) {
            range.next();
        }
        return i;
    }
}

namespace detail {
    template <class T>
    struct invalid_type {};
} // namespace detail

template <class In, class Out>
constexpr void sink_one(In&& in, Out& out) noexcept {
    static_assert(
        has_emplace_back_v<Out> || has_push_back_v<Out> || has_emplace_v<Out>,
        "Output supports neither emplace_back(), push_back(), nor emplace().");

    // Copy elements from the input to the output. If the input is tuple-like, and the output
    // supports emplacement, the tuple will be unpacked and passed as individual arguments to the
    // emplace member function on the output. Otherwise, the tuple-like object will be passed as-is.
    //
    // This means that both linear containers of tuples and associative containers like maps will
    // work as outputs.

    if constexpr (has_emplace_back_v<Out>) {
        if constexpr (is_tuple_like_v<In>) {
            // Output has emplace_back, and the input generates tuple-like elements. Pass tuple
            // elements as individual arguments to emplace_back.
            auto unpack = [&](auto&&... args) constexpr {
                out.emplace_back(std::forward<decltype(args)>(args)...);
            };
            std::apply(unpack, std::forward<In>(in));
        } else {
            out.emplace_back(std::forward<In>(in));
        }
    } else if constexpr (has_push_back_v<Out>) {
        out.push_back(std::forward<In>(in));
    } else if constexpr (has_emplace_v<Out>) {
        if constexpr (is_tuple_like_v<In>) {
            // Output has emplace, and the input generates tuple-like elements. Pass tuple
            // elements as individual arguments to emplace.
            auto unpack = [&](auto&&... args) constexpr {
                out.emplace(std::forward<decltype(args)>(args)...);
            };
            std::apply(unpack, std::forward<In>(in));
        } else {
            out.emplace(std::forward<In>(in));
        }
    }
}

/// Copy elements from an rvalue InputRange to output.
///
/// If an input range is chained with a sink (like `sort()`), this will the initial population of
/// the output, on which any further "sink" operators (like `uniq()`) will operate.
///
/// This function only participates in overload resolution if @a in is an rvalue reference. This is
/// to provide a compile-time check for reentrancy -- `sink()` should never advance the state of an
/// input range that may later be reused by the caller.
///
/// Note: As of C++17, no standard container can actually allocate memory in a `constexpr` context,
///       but this was added in C++20. This is the only reason that this function is marked
///       `constexpr`. See: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p0784r5.html
///
/// @tparam In The range to copy from.
/// @tparam Out A container that can hold the result.
template <class In, class Out>
constexpr void sink(
    In&& in,
    Out& out,
    std::enable_if_t<!std::is_lvalue_reference_v<In> && is_input_range_v<In>>* = nullptr) noexcept {
    RX_TYPE_ASSERT(is_finite<remove_cvref_t<In>>);

    if constexpr (has_reserve_v<Out>) {
        out.reserve(in.size_hint());
    }

    while (RX_LIKELY(!in.at_end())) {
        sink_one(in.get(), out);
        in.next();
    }
}

/// Copy elements from an lvalue InputRange to sink. Note that the input range is copied to maintain
/// reentrancy.
template <class In, class Out, class = std::enable_if_t<is_input_range_v<In>>>
constexpr void sink(const In& in, Out& out) noexcept {
    auto copy = in;
    sink(std::move(copy), out);
}

/// Copy resulting elements from an rvalue sink implementation into the output container.
///
/// This function is what allows operations that require temporary storage to work on the resulting
/// container without allocating their own temporary storage.
///
/// @tparam In The sink to copy from, typically an output modifier like `sort` or `uniq`.
/// @tparam Out A container that can hold the result.
template <
    class In,
    class Out,
    class = std::enable_if_t<is_sink_v<In>>,
    class = std::enable_if_t<!std::is_lvalue_reference_v<In>>>
constexpr void sink(In&& in, Out& out) noexcept {
    std::move(in).sink(out);
}

/// Copy resulting elements from an lvalue range, copying the input range before modifying it.
///
/// Note: This function should not be called from a Sink's implementation of `sink(out)`. Instead,
/// call `sink(std::move(inner), out)` to avoid unnecessary copies.
template <class In, class Out>
constexpr void sink(const In& in, Out& out, std::enable_if_t<is_sink_v<In>>* = nullptr) noexcept {
    In copy = in;
    sink(std::move(copy), out);
}

/// Copy elements from a standard container into a sink.
template <class In, class Out>
constexpr void
sink(const In& in, Out& out, std::enable_if_t<!is_input_or_sink_v<In>>* = nullptr) noexcept {
    std::copy(begin(in), end(in), std::back_inserter(out));
}

/*!
    @brief Create a range from a standard iterator pair, or a standard container.

    @tparam It The type of the iterator that will be incremented.
    @tparam EndIt The type of the end iterator, if it is different from `It`.
*/
template <class It, class EndIt = It>
struct iterator_range {
    using iterator_type = It;
    using end_iterator_type = EndIt;
    using output_type = decltype(*std::declval<const iterator_type&>());

    // TODO: If the end-iterator type is different from the iterator type, this may not produce
    // a finite range.
    static constexpr bool is_finite = true;
    static constexpr bool is_idempotent = true; // we only operate on const iterators

    It current_;
    EndIt end_;

    template <class C>
    constexpr explicit iterator_range(const C& collection) noexcept
        : current_(begin(collection)), end_(end(collection)) {}
    constexpr iterator_range(It begin, EndIt end) noexcept : current_(begin), end_(end) {}

    constexpr void next() noexcept {
        RX_ASSERT(!at_end());
        ++current_;
    }

    [[nodiscard]] constexpr output_type get() const noexcept {
        RX_ASSERT(!at_end());
        return *current_;
    }

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return current_ == end_;
    }

    constexpr size_t size_hint() const noexcept {
        if constexpr (std::is_same_v<It, EndIt> && is_random_access_iterator_v<It>) {
            return end_ - current_;
        } else {
            return 0;
        }
    }
    constexpr size_t advance_by(size_t n) noexcept {
        if constexpr (std::is_same_v<It, EndIt> && is_random_access_iterator_v<It>) {
            if (RX_LIKELY(size_t(end_ - current_) >= n)) {
                current_ += n;
                return n;
            } else {
                size_t advanced = end_ - current_;
                current_ = end_;
                return advanced;
            }
        } else {
            size_t i = 0;
            for (i = 0; i < n && current_ != end_; ++i) {
                next();
            }
            return i;
        }
    }
};
template <class C>
iterator_range(const C& c)->iterator_range<decltype(begin(c)), decltype(end(c))>;
template <class It, class EndIt>
iterator_range(It&&, EndIt &&)->iterator_range<remove_cvref_t<It>, remove_cvref_t<EndIt>>;

/*!
    @brief An input range over an internally owned `std::vector`.

    This is used internally when chains require temporary storage (for example, when a sink is
    used as an input rage).

    @tparam T The element type of the internal vector. Must be a non-reference value type.
*/
template <class T>
struct vector_range {
    using output_type = T;
    static constexpr bool is_finite = true;
    static constexpr bool is_idempotent = true;
    std::vector<T> vector_;
    // Note: Moving a vector does not invalidate iterators.
    typename std::vector<T>::const_iterator current_;

    constexpr explicit vector_range(std::vector<T> vec) noexcept
        : vector_(std::move(vec)), current_(vector_.begin()) {}
    constexpr vector_range(vector_range&&) noexcept = default;
    constexpr vector_range(const vector_range& other) noexcept
        : vector_(other.vector_), current_(vector_.begin()) {}
    constexpr vector_range& operator=(vector_range&&) noexcept = default;
    constexpr vector_range& operator=(const vector_range& other) noexcept {
        vector_ = other.vector_;
        current_ = vector_.begin();
        return *this;
    }
    constexpr void next() noexcept {
        RX_ASSERT(!at_end());
        ++current_;
    }
    constexpr const output_type& get() const& noexcept {
        RX_ASSERT(!at_end());
        return *current_;
    }
    constexpr output_type get() && noexcept {
        RX_ASSERT(!at_end());
        return std::move(*current_);
    }
    constexpr bool at_end() const noexcept {
        return current_ == vector_.end();
    }
    constexpr size_t size_hint() const noexcept {
        return vector_.size();
    }
    constexpr size_t advance_by(size_t n) noexcept {
        if (size_t(vector_.end() - current_) >= n) {
            current_ += n;
            return n;
        } else {
            size_t advanced = vector_.end() - current_;
            current_ = vector_.end();
            return advanced;
        }
    }
};

namespace detail {
    // This exists only to maintain some sanity in figuring out overload resolution for
    // as_input_range().
    struct input_category {
        struct input_range {};
        struct sink {};
        struct other {};
    };

    template <class, class Enable = void>
    struct input_category_of {
        using type = input_category::other;
    };
    template <class T>
    struct input_category_of<T, std::enable_if_t<is_input_range_v<T>>> {
        using type = input_category::input_range;
    };
    template <class T>
    struct input_category_of<T, std::enable_if_t<is_sink_v<T>>> {
        using type = input_category::sink;
    };

    template <class T>
    using input_category_of_t = typename input_category_of<remove_cvref_t<T>>::type;

    template <class T>
    constexpr decltype(auto) as_input_range_impl(input_category::input_range, T&& range) noexcept {
        return std::forward<T>(range);
    }

    template <class T>
    constexpr auto as_input_range_impl(input_category::sink, T&& input) noexcept {
        static_assert(
            !std::is_lvalue_reference_v<T>,
            "Please only convert sinks to input ranges with rvalue references (for efficiency).");
        using RX_NAMESPACE::sink;
        // Store the output in a temporary vector.
        using output_type = remove_cvref_t<typename remove_cvref_t<T>::output_type>;
        std::vector<output_type> vec;
        sink(std::move(input), vec);
        return vector_range<output_type>{std::move(vec)};
    }

    template <class T>
    constexpr auto as_input_range_impl(input_category::other, const T& container) noexcept {
        return iterator_range(container.begin(), container.end());
    }

    template <class T, size_t N>
    constexpr auto as_input_range_impl(input_category::other, T (&arr)[N]) noexcept {
        return iterator_range(std::begin(arr), std::end(arr));
    }
} // namespace detail

template <class R>
constexpr decltype(auto) as_input_range(R&& range) {
    using category = detail::input_category_of_t<R>;
    return detail::as_input_range_impl(category{}, std::forward<R>(range));
}

/*!
    @brief Wrap an non-idempotent range to make it idempotent.

    Some combinators expect an idempotent input to avoid internal storage unless it is actually
    needed. This is a convenience wrapper for those combinators, which wraps a non-idempotent
    combinator.

    Idempotency is achieved by storing the "current" element in internal, temporary storage. For
    this reason, the output type of `R` must be copy-constructible.

    @tparam R The inner (non-idempotent) range.
*/
template <class R>
struct idempotent_range {
    static_assert(!is_idempotent_v<R>); // Don't wrap an already idempotent range.

    static constexpr bool is_finite = is_finite_v<R>;
    static constexpr bool is_idempotent = true;
    using element_type = remove_cvref_t<typename R::output_type>;
    using storage_type = RX_OPTIONAL<element_type>;
    using output_type = const element_type&;

    R inner_;
    storage_type current_;

    template <class Q>
    idempotent_range(Q&& inner) : inner_(std::forward<Q>(inner)) {
        if (!inner_.at_end()) {
            current_.emplace(inner_.get());
        }
    }

    constexpr output_type get() const noexcept {
        RX_ASSERT(!at_end());
        return *current_;
    }

    constexpr void next() noexcept {
        RX_ASSERT(!at_end());
        inner_.next();
        if (!inner_.at_end()) {
            current_.emplace(inner_.get());
        }
    }

    constexpr bool at_end() const noexcept {
        return inner_.at_end();
    }

    constexpr size_t size_hint() const noexcept {
        return inner_.size_hint();
    }
};
template <class R>
idempotent_range(R &&)->idempotent_range<remove_cvref_t<R>>;

// Convenience function to make non-idempotent input ranges idempotent.
template <class R>
constexpr auto as_idempotent_input_range(R&& range) {
    // Convert the input to an input range, and then wrap it in idempotent_range if it isn't already
    // idempotent.
    auto input_range = as_input_range(std::forward<R>(range));
    using range_type = decltype(input_range);
    if constexpr (is_idempotent_v<range_type>) {
        return input_range;
    } else {
        return idempotent_range(std::move(input_range));
    }
}

// Get the equivalent range type of some type. This is the return type of `as_input_range()` without
// cvref-qualifiers when called with T as the argument (which may be any lvalue or rvalue type). The
// reason that we can't juse use remove_cvref_t<T> is that some inputs may be converted to a
// different type by `as_input_range()`, such as standard containers.
template <class T>
using get_range_type_t = remove_cvref_t<decltype(as_input_range(std::declval<T>()))>;

// This function is analogous to get_range_type_t. Instead the result of as_input_range() it tells
// you the result of as_idempotent_input_range().
template <class T>
using get_idempotent_range_type_t =
    remove_cvref_t<decltype(as_idempotent_input_range(std::declval<T>()))>;

// Get the output type of T as if it was converted to a range. This is the output type of the input
// range after conversion through `as_input_range()`.
template <class T>
using get_output_type_of_t = typename get_range_type_t<T>::output_type;

/*!
    @brief Generate a series of values from a function.

    Produces an infinite number of outputs, calling F to produce each output.

    Combine with things like take(), until(), etc. to create a finite range.

    Note: The function does not have to be idempotent, but subsequent chained combinators may cache
          the return value if they need idempotency.

    @tparam F A function that will be invoked to produce an output.
*/
template <class F>
struct generate {
    using output_type = decltype(std::declval<F>()());
    static constexpr bool is_finite = false;
    static constexpr bool is_idempotent = false;

    mutable F func;

    template <class Fx>
    constexpr explicit generate(Fx&& func) : func(std::forward<Fx>(func)) {}

    constexpr void next() noexcept {}

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return false;
    }

    [[nodiscard]] constexpr decltype(auto) get() const {
        return func();
    }

    constexpr size_t size_hint() const noexcept {
        return std::numeric_limits<size_t>::max();
    }
};
template <class F>
generate(F &&)->generate<remove_cvref_t<F>>;

/*!
    @brief Sequence generator.

    Create an infinite sequence of T, advancing each iteration by a value of type U.

    T is expected to be initializable by an integral constant, and to have well-defined
    `operator+=(U)`.
*/
template <class T, class U = T>
struct seq {
    T current;
    U step;

    using output_type = T;
    static constexpr bool is_finite = false;
    static constexpr bool is_idempotent = true;

    constexpr explicit seq(T init = T{}, U step = U{} + 1) : current(init), step(step) {}

    constexpr void next() {
        current += step;
    }
    [[nodiscard]] constexpr output_type get() const {
        return current;
    }
    [[nodiscard]] constexpr bool at_end() const noexcept {
        return false;
    }
    constexpr size_t size_hint() const noexcept {
        return std::numeric_limits<size_t>::max();
    }
};
seq()->seq<int>;
template <class T>
seq(T)->seq<T>;
template <class T, class U>
seq(T, U)->seq<T, U>;

/*!
    @brief Generate infinite copies of type T.
*/
template <class T>
struct fill {
    T value;
    constexpr explicit fill(T value) noexcept : value(std::move(value)) {}

    using output_type = T;
    static constexpr bool is_finite = false;
    static constexpr bool is_idempotent = true;

    constexpr void next() {}

    [[nodiscard]] constexpr output_type get() const {
        return value;
    }

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return false;
    }

    constexpr size_t size_hint() const noexcept {
        return std::numeric_limits<size_t>::max();
    }
};
template <class T>
fill(T &&)->fill<remove_cvref_t<T>>;

/*!
    @brief Transform a range of values by a function F.

    Each output is produced by passing the output of the inner range to the function F.
*/
template <class F>
struct transform {
    F func;
    explicit constexpr transform(F func) noexcept : func(std::move(func)) {}

    template <class InputRange>
    struct Range {
        InputRange input_;
        transform transform_;
        using output_type = decltype(std::declval<const F&>()((input_.get())));
        static constexpr bool is_finite = is_finite_v<InputRange>;
        static constexpr bool is_idempotent = false; // `func` is called each time `get` is called

        constexpr Range(InputRange input, transform transform) noexcept
            : input_(std::move(input)), transform_(std::move(transform)) {}
        constexpr void next() {
            RX_ASSERT(!at_end());
            input_.next();
        }
        constexpr output_type get() const {
            RX_ASSERT(!at_end());
            return transform_.func(input_.get());
        }
        constexpr bool at_end() const {
            return input_.at_end();
        }
        constexpr size_t size_hint() const noexcept {
            return input_.size_hint();
        }
        constexpr size_t advance_by(size_t n) noexcept {
            using RX_NAMESPACE::advance_by;
            return advance_by(input_, n);
        }
    };

    // Since F may be either cheap or expensive to copy, provide overloads for both const-refs and
    // rvalue-refs.

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        using Inner = get_range_type_t<InputRange>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), *this};
    }
    template <class InputRange>
        [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        using Inner = get_range_type_t<InputRange>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), std::move(*this)};
    }
};
template <class F>
transform(F &&)->transform<remove_cvref_t<F>>;

/*!
    @brief Filter range by predicate.

    If the predicate returns true, the element will be included in the output. Otherwise, it will
    not.

    @tparam P A predicate returning a value convertible to bool
*/
template <class P>
struct filter {
    P pred;
    constexpr explicit filter(P pred) : pred(std::move(pred)) {}

    template <class R>
    struct Range {
        static_assert(is_idempotent_v<R>);
        using output_type = remove_cvref_t<typename R::output_type>;
        static constexpr bool is_finite = is_finite_v<R>;
        static constexpr bool is_idempotent = true;

        R input_;
        P pred_;

        template <class Q>
        constexpr Range(R input, Q&& pred)
            : input_(std::move(input)), pred_(std::forward<Q>(pred)) {
            // Skip initial non-matching outputs.
            while (RX_LIKELY(!input_.at_end()) && !pred_(input_.get())) {
                input_.next();
            }
        }

        [[nodiscard]] constexpr decltype(auto) get() const& noexcept {
            RX_ASSERT(!at_end());
            return input_.get();
        }

        [[nodiscard]] constexpr decltype(auto) get() && noexcept {
            RX_ASSERT(!at_end());
            return input_.get();
        }

        constexpr void next() noexcept {
            RX_ASSERT(!at_end());
            // XXX: For some reason, compilers are not able to merge this loop condition with
            // conditions on the input range, causing many more branches than necessary. (Tested
            // with MSVC, Clang, GCC.)
            //
            // This means that filter() can be much much slower (4x with Clang/GCC, 10x with MSVC)
            // in tight loops.
            input_.next();
            while (!at_end() && !pred_(input_.get())) {
                input_.next();
            }
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return input_.at_end();
        }

        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            return input_.size_hint();
        }
    };

    // Since F may be either cheap or expensive to copy, provide overloads for both const-refs and
    // rvalue-refs.

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& {
        using Inner = get_idempotent_range_type_t<InputRange>;
        return Range<Inner>{as_idempotent_input_range(std::forward<InputRange>(input)), pred};
    }
    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) && {
        using Inner = get_idempotent_range_type_t<InputRange>;
        return Range<Inner>{as_idempotent_input_range(std::forward<InputRange>(input)),
                            std::move(pred)};
    }
};
template <class F>
filter(F &&)->filter<remove_cvref_t<F>>;

/*!
    @brief Create a range that produces at most N elements from the beginning of its input.
*/
struct take {
    const size_t n;
    constexpr explicit take(size_t n) noexcept : n(n) {}

    template <class R>
    struct Range {
        using output_type = typename R::output_type;
        static constexpr bool is_finite = true;
        static constexpr bool is_idempotent = is_idempotent_v<R>;

        R inner;
        const size_t n;
        size_t i = 0;

        constexpr Range(R inner, size_t n) noexcept : inner(std::move(inner)), n(n) {}

        [[nodiscard]] constexpr output_type get() const noexcept {
            RX_ASSERT(!at_end());
            return inner.get();
        }

        constexpr void next() noexcept {
            RX_ASSERT(!at_end());
            ++i;
            inner.next();
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return i >= n || inner.at_end();
        }

        constexpr size_t size_hint() const noexcept {
            return n;
        }

        constexpr size_t advance_by(size_t m) noexcept {
            using RX_NAMESPACE::advance_by;
            // Addition beyond n and integer overflow both clamp to the end.
            size_t check = i + m;
            bool int_did_overflow = check < i;
            bool bounds_did_overflow = check > n;

            size_t advanced = 0;
            if (!int_did_overflow && !bounds_did_overflow) {
                advanced = m;
                advance_by(inner, advanced);
                i += m;
            } else if (i != n) {
                advanced = n - i;
                advance_by(inner, advanced);
                i = n;
            }
            RX_ASSERT((!int_did_overflow && !bounds_did_overflow) || at_end());
            return advanced;
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const {
        using Inner = get_range_type_t<InputRange>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), n};
    }
};

/*!
    @brief Generate N copies of type T.

    This is equivalent to `fill(value) | take(n)`.
*/
template <class T>
constexpr auto fill_n(size_t n, T&& v) {
    return fill(std::forward<T>(v)) | take(n);
}

/// Alias for `take()`.
using first_n = take;

/*!
    @brief Create a range that skips the first N elements of its input.
*/
struct skip_n {
    const size_t n;
    constexpr explicit skip_n(size_t n) noexcept : n(n) {}

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const {
        auto range = as_input_range(std::forward<InputRange>(input));
        advance_by(range, n);
        return range;
    }
};

/*!
    @brief Produce elements from the input until the predicate returns true.

    Note: The element for which the predicate returned true is not included in the output.

    @tparam P A predicate function callable with elements of the input as its argument, returning a
              boolean.
*/
template <class P>
struct until {
    P pred;
    constexpr explicit until(P pred) : pred(std::move(pred)) {}

    template <class R>
    struct Range {
        static_assert(is_idempotent_v<R>);
        using output_type = remove_cvref_t<typename R::output_type>;
        static constexpr bool is_finite = true;
        static constexpr bool is_idempotent = false; // get() is called twice per iteration

        R input;
        P pred;
        bool done = false;

        constexpr Range(R input, P pred) noexcept
            : input(std::move(input)), pred(std::move(pred)), done(input.at_end()) {
            if (!done) {
                done = pred(input.get());
            }
        }

        [[nodiscard]] constexpr decltype(auto) get() const& noexcept {
            RX_ASSERT(!at_end());
            return input.get();
        }

        [[nodiscard]] constexpr decltype(auto) get() && noexcept {
            RX_ASSERT(!at_end());
            return input.get();
        }

        constexpr void next() noexcept {
            RX_ASSERT(!at_end());
            input.next();
            done = input.at_end();
            if (!done) {
                done = pred(input.get());
            }
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return done;
        }

        constexpr size_t size_hint() const noexcept {
            return 0;
        }
    };

    template <class InputRange>
    constexpr auto operator()(InputRange&& input) const& {
        using Inner = get_idempotent_range_type_t<InputRange>;
        return Range<Inner>{as_idempotent_input_range(std::forward<InputRange>(input)), pred};
    }

    template <class InputRange>
    constexpr auto operator()(InputRange&& input) && {
        using Inner = get_idempotent_range_type_t<InputRange>;
        return Range<Inner>{as_idempotent_input_range(std::forward<InputRange>(input)),
                            std::move(pred)};
    }
};
template <class P>
until(P &&)->until<remove_cvref_t<P>>;

template <class... Inputs>
struct ZipRange {
    static_assert(sizeof...(Inputs) > 0);

    std::tuple<Inputs...> inputs;
    using output_type = std::tuple<remove_cvref_t<typename Inputs::output_type>...>;
    static constexpr bool is_finite = (is_finite_v<Inputs> || ...);
    static constexpr bool is_idempotent = (is_idempotent_v<Inputs> && ...);

    template <class... Tx>
    constexpr explicit ZipRange(std::tuple<Tx...>&& tuple) : inputs(tuple) {}

    [[nodiscard]] constexpr output_type get() const noexcept {
        RX_ASSERT(!at_end());
        return _get(std::index_sequence_for<Inputs...>{});
    }

    constexpr void next() noexcept {
        RX_ASSERT(!at_end());
        _next(std::index_sequence_for<Inputs...>{});
    }

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return _at_end(std::index_sequence_for<Inputs...>{});
    }

    constexpr size_t size_hint() const noexcept {
        return _size_hint(std::index_sequence_for<Inputs...>{});
    }

    constexpr size_t advance_by(size_t n) noexcept {
        return _advance_by(std::index_sequence_for<Inputs...>{}, n);
    }

private:
    template <size_t... Index>
    [[nodiscard]] constexpr output_type _get(std::index_sequence<Index...>) const noexcept {
        return output_type(std::forward_as_tuple(std::get<Index>(inputs).get()...));
    }

    template <size_t... Index>
    constexpr void _next(std::index_sequence<Index...>) noexcept {
        (std::get<Index>(inputs).next(), ...);
    }

    template <size_t... Index>
    [[nodiscard]] constexpr bool _at_end(std::index_sequence<Index...>) const noexcept {
        return (std::get<Index>(inputs).at_end() || ...);
    }

    template <size_t... Index>
    constexpr size_t _size_hint(std::index_sequence<Index...>) const noexcept {
        return std::min({std::get<Index>(inputs).size_hint()...});
    }

    template <size_t... Index>
    constexpr size_t _advance_by(std::index_sequence<Index...>, size_t n) noexcept {
        using RX_NAMESPACE::advance_by;
        return std::min({advance_by(std::get<Index>(inputs), n)...});
    }
};
template <class... Inputs>
ZipRange(std::tuple<Inputs...> &&)->ZipRange<remove_cvref_t<Inputs>...>;

/*!
    @brief Zip two or more ranges.

    Until none of the ranges are at end, produce a tuple of an element from each range.

    The ranges are not required to produce the same number of elements, but elements will only be
    produced until one of the ranges reaches its end.
*/
template <class... Inputs>
[[nodiscard]] constexpr auto zip(Inputs&&... inputs) noexcept {
    // For some reason, argument deduction doesn't work here.
    return ZipRange(std::forward_as_tuple(as_input_range(std::forward<Inputs>(inputs))...));
}

/*!
    @brief Enumerate elements of the input sequentially.

    Equivalent to `zip(seq(), inputs...)`.

    The inputs are not required to produce the same number of elements, but elements will only be
    produced until one of the ranges reaches its end.
*/
template <class... Inputs>
[[nodiscard]] constexpr auto enumerate(Inputs&&... inputs) noexcept {
    return ZipRange(
        std::forward_as_tuple(seq<size_t>(), as_input_range(std::forward<Inputs>(inputs))...));
}

/*!
    @brief Produce sequential groups of exactly N elements.

    The output type is a range of exactly N element.

    If the input produces a number of inputs that is not divisible by N, elements at the end will be
    skipped.
*/
struct in_groups_of_exactly {
    const size_t n;

    constexpr explicit in_groups_of_exactly(size_t n) noexcept : n(n) {
        RX_ASSERT(n > 0);
    }

    template <class R>
    struct Range {
        using element_type = remove_cvref_t<typename R::output_type>;
        using output_type = remove_cvref_t<decltype(std::declval<R>() | take(1))>;
        static constexpr bool is_finite = is_finite_v<R>;
        static constexpr bool is_idempotent = true; // ... but we output potentially non-idempotent
                                                    // ranges.

        R inner;
        const size_t n;
        RX_OPTIONAL<output_type> storage;

        constexpr explicit Range(R inner, size_t n) : inner(std::move(inner)), n(n) {
            next();
        }


        [[nodiscard]] constexpr output_type get() const& noexcept {
            RX_ASSERT(!at_end());
            return *storage;
        }

        [[nodiscard]] constexpr output_type get() && noexcept {
            RX_ASSERT(!at_end());
            return std::move(*storage);
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return bool(!storage);
        }

        constexpr void next() noexcept {
            storage.reset();
            if (RX_LIKELY(!inner.at_end())) {
                auto copy = as_input_range(inner);
                using RX_NAMESPACE::advance_by;
                if (RX_LIKELY(n > 1)) {
                    advance_by(inner, n - 1);
                    if (inner.at_end()) {
                        // end was reached before we could produce a whole group.
                        storage.reset();
                        return;
                    }
                }
                // n cannot be zero
                inner.next();
                storage.emplace(std::move(copy) | take(n));
            }
        }

        constexpr size_t size_hint() const noexcept {
            return inner.size_hint() / n;
        }

        constexpr size_t advance_by(size_t m) noexcept {
            using RX_NAMESPACE::advance_by;
            size_t advanced = advance_by(inner, n * m);
            next();
            return advanced / n;
        }
    };

    template <class R>
    [[nodiscard]] auto operator()(R&& input) const {
        using Inner = get_range_type_t<R>;
        return Range<Inner>{as_input_range(std::forward<R>(input)), n};
    }
};

/*!
    @brief Produce sequential groups of N or fewer elements.

    The output type is a range producing at most N elements. If the input produces a number of
    inputs that is not divisible by N, the last group produces will have a size < N.
*/
struct in_groups_of {
    size_t n;

    constexpr explicit in_groups_of(size_t n) noexcept : n(n) {
        RX_ASSERT(n > 0);
    }

    template <class R>
    struct Range {
        using element_type = remove_cvref_t<typename R::output_type>;
        using output_type = decltype(std::declval<R>() | take(1));
        static constexpr bool is_finite = is_finite_v<R>;
        static constexpr bool is_idempotent = true; // ... but we output potentially non-idempotent
                                                    // ranges.

        R inner;
        RX_OPTIONAL<output_type> storage;
        size_t n;

        Range(R inner, size_t n) : inner(std::move(inner)), n(n) {
            next();
        }

        [[nodiscard]] output_type get() const& noexcept {
            RX_ASSERT(!at_end());
            return *storage;
        }

        [[nodiscard]] output_type get() && noexcept {
            RX_ASSERT(!at_end());
            return std::move(*storage);
        }

        [[nodiscard]] bool at_end() const noexcept {
            return bool(!storage);
        }

        constexpr void next() noexcept {
            storage.reset();
            if (RX_LIKELY(!inner.at_end())) {
                auto copy = as_input_range(inner);
                using RX_NAMESPACE::advance_by;
                advance_by(inner, n);
                storage.emplace(std::move(copy) | take(n));
            }
        }

        [[nodiscard]] size_t size_hint() const noexcept {
            // Round up.
            return (inner.size_hint() + n - 1) / n;
        }

        constexpr size_t advance_by(size_t m) noexcept {
            using RX_NAMESPACE::advance_by;
            size_t advanced = advance_by(inner, n * m);
            next();
            // Round up
            return (advanced + n - 1) / n;
        }
    };

    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) const {
        using Inner = get_range_type_t<R>;
        return Range<Inner>{as_input_range(std::forward<R>(input)), n};
    }
};

/*!
    @brief Produce consecutive groups of elements for which P returns the same value, according to
    the Compare function.

    The output type is a range of the same types as the input. The size of the range is
    indeterminate, but always greater than 0.

    @tparam P The discriminant function. When the return value of this function changes, according
              to Compare, a new group is emitted.
    @tparam Compare The compare function used to compare return values of P.
    @tparam group_size_hint A hint about the average group size. It is 8 by default (somewhat
                            arbitrarily). If smaller or larger groups are expected, this parameter
                            can be tuned to avoid either preallocating too little or too much,
                            respectively, when outputting to a sink that supports `reserve()`.
*/
template <class P, class Compare = std::equal_to<void>, size_t group_size_hint = 8>
struct group_adjacent_by : private Compare {
    P pred;
    constexpr explicit group_adjacent_by(P pred, Compare cmp)
        : Compare(std::move(cmp)), pred(std::move(pred)) {}

    // Only enabled if C is default-constructible.
    template <class Cx = Compare>
    constexpr explicit group_adjacent_by(P pred) : pred(std::move(pred)) {}

    template <class R>
    struct Range : private Compare {
        using element_type = remove_cvref_t<typename R::output_type>;
        using output_type = remove_cvref_t<decltype(std::declval<R>() | take(1))>;
        static constexpr bool is_finite = is_finite_v<R>;
        static constexpr bool is_idempotent = true; // we have internal storage

        R inner;
        P pred;
        // rationale: most ranges in this library are not assignable
        RX_OPTIONAL<output_type> storage;

        Range(R inner, P pred, Compare cmp)
            : Compare(std::move(cmp)), inner(std::move(inner)), pred(std::move(pred)) {
            next();
        }

        [[nodiscard]] constexpr output_type get() const noexcept {
            RX_ASSERT(!at_end());
            return *storage;
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return !bool(storage);
        }

        void next() noexcept {
            // No at_end() test: method is called in constructor.
            if (RX_LIKELY(!inner.at_end())) {
                fill_group();
            } else {
                storage.reset();
            }
        }

        [[nodiscard]] size_t size_hint() const noexcept {
            // It's impossible to say how many groups we will produce, but we can take a guess.
            // Somewhat arbitrarily, we just assume that we will produce groups of about 8.
            if constexpr (R::is_finite) {
                const size_t n = inner.size_hint();
                return n / group_size_hint + 1;
            } else {
                return std::numeric_limits<size_t>::max();
            }
        }

        constexpr void fill_group() {
            auto copy = as_input_range(inner);
            const auto& current = inner.get(); // lifetime extension for value types
            auto p = pred(current);
            size_t n = 0;
            const Compare& cmp = *this;
            do {
                ++n;
                inner.next();
                if (inner.at_end()) {
                    break;
                }
            } while (cmp(p, pred(inner.get())));
            storage.emplace(std::move(copy) | take(n));
        }
    };

    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) const {
        using Inner = get_range_type_t<R>;
        const Compare& cmp = *this;
        return Range<Inner>{as_input_range(std::forward<R>(input)), pred, cmp};
    }
};

template <class P>
group_adjacent_by(P &&)->group_adjacent_by<remove_cvref_t<P>>;
template <class P, class Compare>
group_adjacent_by(P&&, Compare &&)->group_adjacent_by<remove_cvref_t<P>, remove_cvref_t<Compare>>;

/*!
    @brief Sink element(s) into an optional.

    If the input produces 1 or more output, the last element produced will be placed in the result.

    If the input produces 0 outputs, the result will be nullopt.
*/
struct to_opt {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) noexcept {
        RX_OPTIONAL<get_output_type_of_t<R>> result;
        sink(std::forward<R>(input), result);
        return result;
    }
};

/*!
    @brief Get the first element of a range, if it exists.

    The result is an optional containing the first element, or none if the range did not produce any
    outputs.
*/
struct first {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) const {
        return to_opt()(take(1)(std::forward<R>(input)));
    }
};

struct to_vector {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const {
        std::vector<remove_cvref_t<get_output_type_of_t<R>>> vec;
        sink(std::forward<R>(range), vec);
        return vec;
    }
};

/*!
    @brief Convert range into std::vector.
*/
struct to_list {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const {
        std::list<remove_cvref_t<get_output_type_of_t<R>>> list;
        sink(std::forward<R>(range), list);
        return list;
    }
};

/*!
    @brief Convert range into std::map.

    The input range is expected to produce values conforming to the "tuple" concept, i.e.,
    std::get<0>(value) and std::get<1>(value) are defined, as well as std::tuple_element<0> and
    std::tuple_element<1>.

    The first element of the output will be the key, and the second element will be the value.
*/
struct to_map {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const {
        using output_type = get_output_type_of_t<R>;
        using key_type = remove_cvref_t<std::tuple_element_t<0, output_type>>;
        using value_type = remove_cvref_t<std::tuple_element_t<1, output_type>>;
        std::map<key_type, value_type> result;
        // Note: Sink only modifies the input if it actually is an rvalue reference.
        sink(std::forward<R>(range), result);
        return result;
    }
};

/*!
    @brief Convert range into std::set.
*/
struct to_set {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const {
        using output_type = remove_cvref_t<get_output_type_of_t<R>>;
        std::set<output_type> result;
        // Note: Sink only modifies the input if it actually is an rvalue reference.
        sink(std::forward<R>(range), result);
        return result;
    }
};

/*!
    @brief Count number of outputs of the input range.
*/
struct count {
    template <class R>
    [[nodiscard]] constexpr size_t operator()(R&& range) const {
        using range_type = remove_cvref_t<decltype(as_input_range(std::forward<R>(range)))>;
        RX_TYPE_ASSERT(is_finite<range_type>);
        // Copying for reentrancy.
        auto copy = as_input_range(std::forward<R>(range));
        size_t n = 0;
        while (RX_LIKELY(!copy.at_end())) {
            n += 1;
            copy.next();
        }
        return n;
    }
};

/*!
    @brief Binary left-fold of a range.

    Produces a single value created from consecutive calls to the provided function.

    @tparam T The type of the initial value and internal accumulator.
    @tparam F The
*/
template <class T, class F>
struct foldl {
    T init;
    F func;

    constexpr foldl(T init, F func)
        : init(std::move(init)), func(std::move(func)) {}

    template <class InputRange>
    constexpr T operator()(InputRange&& input) {
        // Copying for reentrancy.
        auto copy = as_input_range(std::forward<InputRange>(input));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        T accum = init;
        if (RX_UNLIKELY(copy.at_end()))
            return accum;
        do {
            accum = func(accum, copy.get());
            copy.next();
        } while (RX_LIKELY(!copy.at_end()));
        return accum;
    }
};
template <class T, class F>
foldl(T&&, F &&)->foldl<remove_cvref_t<T>, remove_cvref_t<F>>;

/*!
    @brief Sum the input range.

    The output type of the input range is required to implement `operator+`.
*/
struct sum {
    constexpr sum() = default;
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) const noexcept {
        using type = remove_cvref_t<get_output_type_of_t<R>>;
        // Note: foldl is reentrant.
        auto folder = foldl(
            type{}, [](type accum, auto&& x) constexpr { return accum + x; });
        return std::move(folder)(std::forward<R>(input));
    }
};

/*!
    @brief Produce the max value of the input range as an optional.

    If the input is empty, "none" is returned.

    The output type of the input range is compared using an instance of Compare, which is
    `std::less<void>` by default.

    @tparam Compare The comparison function.
*/
template <class Compare = std::less<void>>
struct max : private Compare {
    constexpr max(Compare cmp) noexcept : Compare(std::move(cmp)) {}
    constexpr max() = default;

    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const noexcept {
        using type = remove_cvref_t<get_output_type_of_t<R>>;
        decltype(auto) input = as_input_range(std::forward<R>(range));
        using range_type = decltype(input);
        if (RX_LIKELY(!input.at_end())) {
            type first = input.get();
            input.next();
            auto folder = foldl(
                std::move(first), [this](auto&& accum, auto&& x) constexpr {
                    // Note: Can't use std::max(), because it takes the comparison function
                    // by-value.
                    const Compare& cmp = *this;
                    const auto& accum_ = accum;
                    const auto& x_ = x;
                    return cmp(x_, accum_) ? std::forward<decltype(accum)>(accum)
                                           : std::forward<decltype(x)>(x);
                });
            return RX_OPTIONAL<type>{std::move(folder)(std::forward<range_type>(input))};
        } else {
            // GCC 9.1 mistakenly thinks the return value is uninitialized unless we declare it as a
            // local first.
            const RX_OPTIONAL<type> none;
            return none;
        }
    }
};
max()->max<>;

template <class Compare = std::less<void>>
struct min : private Compare {
    constexpr min(Compare cmp) noexcept : Compare(std::move(cmp)) {}
    constexpr min() = default;

    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const noexcept {
        using type = remove_cvref_t<get_output_type_of_t<R>>;
        decltype(auto) input = as_input_range(std::forward<R>(range));
        using range_type = decltype(input);
        if (RX_LIKELY(!input.at_end())) {
            auto folder = foldl(
                type{}, [this](auto&& accum, auto&& x) constexpr {
                    // Note: Can't use std::min(), because it takes the comparison function
                    // by-value.
                    const Compare& cmp = *this;
                    const auto& accum_ = accum;
                    const auto& x_ = x;
                    return cmp(accum_, x_) ? std::forward<decltype(accum)>(accum)
                                           : std::forward<decltype(x)>(x);
                });
            return RX_OPTIONAL<type>{std::move(folder)(std::forward<range_type>(input))};
        } else {
            // GCC 9.1 mistakenly thinks the return value is uninitialized unless we declare it as a
            // local first.
            const RX_OPTIONAL<type> none;
            return none;
        }
    }
};
min()->min<>;

/*!
    @brief Checks if the predicate returns true for any element in the input range.

    @tparam P An object callable with the result of the input range, returning a boolean.
*/
template <class P>
struct any_of {
    P pred;

    constexpr explicit any_of(P pred) : pred(std::move(pred)) {}

    template <class R>
    [[nodiscard]] constexpr bool operator()(R&& range) const {
        // Copying for reentrancy.
        auto copy = as_input_range(std::forward<R>(range));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        while (RX_LIKELY(!copy.at_end())) {
            if (pred(copy.get())) {
                return true;
            }
            copy.next();
        }
        return false;
    }
};
template <class P>
any_of(P &&)->any_of<P>;

/*!
    @brief Checks if the predicate returns true for all elements in the input range.

    @tparam P An object callable with the result of the input range, returning a boolean.
*/
template <class P>
struct all_of {
    P pred;

    constexpr explicit all_of(P pred) : pred(std::move(pred)) {}

    template <class R>
    [[nodiscard]] constexpr bool operator()(R&& range) {
        // Copying for reentrancy.
        auto copy = as_input_range(std::forward<R>(range));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        while (RX_LIKELY(!copy.at_end())) {
            if (!pred(copy.get())) {
                return false;
            }
            copy.next();
        }
        return true;
    }
};
template <class P>
all_of(P &&)->all_of<P>;

/*!
    @brief Checks that the predicate returns false for all elements in the input range.

    @tparam P An object callable with the result of the input range, returning a boolean.
*/
template <class P>
struct none_of {
    P pred;
    constexpr explicit none_of(P pred) : pred(std::move(pred)) {}

    template <class R>
    [[nodiscard]] constexpr bool operator()(R&& range) {
        // Copying for reentrancy.
        auto copy = as_input_range(std::forward<R>(range));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        while (RX_LIKELY(!copy.at_end())) {
            if (pred(copy.get())) {
                return false;
            }
            copy.next();
        }
        return true;
    }
};
template <class P>
none_of(P &&)->none_of<P>;

/*!
    @brief Perform action for each element of the input range.

    This is usually redundant, since normal range-based for loops are supported, but in some cases
    it can yield more readable code, especially for long chains of combinators.
*/
template <class F>
struct for_each {
    F func;
    constexpr explicit for_each(F func) : func(std::move(func)) {}

    template <class R>
    constexpr void operator()(R&& range) {
        // Copying for reentrancy.
        auto copy = as_input_range(std::forward<R>(range));
        while (RX_LIKELY(!copy.at_end())) {
            func(copy.get());
            copy.next();
        }
    }
};
template <class F>
for_each(F &&)->for_each<remove_cvref_t<F>>;

/*!
    @brief Append elements from the range into an existing container.
*/
template <class C>
struct append {
    C& out;
    constexpr explicit append(C& out) : out(out) {}

    template <class R>
    constexpr C& operator()(R&& range) {
        // Note: sink() only advances the input range if it is actually an rvalue reference.
        sink(std::forward<R>(range), out);
        return out;
    }
};
template <class C>
struct append<C&&> {
    C out;
    constexpr explicit append(C&& out) : out(std::move(out)) {}

    template <class R>
    [[nodiscard]] constexpr C operator()(R&& range) && {
        // Note: sink() only advances the input range if it is actually an rvalue reference.
        sink(std::forward<R>(range), out);
        return std::move(out);
    }
};
template <class C>
append(C&)->append<C>;
template <class C>
append(C &&)->append<C&&>;

/// Sorting sink.
///
/// Writes the result of the inner range to the output, and sorts the output container.
template <class Compare = std::less<void>>
struct sort : private Compare {
    constexpr explicit sort(Compare cmp) noexcept : Compare(std::move(cmp)) {}
    constexpr sort() = default;

    template <class InputRange>
    struct Range : private Compare {
        using output_type = get_output_type_of_t<InputRange>;
        InputRange input;
        constexpr Range(InputRange input, Compare cmp)
            : Compare(std::move(cmp)), input(std::move(input)) {}

        template <class Out>
            constexpr void sink(Out& out) && noexcept {
            using RX_NAMESPACE::sink; // enable ADL
            sink(std::move(input), out);
            // Note: This indirection is only required because GNU libstdc++ implements sort() in a
            // way that requires the predicate to be copy-constructible.
            using compare_type = remove_cvref_t<output_type>;
            struct indirection {
                const Compare* cmp;
                constexpr explicit indirection(const Compare* cmp) : cmp(cmp) {}
                constexpr bool operator()(const compare_type& lhs, const compare_type& rhs) const
                    noexcept {
                    return (*cmp)(lhs, rhs);
                }
            };
            std::sort(begin(out), end(out), indirection(this));
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        // Note: We don't want to use as_input_range(), because it would copy the input when
        // chaining sinks.
        using Inner = remove_cvref_t<InputRange>;
        const Compare& cmp = *this;
        return Range<Inner>{std::forward<InputRange>(input), cmp};
    }

    template <class InputRange>
        [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        using Inner = remove_cvref_t<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input), static_cast<Compare&&>(*this)};
    }
};
template <class Compare>
sort(Compare &&)->sort<remove_cvref_t<Compare>>;
sort()->sort<>;

/// Unique-elements sink.
///
/// Writes the result of the inner range to the output, calls std::unique() on the output container,
/// and erases the remaining elements from the output.
///
/// Note: std::unique() only eliminates contiguous sequences of equal elements. If all duplicates
/// should be removed, sort the range first.
template <class Compare = std::equal_to<void>>
struct uniq : private Compare {
    constexpr explicit uniq(Compare cmp) noexcept : Compare(std::move(cmp)) {}
    constexpr uniq() = default;

    template <class InputRange>
    struct Range : private Compare {
        using output_type = get_output_type_of_t<InputRange>;
        InputRange input;
        constexpr Range(InputRange input, Compare cmp) noexcept
            : Compare(std::move(cmp)), input(std::move(input)) {}

        template <class Out>
            constexpr void sink(Out& out) && noexcept {
            using RX_NAMESPACE::sink; // enable ADL
            sink(std::move(input), out);
            const Compare& cmp = *this;
            auto remove_from = std::unique(begin(out), end(out), cmp);
            out.erase(remove_from, end(out));
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        // Note: We don't want to use as_input_range(), because it would copy the input when
        // chaining sinks.
        using Inner = remove_cvref_t<InputRange>;
        const Compare& cmp = *this;
        return Range<Inner>{std::forward<InputRange>(input), cmp};
    }

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        // Note: We don't want to use as_input_range(), because it would copy the input when
        // chaining sinks.
        using Inner = remove_cvref_t<InputRange>;
        Compare& cmp = *this;
        return Range<Inner>{std::forward<InputRange>(input), std::move(cmp)};
    }
};
template <class Compare>
uniq(Compare &&)->uniq<remove_cvref_t<Compare>>;
uniq()->uniq<>;

/// Reverse range sink.
///
/// Writes the result of the inner range to the output, then reverses the order of the elements
/// using `std::reverse()`.
///
/// Note: Contrary to other range implementations, this requires storage. If the output is sinked
/// into a container, the storage for that container will be used, but otherwise temporary storage
/// will be allocated.
///
/// If multiple sinks are chained, they will reuse the same storage (i.e., `sort() | reverse()` will
/// operate on the same output container, modifying it in turn).
struct reverse {
    constexpr reverse() noexcept {}

    template <class InputRange>
    struct Range {
        using output_type = get_output_type_of_t<InputRange>;
        InputRange input;
        constexpr explicit Range(InputRange input) : input(std::move(input)) {}

        template <class Out>
            constexpr void sink(Out& out) && noexcept {
            using RX_NAMESPACE::sink; // enable ADL
            sink(std::move(input), out);
            std::reverse(begin(out), end(out));
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const noexcept {
        // Note: We don't want to use as_input_range(), because it would copy the input when
        // chaining sinks.
        using Inner = remove_cvref_t<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input)};
    }
};

/// A range that is always empty.
template <class T>
struct empty_range : public iterator_range<T*> {
    constexpr empty_range() noexcept : iterator_range<T*>(nullptr, nullptr) {}
};
empty_range()->empty_range<void*>;

template <class... Rs>
struct ChainRange {
    static_assert(sizeof...(Rs) >= 2);

    static constexpr bool is_finite = (is_finite_v<Rs> && ...);
    static constexpr bool is_idempotent = (is_idempotent_v<Rs> && ...);
    using output_type = std::common_type_t<typename Rs::output_type...>;

    std::tuple<Rs...> inputs;
    size_t current_input = 0;

    constexpr explicit ChainRange(std::tuple<Rs...> inputs) : inputs(std::move(inputs)) {
        _skip_to_data(std::make_index_sequence<sizeof...(Rs)>());
    }

    [[nodiscard]] constexpr output_type get() const {
        return _get(std::make_index_sequence<sizeof...(Rs)>());
    }

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return current_input >= sizeof...(Rs);
    }

    constexpr void next() noexcept {
        return _next(std::make_index_sequence<sizeof...(Rs)>());
    }

    [[nodiscard]] constexpr size_t size_hint() const noexcept {
        return _size_hint(std::make_index_sequence<sizeof...(Rs)>());
    }

    constexpr size_t advance_by(size_t by) noexcept {
        return _advance_by(std::make_index_sequence<sizeof...(Rs)>(), by);
    }

private:
    constexpr output_type _get(std::index_sequence<>) const {
        RX_ASSERT(false); // chain was at end!
        return output_type{};
    }

    template <size_t i, size_t... rest>
    constexpr output_type _get(std::index_sequence<i, rest...>) const {
        if (i == current_input) {
            return std::get<i>(inputs).get();
        } else {
            return _get(std::index_sequence<rest...>{});
        }
    }

    constexpr void _next(std::index_sequence<>) noexcept {
        RX_ASSERT(false); // chain was at end!
    }

    template <size_t i, size_t... rest>
    constexpr void _next(std::index_sequence<i, rest...>) noexcept {
        if (i == current_input) {
            auto& input = std::get<i>(inputs);
            input.next();
            if (input.at_end()) {
                ++current_input;
                _skip_to_data(std::index_sequence<rest...>{});
            }
        } else {
            _next(std::index_sequence<rest...>{});
        }
    }

    template <size_t... indices>
    constexpr size_t _size_hint(std::index_sequence<indices...>) const noexcept {
        return (std::get<indices>(inputs).size_hint() + ...);
    }

    constexpr size_t _advance_by(std::index_sequence<>, size_t) noexcept {
        return 0;
    }

    template <size_t i, size_t... rest>
    constexpr size_t _advance_by(std::index_sequence<i, rest...>, size_t by) noexcept {
        using RX_NAMESPACE::advance_by;
        if (i == current_input) {
            auto& input = std::get<i>(inputs);
            size_t advanced = advance_by(input, by);
            size_t remainder = by - advanced;
            if (input.at_end()) {
                ++current_input;
                return advanced + _advance_by(std::index_sequence<rest...>{}, remainder);
            } else {
                return advanced;
            }
        } else {
            return _advance_by(std::index_sequence<rest...>{}, by);
        }
    }

    constexpr void _skip_to_data(std::index_sequence<>) noexcept {
        // Reached end, do nothing.
    }

    template <size_t i, size_t... rest>
    constexpr void _skip_to_data(std::index_sequence<i, rest...>) noexcept {
        const auto& input = std::get<i>(inputs);
        if (input.at_end()) {
            ++current_input;
            _skip_to_data(std::index_sequence<rest...>{});
        }
    }
};
template <class... Rs>
ChainRange(Rs&&...) -> ChainRange<remove_cvref_t<Rs>...>;

/// Return values from multiple ranges.
///
/// Once the first range is exhausted, values from the second range are returned, and so on.
template <class First, class... InputRanges>
[[nodiscard]] constexpr auto chain(First&& first, InputRanges&&... inputs) noexcept {
    if constexpr (sizeof...(InputRanges) == 0) {
        return as_input_range(std::forward<First>(first));
    } else {
        return ChainRange<get_range_type_t<First>, get_range_type_t<InputRanges>...>{
            std::forward_as_tuple(as_input_range(std::forward<First>(first)), as_input_range(std::forward<InputRanges>(inputs))...)};
    }
}
[[nodiscard]] constexpr auto chain() noexcept {
    return empty_range();
}

/// Create an infinite range repeating the input elements in a loop.
///
/// If the input is empty, the output is empty, too. Note that passing an inifite range is
/// supported, but will never cycle.
struct cycle {
    template <class R>
    struct Range {
        using output_type = typename R::output_type;
        static constexpr bool is_finite = false;
        static constexpr bool is_idempotent = R::is_idempotent;

        const R prototype;
        RX_OPTIONAL<R> input; // rationale: most ranges in this library are not assignable

        static_assert(is_finite_v<R>);

        constexpr Range(R prototype_) : prototype(std::move(prototype_)) {
            if (RX_LIKELY(!prototype.at_end())) {
                input.emplace(prototype);
            }
        }

        [[nodiscard]] constexpr output_type get() const noexcept {
            RX_ASSERT(input && !input->at_end());
            return input->get();
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return !bool(input);
        }

        constexpr void next() noexcept {
            RX_ASSERT(!at_end());
            input->next();
            if (RX_UNLIKELY(input->at_end())) {
                input.emplace(prototype);
            }
        }

        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            return bool(input) ? std::numeric_limits<size_t>::max() : 0;
        }

        constexpr size_t advance_by(size_t n) noexcept {
            if (!input)
                return 0;
            using RX_NAMESPACE::advance_by;
            size_t remainder = n;
            while (remainder > 0) {
                size_t advanced = advance_by(*input, remainder);
                RX_ASSERT(advanced <= remainder);
                remainder -= advanced;
                if (RX_UNLIKELY(input->at_end())) {
                    input.emplace(prototype);
                }
            }
            return n;
        }

        // Note: advance_by() not specialized because we need to know when to wrap.
    };

    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) const {
        using Inner = get_range_type_t<R>;
        return Range<Inner>{as_input_range(std::forward<R>(input))};
    }
};

/// Yield an infinite list of constant values once the input range is exhausted.
template <class V>
struct padded {
    V value;
    explicit constexpr padded(V value) noexcept : value(std::move(value)) {}

    template <class R>
    struct Range {
        R input;
        const V value;

        using output_type = std::common_type_t<remove_cvref_t<get_output_type_of_t<R>>, V>;
        static constexpr bool is_finite = false;
        static constexpr bool is_idempotent = is_idempotent_v<R>;

        constexpr Range(R input, V value) noexcept
            : input(std::move(input)), value(std::move(value)) {}

        constexpr void next() {
            RX_ASSERT(!at_end());
            if (!input.at_end()) {
                input.next();
            }
        }

        [[nodiscard]] constexpr output_type get() const {
            RX_ASSERT(!at_end());
            if (!input.at_end()) {
                return input.get();
            } else {
                return value;
            }
        }

        [[nodiscard]] constexpr bool at_end() const {
            return false;
        }

        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            return std::numeric_limits<size_t>::max();
        }

        constexpr size_t advance_by(size_t n) noexcept {
            using RX_NAMESPACE::advance_by;
            size_t advanced = advance_by(input, n);
            return advanced % n;
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        using Inner = get_range_type_t<InputRange>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), value};
    }

    template <class InputRange>
        [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        using Inner = get_range_type_t<InputRange>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), std::move(value)};
    }
};
template <class V>
padded(V&)->padded<remove_cvref_t<V>>;
template <class V>
padded(V &&)->padded<remove_cvref_t<V>>;

template <class... Inputs>
struct ZipLongestRange {
    static_assert(sizeof...(Inputs) > 0);

    std::tuple<Inputs...> inputs;
    using output_type = std::tuple<RX_OPTIONAL<remove_cvref_t<typename Inputs::output_type>>...>;
    static constexpr bool is_finite = (is_finite_v<Inputs> && ...);
    static constexpr bool is_idempotent = (is_idempotent_v<Inputs> && ...);

    constexpr explicit ZipLongestRange(std::tuple<Inputs...> tuple) : inputs(std::move(tuple)) {}

    [[nodiscard]] constexpr output_type get() const noexcept {
        RX_ASSERT(!at_end());
        return _get(std::index_sequence_for<Inputs...>{});
    }

    constexpr void next() noexcept {
        RX_ASSERT(!at_end());
        _next(std::index_sequence_for<Inputs...>{});
    }

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return _at_end(std::index_sequence_for<Inputs...>{});
    }

    constexpr size_t size_hint() const noexcept {
        return _size_hint(std::index_sequence_for<Inputs...>{});
    }

    constexpr size_t advance_by(size_t n) noexcept {
        return _advance_by(std::index_sequence_for<Inputs...>{}, n);
    }

private:
    template <size_t... Index>
    [[nodiscard]] constexpr output_type _get(std::index_sequence<Index...>) const noexcept {
        return output_type(std::forward_as_tuple((std::get<Index>(inputs) | first())...));
    }

    template <size_t... Index>
    constexpr void _next(std::index_sequence<Index...>) noexcept {
        ((std::get<Index>(inputs).at_end() ? 0 : (std::get<Index>(inputs).next(), 0)), ...);
    }

    template <size_t... Index>
    [[nodiscard]] constexpr bool _at_end(std::index_sequence<Index...>) const noexcept {
        return (std::get<Index>(inputs).at_end() && ...);
    }

    template <size_t... Index>
    constexpr size_t _size_hint(std::index_sequence<Index...>) const noexcept {
        return std::max({std::get<Index>(inputs).size_hint()...});
    }

    template <size_t... Index>
    constexpr size_t _advance_by(std::index_sequence<Index...>, size_t n) noexcept {
        using RX_NAMESPACE::advance_by;
        return std::max({advance_by(std::get<Index>(inputs), n)...});
    }
};
template <class... Inputs>
ZipLongestRange(std::tuple<Inputs...>)->ZipLongestRange<remove_cvref_t<Inputs>...>;

/*!
    @brief Zip two or more ranges, return longest range.
    Until all of the ranges are at end, produce a tuple of an element from each range.
    The ranges are not required to produce the same number of elements, and elements will be
    produced until all of the ranges reach their end. Ranges that ended earlier produce nullopt.
*/
template <class... Inputs>
[[nodiscard]] constexpr auto zip_longest(Inputs&&... inputs) noexcept {
    return ZipLongestRange(std::forward_as_tuple(as_input_range(std::forward<Inputs>(inputs))...));
}

/// Copy values of a range into a container during iteration.
template <class Dest>
struct tee {
    Dest& dest;
    explicit constexpr tee(Dest& dest) noexcept : dest(dest) {}

    template <class R>
    struct Range {
        R input;
        Dest& dest;

        using output_type = get_output_type_of_t<R>;
        static constexpr bool is_finite = is_finite_v<R>;
        static constexpr bool is_idempotent = false;

        constexpr void next() {
            sink_one(input.get(), dest);
            input.next();
        }

        constexpr output_type get() const {
            return input.get();
        }

        constexpr bool at_end() const {
            return input.at_end();
        }

        constexpr size_t size_hint() const noexcept {
            return input.size_hint();
        }

        // Note: advance_by() not specialized because tee() needs to see every
        // element.
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const noexcept {
        using Inner = get_idempotent_range_type_t<InputRange>;
        return Range<Inner>{as_idempotent_input_range(std::forward<InputRange>(input)), dest};
    }
};
template <class Dest>
tee(Dest&)->tee<remove_cvref_t<Dest>>;

/// Return a range flattening one level of nesting in a range of ranges.
///
/// Supply a template parameter bigger than 1 to flatten multiple levels at once.
template <size_t Depth>
struct flatten;
flatten()->flatten<1>;

template <>
struct flatten<0> {
    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const noexcept {
        return std::forward<InputRange>(input);
    }
};

template <>
struct flatten<1> {
    template <class R>
    struct Range {
        using S = get_range_type_t<get_output_type_of_t<R>>;
        using output_type = get_output_type_of_t<S>;

        static constexpr bool is_finite = is_finite_v<R> && is_finite_v<S>;
        static constexpr bool is_idempotent = is_idempotent_v<S>;

        R outer_range;
        RX_OPTIONAL<S> inner_range;

        constexpr explicit Range(R input) : outer_range(std::move(input)) {
            while (RX_LIKELY(!outer_range.at_end())) {
                inner_range.emplace(as_input_range(outer_range.get()));
                if (RX_LIKELY(!inner_range->at_end())) {
                    return;
                }
            }
            inner_range.reset();
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return !bool(inner_range);
        }

        [[nodiscard]] constexpr output_type get() const noexcept {
            RX_ASSERT(!at_end());
            return inner_range->get();
        }

        constexpr void next() noexcept {
            RX_ASSERT(!at_end());

            inner_range->next();
            if (RX_LIKELY(!inner_range->at_end())) {
                return;
            }

            for (;;) {
                outer_range.next();
                if (RX_UNLIKELY(outer_range.at_end())) {
                    break;
                }

                inner_range.emplace(as_input_range(outer_range.get()));
                if (RX_LIKELY(!inner_range->at_end())) {
                    return;
                }
            }
            inner_range.reset();
        }

        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            if (at_end()) {
                return 0;
            }

            auto r = outer_range.size_hint();
            if (r == 0 || r == std::numeric_limits<size_t>::max()) {
                return r;
            }

            auto s = inner_range->size_hint();
            if (s == std::numeric_limits<size_t>::max()) {
                return s;
            } else if (s == 0) {
                // Only because this inner range is empty, does not mean all inner ranges are.
                // But either way, we have to assume the other ones have at least one element.
                return r;
            }

            auto rs = r * s;
            if (rs < r || rs < s) {
                // If the multiplication overflows, return the maximum.
                return std::numeric_limits<size_t>::max();
            } else {
                return rs;
            }
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const noexcept {
        using Inner = get_range_type_t<InputRange>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input))};
    }
};

template <size_t Depth>
struct flatten {
    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const noexcept {
        return std::forward<InputRange>(input) | flatten<1>() | flatten<Depth - 1>();
    }
};

/// A sink that simply discards all elements of a range.
///
/// Use as `append(null_sink())` if you need the range to run, but you don't need the result.
struct null_sink {
    struct value_type {};

    template <class... V>
    constexpr void emplace_back(V&&...) const noexcept {}
};

} // namespace RX_NAMESPACE

#endif // RX_RANGES_HPP_INCLUDED
