#ifndef RX_RANGES_HPP_INCLUDED
#define RX_RANGES_HPP_INCLUDED

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <type_traits>
#include <vector>


#if !defined(RX_NAMESPACE_OVERRIDE)
#define RX_NAMESPACE rx
#else
#define RX_NAMESPACE RX_NAMESPACE_OVERRIDE
#endif

#if !defined(RX_OPTIONAL_OVERRIDE)
#include <optional>
#define RX_OPTIONAL std::optional
#else
#define RX_OPTIONAL RX_OPTIONAL_OVERRIDE
#endif

#if !defined(RX_TYPE_ASSERT_OVERRIDE)
#define RX_TYPE_ASSERT(cond) static_assert(std::conjunction<cond>::value)
#else
#define RX_TYPE_ASSERT RX_TYPE_ASSERT_OVERRIDE
#endif

#if !defined(RX_ASSERT_OVERRIDE)
#include <cassert>
#define RX_ASSERT(cond) assert(cond)
#else
#define RX_ASSERT RX_ASSERT_OVERRIDE
#endif

#if !defined(RX_REMOVE_CVREF_OVERRIDE)
namespace RX_NAMESPACE::std_polyfill {
template <class T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;
} // namespace RX_NAMESPACE::std_polyfill
#define RX_REMOVE_CVREF_T RX_NAMESPACE::std_polyfill::remove_cvref_t
#else
#define RX_REMOVE_CVREF_T RX_REMOVE_CVREF_OVERRIDE
#endif


namespace RX_NAMESPACE {


/*!
    @brief InputRange concept

    An InputRange is expected to conform to the following interface:

    @code
    using output_type = ...; // The output element type, usually typename Inner::output_type.
    static constexpr bool is_finite = ...; // True if the range produces a bounded number of
                                           // elements.
    output_type get() const noexcept; // Get the value at the current position.
    void next() noexcept; // Advance to the next position, returning true if there was more.
    bool at_end() const noexcept; // True if there are no more elements.

    size_t size_hint() const noexcept; // A hint about how many times `next()` can be called.
                                       // It is purely an optimization hint, not the accurate number
                                       // of times.
    bool is_finite() const noexcept;
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
        decltype(std::declval<const T&>().get()),
        decltype(std::declval<const T&>().at_end()),
        decltype(std::declval<T&>().next()),
        decltype(T::is_finite)>> : std::true_type {};
template <class T>
constexpr bool is_input_range_v = is_input_range<T>::value;

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
template <class T, class Out, class Enable = void>
struct is_sink : std::false_type {};
template <class T, class Out>
struct is_sink<T, Out, std::void_t<decltype(std::declval<T&&>().sink(std::declval<Out&>()))>>
    : std::true_type {};
template <class T, class Out>
constexpr bool is_sink_v = is_sink<T, Out>::value;

template <class T, class Enable = void>
struct has_reserve : std::false_type {};
template <class T>
struct has_reserve<T, std::void_t<decltype(std::declval<T&>().reserve(std::declval<size_t>()))>>
    : std::true_type {};
template <class T>
constexpr bool has_reserve_v = has_reserve<T>::value;

template <class T, class Enable = void>
struct has_emplace_back : std::false_type {};
template <class T>
struct has_emplace_back<
    T,
    std::void_t<decltype(std::declval<T&>().emplace_back(
        std::declval<typename T::value_type&&>()))>> : std::true_type {};
template <class T>
constexpr bool has_emplace_back_v = has_emplace_back<T>::value;

template <class T, class Enable = void>
struct has_push_back : std::false_type {};
template <class T>
struct has_push_back<
    T,
    std::void_t<decltype(std::declval<T&>().push_back(std::declval<typename T::value_type&&>()))>>
    : std::true_type {};
template <class T>
constexpr bool has_push_back_v = has_push_back<T>::value;

template <class T, class Enable = void>
struct has_emplace : std::false_type {};
template <class T>
struct has_emplace<
    T,
    std::void_t<decltype(std::declval<T&>().emplace(std::declval<typename T::value_type&&>()))>>
    : std::true_type {};
template <class T>
constexpr bool has_emplace_v = has_emplace<T>::value;

template <class T>
struct is_tuple : std::false_type {};
template <class... Tx>
struct is_tuple<std::tuple<Tx...>> : std::true_type {};
template <class T, class U>
struct is_tuple<std::pair<T, U>> : std::true_type {};
template <class T>
constexpr bool is_tuple_v = is_tuple<T>::value;

template <class T, class Enable = void>
struct has_std_get_0_1 : std::false_type {};
template <class T>
struct has_std_get_0_1<
    T,
    std::void_t<
        decltype(std::get<0>(std::declval<const T&>())),
        decltype(std::get<1>(std::declval<const T&>()))>> : std::true_type {};
template <class T>
constexpr bool has_std_get_0_1_v = has_std_get_0_1<T>::value;

template <class T>
struct is_finite : std::bool_constant<T::is_finite> {};

// Fake RandomAccessIterator concept. Any iterator that supports advancing by an arbitrary integer
// amount is considered "random access".
template <class T, class Enable = void>
struct is_random_access_iterator : std::false_type {};
template <class T>
struct is_random_access_iterator<T, std::void_t<decltype(std::declval<T&>() += 1)>>
    : std::true_type {};
template <class T>
constexpr bool is_random_access_iterator_v = is_random_access_iterator<T>::value;

/// Idempotent conversion of rvalue InputRange to InputRange.
template <class R>
[[nodiscard]] constexpr decltype(auto) as_input_range(
    R&& range,
    std::enable_if_t<is_input_range_v<RX_REMOVE_CVREF_T<R>>>* = nullptr) noexcept {
    return std::forward<R>(range);
}

/// Convenience notation for chaining ranges.
///
/// Note: This operator just calles `rhs(lhs)` through `operator()`, and as such will apply to
/// any objects where the right-hand side is callable with the left-hand side as an argument.
///
/// Typically this is used to chain input ranges and sinks, but will also apply to the
/// following:
///
/// @code
/// std::string s = 123 | [](int x) { return std::to_string(x); };
/// // s == "123"
/// @endcode
template <
    class LHS,
    class RHS,
    class = std::void_t<decltype(std::declval<RHS>()(std::declval<LHS>()))>>
constexpr auto operator|(LHS&& lhs, RHS&& rhs) noexcept {
    return std::forward<RHS>(rhs)(std::forward<LHS>(lhs));
}

struct input_range_iterator_end {};
template <class R>
struct input_range_iterator {
    R range;
    template <class Arg>
    constexpr explicit input_range_iterator(Arg&& range) noexcept
        : range(std::forward<Arg>(range)) {}
    constexpr input_range_iterator(input_range_iterator&&) noexcept = default;
    constexpr input_range_iterator(const input_range_iterator&) noexcept = default;
    constexpr input_range_iterator& operator=(input_range_iterator&&) noexcept = default;
    constexpr input_range_iterator& operator=(const input_range_iterator&) noexcept = default;

    constexpr bool operator==(input_range_iterator_end) const {
        return range.at_end();
    }
    constexpr bool operator!=(input_range_iterator_end) const {
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
    template <class T = typename R::output_type, class = std::enable_if_t<std::is_lvalue_reference_v<T>>>
    [[nodiscard]] constexpr auto operator->() const noexcept {
        return &range.get();
    }
};
template <class R>
input_range_iterator(R &&)->input_range_iterator<RX_REMOVE_CVREF_T<R>>;

template <class R>
[[nodiscard]] constexpr auto
begin(R&& range, std::enable_if_t<is_input_range_v<RX_REMOVE_CVREF_T<R>>>* = nullptr) noexcept {
    return input_range_iterator{as_input_range(std::forward<R>(range))};
}
template <class R>
[[nodiscard]] constexpr auto
end(const R&, std::enable_if_t<is_input_range_v<R>>* = nullptr) noexcept {
    // Note: The first argument may be moved-from, but that's OK, we just need its type.
    return input_range_iterator_end{};
}

namespace detail {
    template <class T>
    struct invalid_type {};
} // namespace detail

/// Copy elements from an rvalue InputRange to output.
///
/// @tparam In The range to copy from.
/// @tparam Out A container that can hold the result.
template <class In, class Out>
constexpr void sink(
    In&& in,
    Out& out,
    std::enable_if_t<!std::is_lvalue_reference_v<In> && is_input_range_v<RX_REMOVE_CVREF_T<In>>>* =
        nullptr) noexcept {
    RX_TYPE_ASSERT(is_finite<RX_REMOVE_CVREF_T<In>>);
    using output_type = RX_REMOVE_CVREF_T<typename RX_REMOVE_CVREF_T<In>::output_type>;

    if constexpr (has_reserve_v<Out>) {
        out.reserve(in.size_hint());
    }

    // Copy elements from the input to the output. If the input is tuple-like, and the output
    // supports emplacement, the tuple will be unpacked and passed as individual arguments to the
    // emplace member function on the output. Otherwise, the tuple-like object will be passed as-is.

    while (!in.at_end()) {
        if constexpr (has_emplace_back_v<Out>) {
            if constexpr (is_tuple_v<output_type>) {
                // Output has emplace_back, and the input generates tuple-like elements. Pass tuple
                // elements as individual arguments to emplace_back.
                auto unpack = [&](auto&&... args) {
                    out.emplace_back(std::forward<decltype(args)>(args)...);
                };
                std::apply(unpack, in.get());
            } else {
                out.emplace_back(in.get());
            }
        } else if constexpr (has_push_back_v<Out>) {
            out.push_back(in.get());
        } else if constexpr (has_emplace_v<Out>) {
            if constexpr (is_tuple_v<output_type>) {
                // Output has emplace, and the input generates tuple-like elements. Pass tuple
                // elements as individual arguments to emplace.
                auto unpack = [&](auto&&... args) {
                    out.emplace(std::forward<decltype(args)>(args)...);
                };
                std::apply(unpack, in.get());
            } else {
                out.emplace(in.get());
            }
        }
        // else if constexpr (!std::is_same_v<Out, detail::invalid_type<output_type>>) {
        //     static_assert(
        //         std::is_same_v<Out, detail::invalid_type<output_type>>, "Output supports neither
        //         emplace_back(), push_back(), nor emplace().");
        // }

        in.next();
    }
}

/// Copy elements from an lvalue InputRange to sink with push_into().
template <class In, class Out>
constexpr void
sink(const In& in, Out& out, std::enable_if_t<is_input_range_v<In>>* = nullptr) noexcept {
    In copy = in;
    sink(std::move(copy), out);
}

/// Copy resulting elements from an rvalue sink implementation into the output container.
///
/// This function is what allows operations that require temporary storage to work on the resulting
/// container without allocating their own temporary storage.
///
/// @tparam In The sink to copy from, typically an output modifier like `sort` or `uniq`.
/// @tparam Out A container that can hold the result.
template <class In, class Out>
constexpr void sink(
    In&& in,
    Out& out,
    std::enable_if_t<!std::is_lvalue_reference_v<In> && is_sink_v<RX_REMOVE_CVREF_T<In>, Out>>* =
        nullptr) noexcept {
    std::move(in).sink(out);
}

/// Copy resulting elements from an lvalue range, copying the input range before modifying it.
///
/// Note: This function should not be called from a Sink's implementation of `sink(out)`. Instead,
/// call `sink(std::move(inner), out)` to avoid unnecessary copies.
template <class In, class Out>
constexpr void
sink(const In& in, Out& out, std::enable_if_t<is_sink_v<In, Out>>* = nullptr) noexcept {
    In copy = in;
    sink(std::move(copy), out, nullptr);
}

/// Copy elements from a standard container into a sink.
template <class In, class Out>
constexpr void sink(
    const In& in,
    Out& out,
    std::enable_if_t<!is_sink_v<In, Out> && !is_input_range_v<In>>* = nullptr) noexcept {
    std::copy(begin(in), end(in), std::back_inserter(out));
}

template <class It, class EndIt = It>
struct IteratorRange {
    using iterator_type = It;
    using end_iterator_type = EndIt;
    using output_type = decltype(*std::declval<const iterator_type&>());

    // TODO: If the end-iterator type is different from the iterator type, this may not produce
    // a finite range.
    static constexpr bool is_finite = true;

    It current_;
    EndIt end_;

    template <class C>
    constexpr explicit IteratorRange(const C& collection) noexcept
        : current_(begin(collection)), end_(end(collection)) {}
    constexpr IteratorRange(It begin, EndIt end) noexcept : current_(begin), end_(end) {}

    constexpr void next() noexcept {
        RX_ASSERT(current_ != end_);
        ++current_;
    }

    [[nodiscard]] constexpr output_type get() const noexcept {
        return *current_;
    }
    [[nodiscard]] constexpr bool at_end() const noexcept {
        return current_ == end_;
    }
    constexpr size_t size_hint() const noexcept {
        if constexpr (std::is_same_v<It, EndIt> && is_random_access_iterator_v<It>) {
            return end_ - current_;
        }
        return 0;
    }
};
template <class C>
IteratorRange(const C& c)->IteratorRange<decltype(begin(c)), decltype(end(c))>;
template <class It, class EndIt>
IteratorRange(It, EndIt)->IteratorRange<It, EndIt>;
/// Convert a standard collection to an IteratorRange.
template <class C>
[[nodiscard]] constexpr auto as_input_range(
    const C& collection,
    std::void_t<decltype(collection.begin()), decltype(collection.end())>* = nullptr) noexcept {
    return IteratorRange{collection};
}

template <size_t N, class T>
[[nodiscard]] constexpr auto as_input_range(T (&collection)[N]) noexcept {
    return IteratorRange{std::begin(collection), std::end(collection)};
}


// Sinks need to be able to indicate their output type, but don't want to actually convert their
// inputs to input ranges with as_input_range(), because that would allocate temporary storage when
// chaining sinks.
template <class T>
using get_output_type_of_t =
    typename RX_REMOVE_CVREF_T<decltype(as_input_range(std::declval<T>()))>::output_type;

template <class T>
struct VectorRange {
    using output_type = const T&;
    static constexpr bool is_finite = true;
    std::vector<T> vector_;
    // Note: Moving a vector does not invalidate iterators.
    typename std::vector<T>::const_iterator current_;

    constexpr explicit VectorRange(std::vector<T> vec) noexcept
        : vector_(std::move(vec)), current_(vector_.begin()) {}
    constexpr VectorRange(VectorRange&&) noexcept = default;
    constexpr VectorRange(const VectorRange& other) noexcept
        : vector_(other.vector_), current_(vector_.begin()) {}
    constexpr VectorRange& operator=(VectorRange&&) noexcept = default;
    constexpr VectorRange& operator=(const VectorRange& other) noexcept {
        vector_ = other.vector_;
        current_ = vector_.begin();
        return *this;
    }
    constexpr void next() noexcept {
        RX_ASSERT(current_ != vector_.end());
        ++current_;
    }
    constexpr output_type get() const noexcept {
        return *current_;
    }
    constexpr bool at_end() const noexcept {
        return current_ == vector_.end();
    }
    constexpr size_t size_hint() const noexcept {
        return vector_.size();
    }
};

/// Convert Sink to InputRange by copying elements into temporary storage.
template <
    class R,
    class = std::enable_if_t<
        !std::is_lvalue_reference_v<
            R> && is_sink_v<RX_REMOVE_CVREF_T<R>, std::vector<RX_REMOVE_CVREF_T<typename RX_REMOVE_CVREF_T<R>::output_type>>>>>
[[nodiscard]] constexpr auto as_input_range(R&& range) noexcept {
    // Store the output in a temporary vector.
    using output_type = RX_REMOVE_CVREF_T<typename RX_REMOVE_CVREF_T<R>::output_type>;
    std::vector<output_type> vec;
    sink(std::move(range), vec);
    return VectorRange<output_type>{std::move(vec)};
}

/*!
    @brief Generic range generator.

    Produces an infinite number of outputs, calling F to produce each output.

    Combine with things like \a take or \a until to create a finite range.

    @tparam F A function that will be invoked to produce an output.
*/
template <class F>
struct generate {
    mutable F func;

    using output_type = decltype(func());
    static constexpr bool is_finite = false;
    template <class T>
    constexpr explicit generate(T&& func) : func(std::forward<T>(func)) {}

    constexpr void next() noexcept {}
    [[nodiscard]] constexpr bool at_end() const noexcept {
        return false;
    }
    [[nodiscard]] constexpr output_type get() const {
        return func();
    }
    constexpr size_t size_hint() const noexcept {
        return 0;
    }
};
template <class F>
generate(F &&)->generate<RX_REMOVE_CVREF_T<F>>;

/*!
    @brief Sequence generator

    Create an infinite sequence of T.

    T is expected to be initializable by an integral constant, and to have well-defined
    `operator+=(T)`.
*/
template <class T>
struct seq {
    T current;
    T step;

    using output_type = T;
    static constexpr bool is_finite = false;

    constexpr explicit seq(T init = 0, T step = 1) : current(init), step(step) {}

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
        return 0;
    }
};
seq()->seq<int>;
template <class T>
seq(T)->seq<T>;
template <class T>
seq(T, T)->seq<T>;

/*!
    @brief Generate infinite copies of type T.
*/
template <class T>
struct repeat {
    T value;
    constexpr explicit repeat(T value) noexcept : value(std::move(value)) {}

    using output_type = T;
    static constexpr bool is_finite = false;
    constexpr void next() {}
    [[nodiscard]] constexpr output_type get() const {
        return value;
    }
    [[nodiscard]] constexpr bool at_end() const noexcept {
        return false;
    }
    constexpr size_t size_hint() const noexcept {
        return 0;
    }
};
template <class T>
repeat(T &&)->repeat<T>;

template <class T>
[[nodiscard]] constexpr auto fill(T&& v) {
    return repeat(std::forward<T>(v));
}

/*!
    @brief Generate N copies of type T.

    This is equivalent to `fill(value) | take(n)`.
*/
template <class T>
struct repeat_n {
    using output_type = T;
    static constexpr bool is_finite = true;

    T value;
    size_t n;
    size_t i = 0;
    constexpr explicit repeat_n(size_t n, T value) : value(std::move(value)), n(n) {}

    constexpr void next() {
        ++i;
    }
    [[nodiscard]] constexpr output_type get() const {
        return value;
    }
    [[nodiscard]] constexpr bool at_end() const {
        return i == n;
    }
    constexpr size_t size_hint() const noexcept {
        return n;
    }
};
template <class T>
repeat_n(size_t, T &&)->repeat_n<T>;

template <class T>
[[nodiscard]] constexpr auto fill_n(size_t n, T&& v) {
    return repeat_n(n, std::forward<T>(v));
}

template <class F>
struct transform {
    F func;
    explicit constexpr transform(F func) noexcept : func(std::move(func)) {}

    template <class InputRange>
    struct Range {
        InputRange input_;
        transform transform_;
        using output_type = decltype(std::declval<const F&>()((input_.get())));
        static constexpr bool is_finite = InputRange::is_finite;

        constexpr Range(InputRange input, transform transform) noexcept
            : input_(std::move(input)), transform_(std::move(transform)) {}
        constexpr void next() {
            input_.next();
        }
        constexpr output_type get() const {
            return transform_.func(input_.get());
        }
        constexpr bool at_end() const {
            return input_.at_end();
        }
        constexpr size_t size_hint() const noexcept {
            return input_.size_hint();
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), *this};
    }
    template <class InputRange>
        [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), std::move(*this)};
    }
};
template <class F>
transform(F &&)->transform<RX_REMOVE_CVREF_T<F>>;

template <class F>
struct filter {
    F pred;
    constexpr explicit filter(F pred) : pred(std::move(pred)) {}

    template <class InputRange>
    struct Range {
        InputRange input_;
        filter filter_;

        using output_type = typename InputRange::output_type;
        static constexpr bool is_finite = InputRange::is_finite;

        constexpr Range(InputRange input, filter filter)
            : input_(std::move(input)), filter_(std::move(filter)) {
            while (!input_.at_end() && !filter_.pred(input_.get())) {
                input_.next();
            }
        }
        [[nodiscard]] constexpr output_type get() const noexcept {
            return input_.get();
        }
        constexpr void next() noexcept {
            do {
                input_.next();
            } while (!input_.at_end() && !filter_.pred(input_.get()));
        }
        [[nodiscard]] constexpr bool at_end() const noexcept {
            return input_.at_end();
        }
        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            return input_.size_hint();
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), *this};
    }
    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) && {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), std::move(*this)};
    }
};
template <class F>
filter(F &&)->filter<RX_REMOVE_CVREF_T<F>>;

/*!
    @brief Create a range that produces at most N elements from the beginning of its input.
*/
struct take {
    size_t n;
    constexpr explicit take(size_t n) noexcept : n(n) {}

    template <class InputRange>
    struct Range {
        InputRange input_;
        size_t i = 0;
        size_t n;
        using output_type = typename InputRange::output_type;
        static constexpr bool is_finite = true;

        constexpr Range(InputRange input, size_t n) noexcept : input_(std::move(input)), n(n) {}
        [[nodiscard]] constexpr output_type get() const noexcept {
            return input_.get();
        }
        constexpr void next() noexcept {
            RX_ASSERT(!input_.at_end());
            ++i;
            input_.next();
        }
        [[nodiscard]] constexpr bool at_end() const noexcept {
            return i >= n || input_.at_end();
        }
        constexpr size_t size_hint() const noexcept {
            return n;
        }
    };
    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), n};
    }
};

using first_n = take;

/*!
    @brief Create a range that skips the first N elements of its input.
*/
struct skip_n {
    size_t n;
    constexpr explicit skip_n(size_t n) noexcept : n(n) {}

    template <class InputRange>
    struct Range {
        InputRange input_;
        size_t i_ = 0;
        const size_t n_;
        using output_type = typename InputRange::output_type;
        static constexpr bool is_finite = InputRange::is_finite;

        constexpr Range(InputRange input, size_t n) noexcept : input_(std::move(input)), n_(n) {
            while (!input_.at_end() && i_ < n_) {
                input_.next();
                ++i_;
            }
        }
        [[nodiscard]] constexpr output_type get() const noexcept {
            return input_.get();
        }
        constexpr void next() noexcept {
            input_.next();
        }
        constexpr bool at_end() const noexcept {
            return input_.at_end();
        }
        constexpr size_t size_hint() const noexcept {
            return input_.size_hint();
        }
    };
    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), n};
    }
};

/*!
    @brief Produce elements from the input until the predicate returns true.

    Note: The element for which the predicate returned true is not included in the output.

    Note: If the predicate never returns true, this will loop forever!

    @tparam P A predicate function callable with elements of the input as its argument, returning a
              boolean.
*/
template <class P>
struct until {
    P pred;
    template <class T>
    constexpr explicit until(T&& pred) : pred(std::forward<T>(pred)) {}

    template <class InputRange>
    struct Range {
        InputRange input;
        P pred;
        bool end = false;

        // Temporary storage of the current element of the input is necessary to ensure that the
        // range is reentrant (since we need to call the predicate in next()).
        using storage_t = RX_REMOVE_CVREF_T<typename InputRange::output_type>;
        storage_t storage;

        constexpr Range(InputRange input, P pred) noexcept
            : input(std::move(input)), pred(std::move(pred)), end(input.at_end()) {
            if (!end) {
                storage = input.get();
                end = pred(storage);
            }
        }
        using output_type = typename InputRange::output_type;
        static constexpr bool is_finite = true;
        [[nodiscard]] constexpr output_type get() const noexcept {
            RX_ASSERT(!end);
            return storage;
        }
        constexpr void next() noexcept {
            RX_ASSERT(!end);
            input.next();
            end = input.at_end();
            if (!end) {
                storage = input.get();
                end = pred(storage);
            }
        }
        [[nodiscard]] constexpr bool at_end() const noexcept {
            return end;
        }
        constexpr size_t size_hint() const noexcept {
            return input.size_hint();
        }
    };

    template <class InputRange>
    constexpr auto operator()(InputRange&& input) const {
        using Inner = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<InputRange>(input)))>;
        return Range<Inner>{as_input_range(std::forward<InputRange>(input)), pred};
    }
};
template <class P>
until(P &&)->until<RX_REMOVE_CVREF_T<P>>;

template <class... Inputs>
struct ZipRange {
    std::tuple<Inputs...> inputs;
    using output_type = std::tuple<RX_REMOVE_CVREF_T<typename Inputs::output_type>...>;
    static constexpr bool is_finite = (false || ... || Inputs::is_finite);

    constexpr explicit ZipRange(Inputs... args) : inputs(std::move(args)...) {}

    [[nodiscard]] constexpr output_type get() const noexcept {
        return output_type(std::get<Inputs>(inputs).get()...);
    }

    constexpr void next() noexcept {
        int unused[] = {(std::get<Inputs>(inputs).next(), 0)..., 0};
        static_cast<void>(unused);
    }

    [[nodiscard]] constexpr bool at_end() const noexcept {
        return (false || ... || std::get<Inputs>(inputs).at_end());
    }

    constexpr size_t size_hint() const noexcept {
        return std::max({std::get<Inputs>(inputs).size_hint()...});
    }
};
template <class... Inputs>
ZipRange(Inputs&&...)->ZipRange<RX_REMOVE_CVREF_T<Inputs>...>;

/*!
    @brief Zip two or more ranges.

    Until none of the ranges are at end, produce a tuple of an element from each range.

    The ranges are not required to produce the same number of elements, but elements will only be
    produced until one of the ranges reaches its end.
*/
template <class... Inputs>
[[nodiscard]] constexpr auto zip(Inputs&&... inputs) noexcept {
    return ZipRange(as_input_range(std::forward<Inputs>(inputs))...);
}

/*!
    @brief Enumerate elements of the input sequentially.

    Equivalent to `zip(seq(), inputs...)`.

    The inputs are not required to produce the same number of elements, but elements will only be
    produced until one of the ranges reaches its end.
*/
template <class... Inputs>
[[nodiscard]] constexpr auto enumerate(Inputs&&... inputs) noexcept {
    return ZipRange(seq(), as_input_range(std::forward<Inputs>(inputs))...);
}

/*!
    @brief Sink element(s) into an optional.

    If the input produces 1 or more output, the last element produced will be placed in the result.

    If the input produces 0 outputs, the result will be nullopt.
*/
struct to_opt {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) noexcept {
        RX_OPTIONAL<RX_REMOVE_CVREF_T<typename R::output_type>> result;
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
        return input | take(1) | to_opt();
    }
};

struct to_vector {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& range) const {
        std::vector<RX_REMOVE_CVREF_T<get_output_type_of_t<R>>> vec;
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
        std::list<RX_REMOVE_CVREF_T<get_output_type_of_t<R>>> list;
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
        using key_type = RX_REMOVE_CVREF_T<std::tuple_element_t<0, output_type>>;
        using value_type = RX_REMOVE_CVREF_T<std::tuple_element_t<1, output_type>>;
        std::map<key_type, value_type> result;
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
        using output_type = RX_REMOVE_CVREF_T<get_output_type_of_t<R>>;
        std::set<output_type> result;
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
        using range_type = RX_REMOVE_CVREF_T<decltype(as_input_range(std::forward<R>(range)))>;
        RX_TYPE_ASSERT(is_finite<range_type>);
        auto copy = as_input_range(std::forward<R>(range));
        size_t n = 0;
        while (!copy.at_end()) {
            n += 1;
            copy.next();
        }
        return n;
    }
};

/*!
    @brief Fold-left of a range.
*/
template <class T, class F>
struct foldl {
    T init;
    F func;

    template <class U, class E>
    constexpr foldl(U&& init, E&& func)
        : init(std::forward<U>(init)), func(std::forward<E>(func)) {}

    template <class InputRange>
    constexpr T operator()(InputRange&& input) {
        auto copy = as_input_range(std::forward<InputRange>(input));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        T accum = init;
        while (!copy.at_end()) {
            accum = func(accum, copy.get());
            copy.next();
        }
        return accum;
    }
};
template <class T, class F>
foldl(T&&, F &&)->foldl<RX_REMOVE_CVREF_T<T>, RX_REMOVE_CVREF_T<F>>;

struct sum {
    constexpr sum() noexcept {}
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) noexcept {
        using type = get_output_type_of_t<R>;
        return std::forward<R>(input)
               | foldl(
                   type{}, [](type accum, auto&& x) constexpr { return accum + x; });
    }
};

struct max {
    constexpr max() noexcept {}
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) noexcept {
        using type = get_output_type_of_t<R>;
        auto folder = foldl(
            RX_OPTIONAL<type>{}, [](auto&& accum, auto&& x) constexpr {
                if (accum) {
                    return std::max(*accum, x);
                } else {
                    return x;
                }
            });
        return std::move(folder)(std::forward<R>(input));
    }
};

struct min {
    template <class R>
    [[nodiscard]] constexpr auto operator()(R&& input) noexcept {
        using type = RX_REMOVE_CVREF_T<get_output_type_of_t<R>>;
        auto folder = foldl(
            RX_OPTIONAL<type>{}, [](auto&& accum, auto&& x) constexpr {
                if (accum) {
                    return std::min(*accum, x);
                } else {
                    return x;
                }
            });
        return std::move(folder)(std::forward<R>(input));
    }
};

/*!
    @brief Checks if the predicate returns true for any element in the input range.

    @tparam P An object callable with the result of the input range, returning a boolean.
*/
template <class P>
struct any_of {
    P pred;

    template <class T>
    constexpr explicit any_of(T&& pred) : pred(std::forward<T>(pred)) {}

    template <class R>
    [[nodiscard]] constexpr bool operator()(R&& range) const {
        auto copy = as_input_range(std::forward<R>(range));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        while (!copy.at_end()) {
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

    template <class T>
    constexpr explicit all_of(T&& pred) : pred(std::forward<T>(pred)) {}

    template <class R>
    [[nodiscard]] constexpr bool operator()(R&& range) {
        auto copy = as_input_range(std::forward<R>(range));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        while (!copy.at_end()) {
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
    template <class T>
    constexpr explicit none_of(T&& pred) : pred(std::forward<T>(pred)) {}

    template <class R>
    [[nodiscard]] constexpr bool operator()(R&& range) {
        auto copy = as_input_range(std::forward<R>(range));
        RX_TYPE_ASSERT(is_finite<decltype(copy)>);
        while (!copy.at_end()) {
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
    @brief Append elements from the range into an existing container.
*/
template <class C>
struct append {
    C& out;
    constexpr explicit append(C& out) : out(out) {}

    template <class R>
    constexpr C& operator()(R&& range) {
        sink(std::forward<R>(range), out);
        return out;
    }
};
template <class C>
append(C&)->append<C>;

/// Sorting sink.
///
/// Writes the result of the inner range to the output, and sorts the output container.
template <class Compare = std::less<void>>
struct sort {
    Compare cmp;
    constexpr explicit sort(Compare cmp = Compare{}) noexcept : cmp(std::move(cmp)) {}

    template <class InputRange>
    struct Range {
        using output_type = get_output_type_of_t<InputRange>;
        InputRange input;
        Compare cmp;
        constexpr Range(InputRange input, Compare cmp)
            : input(std::move(input)), cmp(std::move(cmp)) {}

        template <class Out>
            constexpr void sink(Out& out) && noexcept {
            using RX_NAMESPACE::sink; // enable ADL
            sink(std::move(input), out);
            std::sort(begin(out), end(out), cmp);
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        using Inner = RX_REMOVE_CVREF_T<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input), cmp};
    }

    template <class InputRange>
        [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        using Inner = RX_REMOVE_CVREF_T<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input), std::move(cmp)};
    }
};
template <class Compare>
sort(Compare &&)->sort<RX_REMOVE_CVREF_T<Compare>>;
sort()->sort<>;

/// Unique-elements sink.
///
/// Writes the result of the inner range to the output, calls std::unique() on the output container,
/// and erases the remaining elements from the output.
///
/// Note: std::unique() only eliminates contiguous sequences of equal elements. If all duplicates
/// should be removed, sort the range first.
template <class Compare = std::equal_to<void>>
struct uniq {
    Compare cmp;
    constexpr explicit uniq(Compare cmp = Compare{}) noexcept : cmp(std::move(cmp)) {}

    template <class InputRange>
    struct Range {
        using output_type = get_output_type_of_t<InputRange>;
        InputRange input;
        Compare cmp;
        constexpr Range(InputRange input, Compare cmp) noexcept
            : input(std::move(input)), cmp(std::move(cmp)) {}

        template <class Out>
            constexpr void sink(Out& out) && noexcept {
            using RX_NAMESPACE::sink; // enable ADL
            sink(std::move(input), out);
            auto remove_from = std::unique(begin(out), end(out), cmp);
            out.erase(remove_from, end(out));
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const& noexcept {
        using Inner = RX_REMOVE_CVREF_T<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input), cmp};
    }

    template <class InputRange>
        [[nodiscard]] constexpr auto operator()(InputRange&& input) && noexcept {
        using Inner = RX_REMOVE_CVREF_T<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input), std::move(cmp)};
    }
};
template <class Compare>
uniq(Compare &&)->uniq<RX_REMOVE_CVREF_T<Compare>>;
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
            std::reverse(std::begin(out), std::end(out));
        }
    };

    template <class InputRange>
    [[nodiscard]] constexpr auto operator()(InputRange&& input) const noexcept {
        using Inner = RX_REMOVE_CVREF_T<InputRange>;
        return Range<Inner>{std::forward<InputRange>(input)};
    }
};

} // namespace RX_NAMESPACE

#endif // RX_RANGES_HPP_INCLUDED