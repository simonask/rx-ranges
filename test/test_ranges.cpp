#include <rx/ranges.hpp>

#include <algorithm>
#include <string>
#include <unordered_map>


#include <doctest/doctest.h>


using namespace rx;
using namespace std::literals::string_literals;

template <class T>
std::string to_string(T val) {
    return std::to_string(val);
}

// TEST_CASE("ranges operator| is general") {
//     // This is not necessarily something we actually want, but it is very convenient when
//     defining
//     // custom combinators.
//     std::string s = 123 | [](int x) { return to_string(x); };
//     CHECK(s == "123");
// }

TEST_CASE("range take advance_by overflow") {
    auto bounds = seq() | take(10);
    advance_by(bounds, 11);
    CHECK(bounds.i == bounds.n);

    auto arithmetic = seq() | take(10);
    advance_by(arithmetic, std::numeric_limits<size_t>::max());
    CHECK(arithmetic.i == arithmetic.n);
}

TEST_CASE("range transform") {
    auto input = std::vector{{1, 2, 3, 4}};
    auto strings = input | transform(&to_string<int>) | to_vector();
    auto expected = std::vector{{"1"s, "2"s, "3"s, "4"s}};
    CHECK(strings == expected);
}

TEST_CASE("range transform reentrant") {
    auto input = std::vector{{1, 2, 3, 4}};
    auto strings = input | transform(&to_string<int>);
    auto a = strings | to_vector();
    auto b = strings | to_vector();
    CHECK(a == b);
}

TEST_CASE("range filter") {
    auto input = std::list{{1, 2, 3, 4}};
    auto odd = input | filter([](int x) { return x % 2 == 1; }) | to_list();
    for (auto x : odd) {
        CHECK(x % 2 == 1);
    }
}

TEST_CASE("range filter reentrant") {
    auto input = std::list{{1, 2, 3, 4}};
    auto odd = input | filter([](int x) { return x % 2 == 1; });
    auto a = odd | to_vector();
    auto b = odd | to_vector();
    CHECK(a == b);
}

TEST_CASE("range filter idempotent") {
    auto input = std::vector{{1, 2, 3, 4}};
    int idempotent_guard{0};
    auto odd = input | transform([&idempotent_guard] (int i) { ++idempotent_guard; return i; }) | filter([](int x) { return x % 2 == 1; });
    auto a = odd | to_vector();
    CHECK(a == std::vector{{1,3}});
    CHECK(idempotent_guard == 4);
}

TEST_CASE("range first") {
    auto input = std::vector{{"Hello"s, "World"s, "Morty"s}};
    auto contains_y = [](std::string_view sv) { return sv.find('y') != std::string::npos; };
    RX_OPTIONAL morty = input | filter(contains_y) | first();
    CHECK(morty);
    CHECK(*morty == "Morty");
}

TEST_CASE("range first reentrant") {
    auto input = std::vector{{"Hello"s, "World"s, "Morty"s}};
    auto contains_y = [](std::string_view sv) { return sv.find('y') != std::string::npos; };
    auto range = input | filter(contains_y);
    auto a = range | first();
    auto b = range | first();
    CHECK(a == b);
}

TEST_CASE("range first_n") {
    auto input = std::vector{{1, 2, 3, 4, 5}};
    auto first_3 = input | first_n(3) | to_vector();
    CHECK(first_3.size() == 3);
    CHECK(first_3 == std::vector{{1, 2, 3}});
}

TEST_CASE("range first_n reentrant") {
    auto input = std::vector{{1, 2, 3, 4, 5}};
    auto first_3 = input | first_n(3);
    auto a = first_3 | to_vector();
    auto b = first_3 | to_vector();
    CHECK(a == b);
}

TEST_CASE("range skip_n") {
    auto input = seq() | skip_n(1000) | first_n(10) | to_vector();
    auto expected = seq(1000) | first_n(10) | to_vector();
    CHECK(input == expected);
}

TEST_CASE("ranges zip") {
    auto input1 = seq() | first_n(5);
    auto input2 = input1 | transform(&to_string<int>);
    auto input3 = seq(10); // inifinite range!
    auto zipped = zip(input1, input2, input3) | to_vector();
    CHECK(zipped.size() == 5);
    auto expected = std::vector{
        std::make_tuple(0, "0"s, 10),
        std::make_tuple(1, "1"s, 11),
        std::make_tuple(2, "2"s, 12),
        std::make_tuple(3, "3"s, 13),
        std::make_tuple(4, "4"s, 14),
    };
    CHECK(zipped == expected);
}

TEST_CASE("ranges zip two same") {
    auto add = [](auto lr) {
        auto [l, r] = lr;
        return l + r;
    };
    auto value = zip(seq(0), seq(1)) | first_n(5) | transform(add) | max();
    CHECK(value == 9);

    auto advancing = zip(seq(0), seq(1)) | transform(add);
    advance_by(advancing, 4);
    CHECK(advancing.get() == 9);
}

TEST_CASE("ranges zip advance_by") {
    auto input = zip(seq(), seq(1));
    advance_by(input, 10);
    CHECK(input.get() == std::tuple{10, 11});

    auto finite = zip(seq(), seq() | take(5));
    size_t advanced = advance_by(finite, 6);
    CHECK(advanced == 5);
}

TEST_CASE("ranges zip reentrant") {
    auto input1 = seq() | first_n(5);
    auto input2 = input1 | transform(&to_string<int>);
    auto input3 = seq(10); // inifinite range!
    auto zipped = zip(input1, input2, input3);
    auto a = zipped | to_vector();
    auto b = zipped | to_vector();
    CHECK(a == b);
}

TEST_CASE("ranges to_map") {
    auto input1 = seq();
    auto input2 = input1 | transform(&to_string<int>) | first_n(5);
    auto result = zip(input1, input2) | to_map();
    CHECK(result.size() == 5);
    auto expected = std::map<int, std::string>{{
        std::make_pair(0, "0"s),
        std::make_pair(1, "1"s),
        std::make_pair(2, "2"s),
        std::make_pair(3, "3"s),
        std::make_pair(4, "4"s),
    }};
    CHECK(result == expected);
}

TEST_CASE("ranges to_set") {
    auto input = std::vector{{0, 0, 1, 1}};
    auto result = as_input_range(input) | to_set();
    CHECK(result.size());
    auto expected = std::set{{0, 1}};
    CHECK(result == expected);
}

TEST_CASE("ranges append to arbitrary container") {
    std::unordered_map<double, std::string> result;
    auto keys = seq();
    auto values = keys | transform(&to_string<int>);
    zip(keys, values) | first_n(5) | append(result);
    auto expected = std::unordered_map<double, std::string>{{std::make_pair(0.0, "0"s),
                                                             std::make_pair(1.0, "1"s),
                                                             std::make_pair(2.0, "2"s),
                                                             std::make_pair(3.0, "3"s),
                                                             std::make_pair(4.0, "4"s)}};
    CHECK(result == expected);
}

TEST_CASE("range append to rvalue container") {
    auto lower = seq<char>('a') | take(26) | append(""s);
    CHECK(lower == "abcdefghijklmnopqrstuvwxyz"s);

    auto digits = seq() | take(10) | append(std::list(0,0));
    CHECK(digits == std::list{{0,1,2,3,4,5,6,7,8,9}});
}

TEST_CASE("ranges generate") {
    int x = 0;
    auto input = generate([&] { return x++; });
    auto result = input | first_n(5) | to_vector();
    auto expected = seq() | first_n(5) | to_vector();
    CHECK(result == expected);
}

TEST_CASE("ranges generate reentrant") {
    struct callable {
        int x = 0;
        int operator()() noexcept {
            return x++;
        }
    };
    // Check that the generator function is copied when sinking into a range.
    auto input = generate(callable{}) | first_n(5);
    auto a = input | to_vector();
    auto b = input | to_vector();
    CHECK(a == b);
}

TEST_CASE("ranges until") {
    auto input = seq() | until([](int x) { return x == 5; });
    auto result = input | to_vector();
    auto expected = seq() | first_n(5) | to_vector();
    CHECK(result == expected);
}

TEST_CASE("ranges any_of") {
    auto input = seq() | first_n(5);
    auto a = input | any_of([](int x) { return x > 3; });
    CHECK(a);
    auto b = input | any_of([](int x) { return x == 5; });
    CHECK(!b);
}

TEST_CASE("ranges all_of") {
    auto input = seq() | first_n(5);
    auto a = input | all_of([](int x) { return x < 5; });
    CHECK(a);
    auto b = input | all_of([](int x) { return x < 4; });
    CHECK(!b);
}

TEST_CASE("ranges none_of") {
    auto input = seq() | first_n(5);
    auto a = input | none_of([](int x) { return x > 4; });
    CHECK(a);
    auto b = input | none_of([](int x) { return x == 4; });
    CHECK(!b);
}

TEST_CASE("ranges avoid copy") {
    auto input = std::vector{{1, 2, 3, 4}};
    auto odd = input | filter([](int x) { return x % 2 == 1; });
    // modify the input to check that filtered range is not actually operating on a copy of the
    // vector. Note: filter() skips non-matching elements initially, which is a bit awkward.
    input[2] = 0;
    CHECK((odd | count()) == 1);
}

TEST_CASE("ranges count reentrant") {
    auto input = seq() | first_n(10);
    auto a = input | count();
    CHECK(a == 10);
    auto b = input | count();
    CHECK(b == 10);
}

TEST_CASE("ranges fill") {
    std::string a;
    fill_n(5, 'a') | append(a);
    CHECK(a == "aaaaa");

    std::string b;
    fill('b') | first_n(5) | append(b);
    CHECK(b == "bbbbb");

    int v = 7;
    CHECK((fill(v) | take(5) | sum()) == 7*5);
    CHECK(v == 7);
    CHECK((fill_n(5, v) | sum()) == 7*5);
    CHECK(v == 7);
}

TEST_CASE("ranges sum") {
    auto s = fill_n(5, 1) | sum();
    CHECK(s == 5);

    auto d = fill_n(5, 1.0) | sum();
    CHECK(d == 5.0);
}

TEST_CASE("ranges max") {
    auto s = seq() | first_n(5) | max();
    CHECK(*s == 4);
}

TEST_CASE("ranges min") {
    auto s = seq() | first_n(5) | min();
    CHECK(*s == 0);

    s = seq() | skip_n(1) | first_n(5) | min();
    CHECK(*s == 1);
}

TEST_CASE("ranges infinity propagates") {
    auto s =
        seq() | skip_n(1) | filter([](auto) { return true; }) | transform([](auto) { return 0; });
    CHECK(!decltype(s)::is_finite);
}

TEST_CASE("ranges enumerate with indices") {
    auto input = std::vector{{"a"s, "b"s, "c"s}};
    for (auto pair : zip(input, seq())) {
        if (std::get<0>(pair) == "a") {
            CHECK(std::get<1>(pair) == 0);
        } else if (std::get<0>(pair) == "b") {
            CHECK(std::get<1>(pair) == 1);
        } else if (std::get<0>(pair) == "c") {
            CHECK(std::get<1>(pair) == 2);
        } else {
            CHECK(false);
        }
    }

    auto a = zip(seq<size_t>(), input) | to_vector();
    auto b = enumerate(input) | to_vector();
    CHECK(a == b);
}

TEST_CASE("ranges sort") {
    // Check that we can use std algorithms directly.
    auto sorted = std::vector{{3, 2, 1}} | sort() | to_vector();
    CHECK(std::is_sorted(begin(sorted), end(sorted)));

    auto odd = [](auto x) { return x % 2 == 1; };

    // Chaining
    auto filtered_sorted = std::vector{{3, 2, 1}} | filter(odd) | sort() | to_vector();
    CHECK(std::is_sorted(begin(filtered_sorted), end(filtered_sorted)));
}

TEST_CASE("ranges reverse") {
    auto input = std::vector{{2, 3, 6, 1, 7, 8, 3, 4}};
    auto result = input | sort() | reverse() | to_vector();
    auto expected = std::vector{{8, 7, 6, 4, 3, 3, 2, 1}};
    CHECK(result == expected);
}

TEST_CASE("ranges in_groups_of_exactly, dynamic size") {
    auto input = seq<float>() | take(1001) | to_vector();

    size_t num_groups = input | in_groups_of_exactly(4) | count();
    CHECK(num_groups == 250);

    // In optimized builds, compilers should be able to auto-vectorize this.
    std::array<float, 4> sums = {0.f, 0.f, 0.f, 0.f};
    for (auto group : input | in_groups_of_exactly(4)) {
        std::get<0>(sums) += group.get();
        group.next();
        std::get<1>(sums) += group.get();
        group.next();
        std::get<2>(sums) += group.get();
        group.next();
        std::get<3>(sums) += group.get();
        group.next();
    }

    std::array<float, 4> expected_sums = {0.f, 0.f, 0.f, 0.f};
    for (auto [i, x] : enumerate(input)) {
        if (i != 1000)
            expected_sums[i % 4] += x;
    }

    CHECK(sums == expected_sums);
}

TEST_CASE("ranges in_groups_of_exactly advance_by") {
    auto input = seq() | in_groups_of_exactly(4);
    size_t advanced = advance_by(input, 3);  // already at the first group
    auto group = input.get();
    CHECK(group.get() == 16);
    CHECK(advanced == 3);

    auto finite = seq() | take(11) | in_groups_of_exactly(4);
    advanced = advance_by(finite, 2);
    CHECK(finite.at_end());
    CHECK(advanced == 1);
}

TEST_CASE("ranges in_groups_of") {
    const auto input = seq<float>() | take(1001) | to_vector();

    size_t num_groups_even = seq() | take(12) | in_groups_of(4) | count();
    CHECK(num_groups_even == 3);

    size_t num_groups = input | in_groups_of(4) | count();
    CHECK(num_groups == 251);

    std::array<float, 4> sums = {0.f, 0.f, 0.f, 0.f};
    float last = 0.f;
    auto groups = input | in_groups_of(4);

    for (auto&& group : groups) {
        CHECK(!group.at_end());
        size_t len = group | count();
        if (len == 4) {
            std::get<0>(sums) += group.get();
            group.next();
            std::get<1>(sums) += group.get();
            group.next();
            std::get<2>(sums) += group.get();
            group.next();
            std::get<3>(sums) += group.get();
            group.next();
        } else {
            do {
                last = group.get();
                group.next();
            } while (!group.at_end());
        }
    }

    std::array<float, 4> expected_sums = {0.f, 0.f, 0.f, 0.f};
    float expected_last = 0.f;
    for (auto [i, x] : enumerate(input)) {
        if (i < 1000) {
            expected_sums[i % 4] += x;
        } else {
            expected_last = x;
        }
    }

    CHECK(sums == expected_sums);
    CHECK(last == expected_last);
}

TEST_CASE("ranges in_groups_of advance_by") {
    auto input = seq() | in_groups_of(4);
    advance_by(input, 3);  // already at the first group
    auto group = input.get();
    CHECK(group.get() == 16);

    auto finite = seq() | take(11) | in_groups_of(4);
    size_t advanced = advance_by(finite, 2);
    CHECK(advanced == 2);
    advanced = advance_by(finite, 1);
    CHECK(advanced == 0);
    CHECK(finite.at_end());
}

TEST_CASE("ranges group_adjacent_by") {
    const auto input = seq() | take(10);

    auto pred = [](int x) { return x / 3; };

    auto groups = input | group_adjacent_by(pred);
    auto tmp = groups | to_vector();
    size_t num_groups = groups | count();
    CHECK(num_groups == 4);

    int previous = std::numeric_limits<int>::max();
    for (const auto& group : groups) {
        for (auto x : group) {
            CHECK(pred(x) == pred(group.get()));
            CHECK(pred(x) != previous);
        }
        previous = pred(group.get());
    }

    auto group_vectors = groups | transform(to_vector()) | to_vector();
    CHECK(group_vectors.size() == 4);
    CHECK(group_vectors[0] == std::vector{{0, 1, 2}});
    CHECK(group_vectors[1] == std::vector{{3, 4, 5}});
    CHECK(group_vectors[2] == std::vector{{6, 7, 8}});
    CHECK(group_vectors[3] == std::vector(1, 9)); // note: initializer lists are broken
}

TEST_CASE("ranges non-default-constructible") {
    struct Foo {
        const int x;
        constexpr explicit Foo(int x) : x(x) {}
        bool operator<(const Foo& other) const noexcept {
            return x < other.x;
        }
    };
    static_assert(!std::is_default_constructible_v<Foo>);

    auto generate_foos = seq() | transform([](int x) { return Foo{x}; });

    auto vec = generate_foos | filter([](const Foo& foo) { return bool(foo.x % 2); })
               | transform([](const Foo& foo) { return Foo{foo.x + 1}; }) | take(10) | to_vector();
    static_cast<void>(vec);

    std::vector<Foo> vec2;
    generate_foos | take(10) | append(vec2);
}

TEST_CASE("ranges first after sort") {
    auto result = std::vector{{4, 3, 2, 1}} | sort() | first();
    CHECK(result == 1);
}

TEST_CASE("ranges non-default-constructible, non-copyable predicate") {
    // A compare predicate that is not default-constructible or copyable.
    struct Compare : std::less<void> {
        constexpr explicit Compare(int) {}
        constexpr Compare(Compare&&) {}
        constexpr Compare& operator=(Compare&&) { return *this; }
        constexpr Compare(const Compare&) = delete;
        constexpr Compare& operator=(const Compare&) = delete;
    };
    static_assert(!std::is_default_constructible_v<Compare>);

    auto in = seq() | take(10);
    auto vec = in | sort(Compare{0}) | to_vector();
    CHECK(vec == (in | to_vector()));

    CHECK((in | sort(Compare{0}) | max(Compare{0})) == 9);
    CHECK((in | sort(Compare{0}) | min(Compare{0})) == 0);

    // Compile-time check that iterators don't introduce default-constructibility as a requirement.
    // Note: Explicit call to as_input_range() is required here because range-based for loops do not
    //       perfectly-forward to `begin()/end()` (and it would usually be wrong if they did).
    for (auto x : as_input_range(in | sort(Compare{0}))) {
        static_cast<void>(x);
    }
}

TEST_CASE("ranges empty_range") {
    CHECK((empty_range() | count()) == 0);
    CHECK((empty_range() | to_vector()) == std::vector<void*>());
}

TEST_CASE("ranges chain") {
    // 0 arguments
    static_assert(std::is_same_v<decltype(chain()), decltype(empty_range())>);

    // 1 argument
    static_assert(std::is_same_v<decltype(chain(seq())), decltype(seq())>);

    // 2 arguments
    auto homogenous_actual = chain("hello"s, "world"s) | append(""s);
    auto homogenous_expected = "helloworld"s;
    CHECK(homogenous_actual == homogenous_expected);

    // 3 arguments
    auto heterogeneous_actual = chain(seq() | take(4), "test"s, seq()) | take(10) | to_vector();
    auto heterogeneous_expected = std::vector<int>{{0,1,2,3,'t','e','s','t',0,1}};
    CHECK(heterogeneous_actual == heterogeneous_expected);

    // Ensure ranges inbetween can be empty.
    homogenous_actual = chain(""s, "hello"s, "world"s) | append(""s);
    CHECK(homogenous_actual == homogenous_expected);

    homogenous_actual = chain("hello"s, ""s, "world"s) | append(""s);
    CHECK(homogenous_actual == homogenous_expected);

    homogenous_actual = chain("hello"s, "world"s, ""s) | append(""s);
    CHECK(homogenous_actual == homogenous_expected);
}

struct NoDefaultConstruction {
    NoDefaultConstruction() = delete;
    NoDefaultConstruction(int i_) : i{ i_ } {}

    int i;

    friend bool operator==(NoDefaultConstruction const& lhs, NoDefaultConstruction const& rhs) { return lhs.i == rhs.i; }
};

TEST_CASE("ranges chain no-default-constructible type") {
    std::vector<NoDefaultConstruction> vec1{
        NoDefaultConstruction(1),
        NoDefaultConstruction(2)
    };

    std::vector<NoDefaultConstruction> vec2{
        NoDefaultConstruction(3),
        NoDefaultConstruction(4)
    };

    std::vector<NoDefaultConstruction> vec_expected{
        NoDefaultConstruction(1),
        NoDefaultConstruction(2),
        NoDefaultConstruction(3),
        NoDefaultConstruction(4)
    };

    auto vec_actual = chain(vec1, vec2) | to_vector();
    CHECK(vec_actual == vec_expected);
}

TEST_CASE("ranges chain advance_by") {
    auto input = chain(seq() | take(3), seq(10) | take(3), seq(20) | take(3));
    auto result1 = input | to_vector();
    CHECK(result1 == std::vector{{0, 1, 2, 10, 11, 12, 20, 21, 22}});
    advance_by(input, 4);
    auto result2 = input | to_vector();
    CHECK(result2 == std::vector{{11, 12, 20, 21, 22}});
    advance_by(input, 3);
    auto result3 = input | to_vector();
    CHECK(result3 == std::vector{{21, 22}});
    advance_by(input, 3); // advance beyond end
    CHECK(input.at_end());
}

TEST_CASE("ranges cycle") {
    auto nothing = seq() | take(0) | cycle() | take(10) | to_vector();
    CHECK(nothing == std::vector(0, 0));

    auto zeroes = seq() | take(1) | cycle() | take(10) | to_vector();
    CHECK(zeroes == std::vector(10, 0));

    auto zero_one_two = seq() | take(3) | cycle() | take(10) | to_vector();
    CHECK(zero_one_two == std::vector{{0, 1, 2, 0, 1, 2, 0, 1, 2, 0}});
}

TEST_CASE("ranges cycle advance_by") {
    auto input = seq() | take(5) | cycle();
    advance_by(input, 5); // advancing to the end should wrap around
    CHECK(input.get() == 0);
    advance_by(input, 6); // overflow
    CHECK(input.get() == 1);
}

TEST_CASE("ranges padded") {
    auto actual = seq() | take(3) | padded(-1) | take(5) | to_vector();
    auto expected = std::vector{{0,1,2,-1,-1}};
    CHECK(actual == expected);
}

TEST_CASE("ranges padded advance_by") {
    auto actual = seq() | take(3) | padded(-1);
    CHECK(actual.get() == 0);
    advance_by(actual, 2);
    CHECK(actual.get() == 2);
    advance_by(actual, 1);
    CHECK(actual.get() == -1);
}

TEST_CASE("ranges zip_longest") {
    auto input1 = seq() | first_n(5);
    auto input2 = input1 | transform(&to_string<int>);
    auto input3 = seq(10) | first_n(7);
    auto zipped = zip_longest(input1, input2, input3) | to_vector();
    CHECK(zipped.size() == 7);
    auto expected = std::vector{
        std::make_tuple(RX_OPTIONAL(0), RX_OPTIONAL("0"s), RX_OPTIONAL(10)),
        std::make_tuple(RX_OPTIONAL(1), RX_OPTIONAL("1"s), RX_OPTIONAL(11)),
        std::make_tuple(RX_OPTIONAL(2), RX_OPTIONAL("2"s), RX_OPTIONAL(12)),
        std::make_tuple(RX_OPTIONAL(3), RX_OPTIONAL("3"s), RX_OPTIONAL(13)),
        std::make_tuple(RX_OPTIONAL(4), RX_OPTIONAL("4"s), RX_OPTIONAL(14)),
        std::make_tuple(RX_OPTIONAL<int>(), RX_OPTIONAL<std::string>(), RX_OPTIONAL(15)),
        std::make_tuple(RX_OPTIONAL<int>(), RX_OPTIONAL<std::string>(), RX_OPTIONAL(16)),
    };
    CHECK(zipped == expected);
}

TEST_CASE("ranges zip_longest advance_by") {
    auto input1 = seq() | first_n(5);
    auto input2 = input1 | transform(&to_string<int>);
    auto input3 = seq(10) | first_n(7);
    auto zipped = zip_longest(input1, input2, input3);
    advance_by(zipped, 4);
    auto expected1 = std::make_tuple(RX_OPTIONAL(4), RX_OPTIONAL("4"s), RX_OPTIONAL(14));
    CHECK(zipped.get() == expected1);
    advance_by(zipped, 2);
    auto expected2 = std::make_tuple(RX_OPTIONAL<int>(), RX_OPTIONAL<std::string>(), RX_OPTIONAL(16));
    CHECK(zipped.get() == expected2);
    size_t advanced = advance_by(zipped, 2);
    CHECK(zipped.at_end());
    CHECK(advanced == 1);
}

TEST_CASE("ranges tee") {
    auto container1 = std::vector(0, 0);
    auto container2 = std::vector(0, 0);
    seq() | tee(container1) | take(10) | append(container2);
    CHECK(container1 == container2);

    container1.clear();
    auto value = seq() | tee(container1) | take(10) | sum();
    CHECK(container1 == container2);
    CHECK(value == 9 * 10 / 2);
}

TEST_CASE("ranges ad-hoc lambdas") {
    auto f = [](auto&& range) {
        return range | filter([](auto x) { return x % 2 == 1; }) | take(5);
    };

    auto result = seq() | f | to_vector();
    CHECK(result == std::vector{{1, 3, 5, 7, 9}});
}

TEST_CASE("ranges flatten") {
    auto l0 = seq(11) | take(3);
    auto l1 = fill_n(3, l0);
    auto l2 = fill_n(3, l1);
    auto l3 = fill_n(3, l2);

    auto flatten0 = l3 | flatten<0>();
    auto flatten1 = l3 | flatten();
    auto flatten2 = l3 | flatten<2>();
    auto flatten3 = l3 | flatten<3>();

    CHECK((flatten0 | count()) == 3);
    CHECK((flatten1 | count()) == 3*3);
    CHECK((flatten2 | count()) == 3*3*3);
    CHECK((flatten3 | count()) == 3*3*3*3);

    CHECK((flatten3 | sum()) == 3*3*3 * (11+12+13));
}

TEST_CASE("ranges null_sink") {
    int a = 0;
    int b = 0;
    generate([&]{ return ++a; }) | take(5) | transform([&](auto v) { CHECK(v == ++b); return v; }) | append(null_sink());
    CHECK(a == 5);
    CHECK(b == 5);
}

namespace {

// NOTE: When modifying this, make sure to modify README.md as well!
struct convert_to_string {
    template <class Input>
    struct Range {
        using output_type = std::string;
        static constexpr bool is_finite = rx::is_finite_v<Input>;
        static constexpr bool is_idempotent = rx::is_idempotent_v<Input>;

        Input input;
        constexpr explicit Range(Input input) : input(std::move(input)) {}

        [[nodiscard]] output_type get() const noexcept {
            return std::to_string(input.get());
        }

        constexpr void next() noexcept {
            input.next();
        }

        [[nodiscard]] constexpr bool at_end() const noexcept {
            return input.at_end();
        }

        [[nodiscard]] constexpr size_t size_hint() const noexcept {
            return input.size_hint();
        }

        constexpr size_t advance_by(size_t n) const noexcept {
            using rx::advance_by; // Enable ADL.
            return advance_by(input, n);
        }
    };

    template <class Input>
    [[nodiscard]] constexpr auto operator()(Input&& input) const {
        using Inner = decltype(rx::as_input_range(std::forward<Input>(input)));
        return Range<Inner>(rx::as_input_range(std::forward<Input>(input)));
    }
};
std::vector<std::string> convert_ints_to_sorted_strings(std::vector<int> input) {
    return input | convert_to_string() | rx::sort() | rx::to_vector();
}
struct normalize {
    template <class Input>
    struct Range {
        using output_type = rx::get_output_type_of_t<Input>;
        Input input;
        constexpr explicit Range(Input input) : input(std::move(input)) {}
        template <class Out>
        constexpr void sink(Out& out) && noexcept {
            rx::sink(std::move(input), out);
            auto square = [](auto x) { return x * x; };
            auto length = std::sqrt(out | transform(square) | sum());
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
} // anonymous namespace

TEST_CASE("ranges doc examples test") {
    auto strings = convert_ints_to_sorted_strings(std::vector{{3, 1, 2, 3, 4}});
    CHECK(strings == std::vector{{"1"s, "2"s, "3"s, "3"s, "4"s}});

    // length is 9 — this happens to produce exact floating point results.
    auto normalized = normalize_vector(std::vector{{4.0, 8.0, 1.0}});
    CHECK(normalized == std::vector{{4.0 / 9, 8.0 / 9, 1.0 / 9}});

    auto avg = compute_average(std::vector{{1.0, 2.0, 3.0, 4.0, 5.0}});
    CHECK(avg == 3.0);
}

/*
TEST_CASE("ranges append to non-container [no compile]") {
    double not_a_container = 0;
    seq() | first_n(10) | append(not_a_container);
}

TEST_CASE("ranges infinite to vector [no compile]") {
    auto s = seq() | to_vector();
}
TEST_CASE("ranges sort infinite [no compile]") {
    auto s = seq() | sort() | to_vector();
}
*/
