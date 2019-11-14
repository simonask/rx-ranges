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

TEST_CASE("range operator|") {
    std::string s = 123 | [](int x) { return to_string(x); };
    CHECK(s == "123");
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
    auto expected = std::map{{
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
    auto expected = std::unordered_map{{std::make_pair(0.0, "0"s),
                                        std::make_pair(1.0, "1"s),
                                        std::make_pair(2.0, "2"s),
                                        std::make_pair(3.0, "3"s),
                                        std::make_pair(4.0, "4"s)}};
    CHECK(result == expected);
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
}

TEST_CASE("ranges infinity propagates") {
    auto s =
        seq() | skip_n(1) | filter([](auto) { return true; }) | transform([](auto) { return 0; });
    CHECK(!decltype(s)::is_finite);
}

TEST_CASE("ranges enumerate with indices") {
    auto input = std::vector{{"a"s, "b"s, "c"s}};
    size_t n = 0;
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
        ++n;
    }
    CHECK(n == input.size());

    auto a = zip(seq(), input) | to_vector();
    auto b = enumerate(input) | to_vector();
    CHECK(a == b);
}

TEST_CASE("ranges reverse") {
    auto input = std::vector{{2, 3, 6, 1, 7, 8, 3, 4}};
    auto result = input | sort() | reverse() | to_vector();
    auto expected = std::vector{{8, 7, 6, 4, 3, 3, 2, 1}};
    CHECK(result == expected);
}

TEST_CASE("ranges in_groups_of_exactly, constant size") {
    auto input = seq<float>() | take(1000) | to_vector();

    size_t num_groups = input | in_groups_of_exactly<4>() | count();
    CHECK(num_groups == 250);

    // In optimized builds, compilers should be able to auto-vectorize this.
    std::array<float, 4> sums = {0.f, 0.f, 0.f, 0.f};
    for (auto group : input | in_groups_of_exactly<4>()) {
        // This becomes a single _mm_add_ps on Clang, but not MSVC.
        std::get<0>(sums) += std::get<0>(group);
        std::get<1>(sums) += std::get<1>(group);
        std::get<2>(sums) += std::get<2>(group);
        std::get<3>(sums) += std::get<3>(group);
    }

    std::array<float, 4> expected_sums = {0.f, 0.f, 0.f, 0.f};
    for (auto [i, x]: enumerate(input)) {
        expected_sums[i % 4] += x;
    }

    CHECK(sums == expected_sums);
}

TEST_CASE("ranges in_groups_of_exactly, dynamic size") {
    auto input = seq<float>() | take(1000) | to_vector();

    size_t num_groups = input | in_groups_of_exactly(4) | count();
    CHECK(num_groups == 250);

    // In optimized builds, compilers should be able to auto-vectorize this.
    std::array<float, 4> sums = {0.f, 0.f, 0.f, 0.f};
    for (auto group : input | in_groups_of_exactly(4)) {
        std::get<0>(sums) += group[0];
        std::get<1>(sums) += group[1];
        std::get<2>(sums) += group[2];
        std::get<3>(sums) += group[3];
    }

    std::array<float, 4> expected_sums = {0.f, 0.f, 0.f, 0.f};
    for (auto [i, x]: enumerate(input)) {
        expected_sums[i % 4] += x;
    }

    CHECK(sums == expected_sums);
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

    // For some reason, MSVC does not correctly find begin()/end() via ADL in range-based for loops
    // for this particular case. It's a bit of a mystery, since this works for all the other range
    // types, and there doesn't seem to be any difference in how the in_groups_of() adapter works.
    for (auto it = begin(groups); it != end(groups); ++it) {
        auto& group = *it;
        CHECK(group.size() != 0);
        if (group.size() == 4) {
            std::get<0>(sums) += group[0];
            std::get<1>(sums) += group[1];
            std::get<2>(sums) += group[2];
            std::get<3>(sums) += group[3];
        } else {
            last = group.back();
        }
    }

    std::array<float, 4> expected_sums = {0.f, 0.f, 0.f, 0.f};
    float expected_last = 0.f;
    for (auto [i, x]: enumerate(input)) {
        if (i < 1000) {
            expected_sums[i % 4] += x;
        } else {
            expected_last = x;
        }
    }

    CHECK(sums == expected_sums);
    CHECK(last == expected_last);
}

TEST_CASE("ranges group_adjacent_by") {
    const auto input = seq() | take(10) | to_vector();

    auto pred = [](int x) {
        return x / 3;
    };

    auto groups = input | group_adjacent_by(pred);
    auto tmp = groups | to_vector();
    size_t num_groups = groups | count();
    CHECK(num_groups == 4);

    // For some reason, MSVC does not correctly find begin()/end() via ADL in range-based for loops
    // for this particular case. It's a bit of a mystery, since this works for all the other range
    // types, and there doesn't seem to be any difference in how the in_groups_of() adapter works.
    int previous = std::numeric_limits<int>::max();
    for (auto it = begin(groups); it != end(groups); ++it) {
        const auto& group = *it;
        for (auto x : group) {
            CHECK(pred(x) == pred(group[0]));
            CHECK(pred(x) != previous);
        }
        previous = pred(group[0]);
    }

    auto group_vectors = groups | to_vector(); // vector<vector<int>>;
    CHECK(group_vectors.size() == 4);
    CHECK(group_vectors[0] == std::vector{{0, 1, 2}});
    CHECK(group_vectors[1] == std::vector{{3, 4, 5}});
    CHECK(group_vectors[2] == std::vector{{6, 7, 8}});
    CHECK(group_vectors[3] == std::vector<int>(1, 9));
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
