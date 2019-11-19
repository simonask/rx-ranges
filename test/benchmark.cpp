#include <rx/ranges.hpp>

#include <benchmark/benchmark.h>

#include <numeric>

using namespace rx;

// Note: The std algorithms almost always fall back to std::for_each, because the standard
// algorithms like `std::transform()` etc. always require an output container, which would be
// unfairly expensive due to heap allocations.

static void bench_transform_rx(benchmark::State& state) {
    std::vector<int> input;
    input.resize(1'000'000);
    std::iota(begin(input), end(input), 0);
    for (auto _ : state) {
        auto f = [](auto begin, auto end) constexpr {
            return iterator_range(begin, end) | transform([](int x) { return x * 2; }) | sum();
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_transform_rx);

static void bench_transform_std(benchmark::State& state) {
    std::vector<int> input;
    input.resize(1'000'000);
    std::iota(begin(input), end(input), 0);
    for (auto _ : state) {
        auto f = [](auto begin, auto end) constexpr {
            int sum = 0;
            std::for_each(begin, end, [&](int x) { sum += x * 2; });
            return sum;
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_transform_std);

static void bench_transform_plain(benchmark::State& state) {
    std::vector<int> input;
    input.resize(1'000'000);
    std::iota(begin(input), end(input), 0);
    for (auto _ : state) {
        auto f = [](auto begin, auto end) constexpr {
            int sum = 0;
            for (auto it = begin; it != end; ++it) {
                sum += *it * 2;
            }
            return sum;
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_transform_plain);

static void bench_filter_sum_rx(benchmark::State& state) {
    std::vector<int> input;
    input.resize(1'000'000);
    std::iota(begin(input), end(input), 0);
    for (auto _ : state) {
        // For some reason, compilers are not able to merge the condition in filter() with the loop
        // condition in sum(). For that reason, this benchmark is significantly slower than the
        // plain alternatives.
        auto f = [](auto begin, auto end) constexpr {
            return iterator_range(begin, end) | filter([](int x) constexpr { return bool(x % 2); })
                   | sum();
        };
        //// This version is just as fast as the others.
        // auto f = [](auto begin, auto end) constexpr {
        //     int sum = 0;
        //     iterator_range(begin, end) | for_each([&](int x) constexpr {
        //         if (x % 2)
        //             sum += x;
        //     });
        //     return sum;
        // };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_filter_sum_rx);

static void bench_filter_sum_std(benchmark::State& state) {
    std::vector<int> input;
    input.resize(1'000'000);
    std::iota(begin(input), end(input), 0);
    for (auto _ : state) {
        auto f = [](auto begin, auto end) {
            int sum = 0;
            std::for_each(
                begin, end, [&](int x) constexpr {
                    if (x % 2)
                        sum += x;
                });
            return sum;
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_filter_sum_std);

static void bench_filter_sum_plain(benchmark::State& state) {
    std::vector<int> input;
    input.resize(1'000'000);
    std::iota(begin(input), end(input), 0);
    for (auto _ : state) {
        auto f = [](auto begin, auto end) constexpr {
            int sum = 0;
            for (auto it = begin; it != end; ++it) {
                if (*it % 2) {
                    sum += *it;
                }
            }
            return sum;
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_filter_sum_plain);

static void bench_in_groups_of_exactly_rx(benchmark::State& state) {
    std::vector<int> input = seq() | take(1'000'000) | to_vector();
    for (auto _ : state) {
        // Sum the last element in each group
        auto f = [](auto begin, auto end) {
            return iterator_range(begin, end) | in_groups_of_exactly(16) | transform([](auto&& group) {
                advance_by(group, 15);
                return group.get();
            }) | sum();
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_in_groups_of_exactly_rx);

static void bench_in_groups_of_rx(benchmark::State& state) {
    std::vector<int> input = seq() | take(1'000'000) | to_vector();
    for (auto _ : state) {
        // sum highest element of each group
        auto f = [](auto begin, auto end) constexpr {
            return iterator_range(begin, end) | in_groups_of(16) | transform([](auto&& group) {
                auto copy = group;
                advance_by(copy, 15);
                if (RX_LIKELY(!copy.at_end())) {
                    return copy.get();
                } else {
                    auto tmp = group.get();
                    while (!group.at_end()) {
                        group.next();
                        tmp = group.get();
                    }
                    return tmp;
                }
                return group.get();
            }) | sum();
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_in_groups_of_rx);

static void bench_in_groups_of_std(benchmark::State& state) {
    std::vector<int> input = seq() | take(1'000'000) | to_vector();
    for (auto _ : state) {
        auto f = [](auto begin, auto end) constexpr {
            int sum = 0;
            for (auto it = begin; it != end;) {
                if (end - it >= 16) {
                    sum += *(it + 15);
                    it += 16;
                } else {
                    sum += *(end - 1);
                    break;
                }
            }
            return sum;
        };
        benchmark::DoNotOptimize(f(begin(input), end(input)));
    }
}
BENCHMARK(bench_in_groups_of_std);

static void bench_cycle_rx(benchmark::State& state) {
    auto input = seq() | take(10);
    for (auto _ : state) {
        auto f = [=]() {
            return input | cycle() | take(1'000'000) | sum();
        };
        benchmark::DoNotOptimize(f());
    }
}
BENCHMARK(bench_cycle_rx);

static void bench_cycle_plain(benchmark::State& state) {
    for (auto _ : state) {
        auto f = []() {
            int sum = 0;
            for (size_t i = 0; i < 1'000'000; ++i) {
                sum += int(i % 10);
            }
            return sum;
        };
        benchmark::DoNotOptimize(f());
    }
}
BENCHMARK(bench_cycle_plain);

static void bench_padded_rx(benchmark::State& state) {
    std::vector<char> result;
    result.reserve(1'000'000);
    for (auto _ : state) {
        auto f = [&]() {
            result.clear();
            fill('a') | take(500'000) | padded('b') | take(1'000'000) | append(result);
            return 0;
        };
        benchmark::DoNotOptimize(f());
    }
}
BENCHMARK(bench_padded_rx);

static void bench_padded_plain(benchmark::State& state) {
    std::vector<char> result;
    result.reserve(1'000'000);
    for (auto _ : state) {
        auto f = [&]() {
            result.clear();
            for (size_t i = 0; i < 1'000'000; ++i) {
                char c = i < 500'000 ? 'a' : 'b';
                result.push_back(c);
            }
            return 0;
        };
        benchmark::DoNotOptimize(f());
    }
}
BENCHMARK(bench_padded_plain);

static void bench_chain_rx(benchmark::State& state) {
    std::vector<int> input1 = seq() | take(200'000) | to_vector();
    std::vector<int> input2 = seq(200'000) | take(200'000) | to_vector();
    std::vector<int> input3 = seq(400'000) | take(200'000) | to_vector();
    std::vector<int> input4 = seq(600'000) | take(200'000) | to_vector();
    for (auto _ : state) {
        auto f = [&]() {
            return chain(input1, input2, input3, input4) | sum();
        };
        benchmark::DoNotOptimize(f());
    }
}
BENCHMARK(bench_chain_rx);

static void bench_chain_plain(benchmark::State& state) {
    std::vector<int> input1 = seq() | take(200'000) | to_vector();
    std::vector<int> input2 = seq(200'000) | take(200'000) | to_vector();
    std::vector<int> input3 = seq(400'000) | take(200'000) | to_vector();
    std::vector<int> input4 = seq(600'000) | take(200'000) | to_vector();
    for (auto _ : state) {
        auto f = [&]() {
            int sum = 0;
            for (auto x : input1) {
                sum += x;
            }
            for (auto x : input2) {
                sum += x;
            }
            for (auto x : input3) {
                sum += x;
            }
            for (auto x : input4) {
                sum += x;
            }
            return sum;
        };
        benchmark::DoNotOptimize(f());
    }
}
BENCHMARK(bench_chain_plain);

BENCHMARK_MAIN();