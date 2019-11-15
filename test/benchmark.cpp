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

BENCHMARK_MAIN();