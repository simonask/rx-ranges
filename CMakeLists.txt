cmake_minimum_required(VERSION 3.6)
if ("${CMAKE_VERSION}" VERSION_GREATER_EQUAL "3.15.0")
    cmake_policy(SET CMP0092 NEW) # Disable default warning flags so we can override them in MSVC.
endif()

project(rx-ranges VERSION 2.0.0)

option(RX_TEST ON)

add_library(rx-ranges INTERFACE)
add_library(rx::ranges ALIAS rx-ranges)
target_compile_features(rx-ranges INTERFACE
    cxx_std_17
)
target_sources(rx-ranges
    INTERFACE
        "${CMAKE_CURRENT_SOURCE_DIR}/include/rx/ranges.hpp"
)
target_include_directories(rx-ranges
    INTERFACE include
)

if (RX_TEST)
    # RelWithDebInfo falls back to Release, then MinSizeRel
    set(CMAKE_MAP_IMPORTED_CONFIG_RELWITHDEBINFO RelWithDebInfo Release MinSizeRel NoConfig "")
    # MinSizeRel falls back to Release, then RelWithDebInfo
    set(CMAKE_MAP_IMPORTED_CONFIG_MINSIZEREL MinSizeRel Release RelWithDebInfo NoConfig "")
    # Release falls back to RelWithDebInfo, then MinSizeRel
    set(CMAKE_MAP_IMPORTED_CONFIG_RELEASE Release RelWithDebInfo MinSizeRel NoConfig "")

    if (APPLE)
        add_subdirectory(doctest)
    else()
        find_package(doctest REQUIRED)
    endif()

    add_executable(rx-ranges-test test/test_ranges.cpp)
    target_link_libraries(rx-ranges-test PRIVATE rx::ranges doctest::doctest)
    target_compile_definitions(rx-ranges-test PRIVATE "DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN")
    if (WIN32)
        target_compile_definitions(rx-ranges-test PRIVATE "_CRT_SECURE_NO_WARNINGS")
        target_compile_options(rx-ranges-test PRIVATE /W4 /WX /permissive-)
    else()
        target_compile_options(rx-ranges-test PRIVATE -Wall -Werror -Wpedantic)
    endif()

    set(Boost_NO_BOOST_CMAKE ON)
    find_package(Boost QUIET COMPONENTS date_time)
    if (Boost_FOUND)
        add_executable(rx-calendar test/calendar.cpp)
        if (WIN32)
            target_compile_definitions(rx-calendar PRIVATE "_CRT_SECURE_NO_WARNINGS")
        endif()
        target_link_libraries(rx-calendar PRIVATE rx::ranges Boost::date_time)
    else()
        message(WARNING "Boost not found, not building calendar example")
    endif()
    find_package(benchmark QUIET)
    if (benchmark_FOUND)
        if (UNIX AND NOT APPLE)
            set(benchmark_LIBS "benchmark")
        else()
            set(benchmark_LIBS "benchmark::benchmark")
        endif()
        add_executable(rx-benchmark test/benchmark.cpp)
        if (WIN32)
            target_compile_definitions(rx-benchmark PRIVATE "_CRT_SECURE_NO_WARNINGS")
        endif()
        target_link_libraries(rx-benchmark PRIVATE rx::ranges ${benchmark_LIBS})
    else()
        message(WARNING "Google benchmark not found, not building benchmarks")
    endif()
endif()
