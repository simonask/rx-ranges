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

    # Note: Cloning ad-hoc, because we don't want to add submodules (interfering with other
    # projects using us as submodule and `--recursive`), and `ExternalProject` does not work well at
    # all for importing targets from other projects.
    if (NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/doctest")
        message(STATUS "Cloning onqtam/doctest...")
        execute_process(
            COMMAND git clone https://github.com/onqtam/doctest.git
            WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        )
    endif()
    if (NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/benchmark")
        message(STATUS "Cloning google/benchmark...")
        execute_process(
            COMMAND git clone https://github.com/google/benchmark.git
            WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        )
    endif()

    set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "" FORCE)
    set(BENCHMARK_ENABLE_GTEST_TESTS OFF CACHE BOOL "" FORCE)
    set(BENCHMARK_ENABLE_EXCEPTIONS ON CACHE BOOL "" FORCE)
    set(BENCHMARK_ENABLE_INSTALL OFF CACHE BOOL "" FORCE)
    mark_as_advanced(BENCHMARK_ENABLE_TESTING)
    mark_as_advanced(BENCHMARK_ENABLE_GTEST_TESTS)
    mark_as_advanced(BENCHMARK_ENABLE_EXCEPTIONS)
    mark_as_advanced(BENCHMARK_ENABLE_INSTALL)
    set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)

    set(DOCTEST_WITH_TESTS OFF CACHE BOOL "" FORCE)
    set(DOCTEST_WITH_MAIN_IN_STATIC_LIB OFF CACHE BOOL "" FORCE)
    set(DOCTEST_NO_INSTALL OFF CACHE BOOL "" FORCE)
    mark_as_advanced(DOCTEST_WITH_TESTS)
    mark_as_advanced(DOCTEST_WITH_MAIN_IN_STATIC_LIB)
    mark_as_advanced(DOCTEST_NO_INSTALL)

    add_subdirectory("${CMAKE_CURRENT_BINARY_DIR}/doctest")
    add_subdirectory("${CMAKE_CURRENT_BINARY_DIR}/benchmark")

    add_executable(rx-ranges-test test/test_ranges.cpp)
    target_link_libraries(rx-ranges-test PRIVATE rx::ranges doctest)
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

    add_executable(rx-benchmark test/benchmark.cpp)
    if (WIN32)
        target_compile_definitions(rx-benchmark PRIVATE "_CRT_SECURE_NO_WARNINGS")
    endif()
    target_link_libraries(rx-benchmark PRIVATE rx::ranges benchmark)
endif()
