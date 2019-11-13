cmake_minimum_required(VERSION 3.6)
cmake_policy(SET CMP0076 NEW)
project(rx-ranges VERSION 1.0.1)

option(RX_TEST ON)

add_library(rx-ranges INTERFACE)
add_library(rx::ranges ALIAS rx-ranges)
target_compile_features(rx-ranges INTERFACE
    cxx_std_17
)
target_sources(rx-ranges
    INTERFACE
        include/rx/ranges.hpp
)
target_include_directories(rx-ranges
    INTERFACE include
)

if (RX_TEST)
    find_package(doctest REQUIRED)
    add_executable(rx-ranges-test test/test_ranges.cpp)
    target_link_libraries(rx-ranges-test PRIVATE rx::ranges doctest::doctest)
    target_compile_definitions(rx-ranges-test PRIVATE "DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN")
    if (MSVC)
        target_compile_options(rx-ranges-test PRIVATE "/D_CRT_SECURE_NO_WARNINGS /Wall /WX /permissive-")
    else()
        target_compile_options(rx-ranges-test PRIVATE "-Wall -Werror -Wpedantic")
    endif()

    find_package(Boost QUIET COMPONENTS date_time)
    if (Boost_FOUND)
        add_executable(rx-calendar test/calendar.cpp)
        target_link_libraries(rx-calendar PRIVATE rx::ranges Boost::date_time)
    else()
        message(WARNING "Boost not found, not building calendar example")
    endif()
endif()