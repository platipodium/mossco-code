cmake_minimum_required(VERSION 3.10)

# Project Name
project(MyProject VERSION 1.0
 LANGUAGES CXX)

# Standard C++
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add subdirectories
add_subdirectory(src)
add_subdirectory(examples)
add_subdirectory(external)

# Enable testing with CTest
enable_testing()
