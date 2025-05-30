cmake_minimum_required(VERSION 3.10)

# Project Name
project(mossco VERSION 1.2.0
    DESCRIPTION "Modular System for Shelves and Coasts"
    LANGUAGES Fortran C CXX)

# Homepage URL
set(PROJECT_HOMEPAGE_URL "http://www.mossco.de")

# Standard C++
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add subdirectories
add_subdirectory(src)
add_subdirectory(examples)
add_subdirectory(external)

# Enable testing with CTest
enable_testing()
