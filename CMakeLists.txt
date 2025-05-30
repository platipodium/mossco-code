# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

cmake_minimum_required(VERSION 3.10)

# Project Name
project(mossco VERSION 1.2.0
    DESCRIPTION "Modular System for Shelves and Coasts"
    LANGUAGES Fortran C)

# Homepage URL
set(PROJECT_HOMEPAGE_URL "http://www.mossco.de")

# Add subdirectories
add_subdirectory(src)
add_subdirectory(examples)
add_subdirectory(external)

# Enable testing with CTest
enable_testing()
