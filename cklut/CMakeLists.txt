cmake_minimum_required(VERSION 3.16)
project(cklut CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Header-only interface library.
add_library(cklut INTERFACE)
target_include_directories(cklut INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

# Dependency-free examples.
add_executable(build_csv examples/build_csv.cpp)
target_link_libraries(build_csv PRIVATE cklut)

add_executable(query examples/query.cpp)
target_link_libraries(query PRIVATE cklut)

# Optional Parquet importer (requires Apache Arrow + Parquet).
option(CKLUT_PARQUET "Build the Parquet importer" OFF)
if(CKLUT_PARQUET)
  find_package(Arrow CONFIG REQUIRED)
  find_package(Parquet CONFIG REQUIRED)
  add_executable(build_parquet examples/build_parquet.cpp)
  target_link_libraries(build_parquet PRIVATE cklut Arrow::arrow_shared Parquet::parquet_shared)
endif()

if(MSVC)
  add_compile_options(/O2 /DNDEBUG)
else()
  add_compile_options(-O3 -march=native -DNDEBUG)
endif()
