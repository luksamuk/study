cmake_minimum_required(VERSION 3.15)

project(clox
    VERSION
        1.0
    DESCRIPTION
        "CLox compiler implementation"
    LANGUAGES
        C)


list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
find_package(Criterion REQUIRED)

file(GLOB SRC_FILES ${CMAKE_CURRENT_LIST_DIR}/src/clox/*.c)
file(GLOB TEST_FILES ${CMAKE_CURRENT_LIST_DIR}/test/*.c)

add_library(cloxlib ${SRC_FILES})
target_include_directories(cloxlib PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/src/include/>
	$<INSTALL_INTERFACE:include/>)


add_executable(clox ${CMAKE_CURRENT_LIST_DIR}/src/main.c)
target_link_libraries(clox cloxlib)

add_executable(clox_test ${TEST_FILES})
target_link_libraries(clox_test cloxlib)
target_link_libraries(clox_test criterion)

add_test(NAME clox_test
	COMMAND clox_test)

