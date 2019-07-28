cmake_minimum_required(VERSION 2.8)
project(neuralnet)

set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --std=c++14")

set(NEURALNET_SOURCES
  src/main.cpp)

set(TRAININGDATAGEN_SOURCES
  src/makeTrainingData.cpp)

add_executable(neuralnet ${NEURALNET_SOURCES})
add_executable(trainingdatagen ${TRAININGDATAGEN_SOURCES})
