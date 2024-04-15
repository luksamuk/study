cmake_minimum_required(VERSION 3.5)

project(helloworld)

add_executable(${PROJECT_NAME}
  main.c
)

target_link_libraries(${PROJECT_NAME} PRIVATE
  pspdebug
  pspdisplay
  pspgu
  pspge
  pspctrl
)

create_pbp_file(
  TARGET ${PROJECT_NAME}
  ICON_PATH NULL
  BACKGROUND_PATH NULL
  PREVIEW_PATH NULL
  TITLE "Hello World"
  VERSION 01.00
)

