set(LISTOFTOOLS)

file(GLOB APP_SOURCES */main.f90)

foreach(testsourcefile ${APP_SOURCES})

  get_filename_component(dir ${testsourcefile} DIRECTORY)
  get_filename_component(program ${dir} NAME)
  set(program_name  ${program})

  # include in project
  add_executable(${program_name} ${testsourcefile})
  target_link_libraries(${program_name} globals)

  list(APPEND LISTOFTOOLS ${program_name})

endforeach(testsourcefile ${APP_SOURCES})

install(TARGETS ${LISTOFTOOLS}
   RUNTIME DESTINATION bin COMPONENT tools
)

file(GLOB BASHSCRIPTS *.sh)

install(PROGRAMS ${BASHSCRIPTS}
   DESTINATION bin
   COMPONENT tools
)
